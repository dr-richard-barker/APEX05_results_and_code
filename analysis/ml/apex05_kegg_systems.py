#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | KEGG systems-biology: identify pathways & group flight-DEG loci
# =============================================================================
#
#  Groups the spaceflight DESeq2 DEGs (flight-referenced) into KEGG pathways and
#  tests pathway over-representation per genotype x tissue. KEGG pathway<->gene
#  membership is fetched once from the KEGG REST API (ath organism) and cached.
#  Companion to the ggKEGG pathway maps (analysis/R/apex05_ggkegg_pathways.R),
#  which draw selected pathways with the same DE overlay.
#
#  Plant Reactome (Gramene) was also tested but has sparse, rice-projected
#  Arabidopsis coverage (best FDR ~0.98 on these DEGs), so KEGG is the primary
#  systems view; the Plant Reactome pathway list is saved for transparency.
#
#  OUTPUTS  results/tables/apex05_kegg_pathway_enrichment.csv     (per genotype x tissue)
#           results/tables/apex05_kegg_pathway_loci_grouping.csv  (DEG loci per pathway)
#           results/tables/apex05_plantreactome_pathways.csv
#           results/ml/figK1_kegg_pathway_enrichment.png
#  RUN  python analysis/ml/apex05_kegg_systems.py       (needs internet: KEGG + Gramene)
# =============================================================================

from __future__ import annotations
import re, json, io
from pathlib import Path
import numpy as np
import pandas as pd
import requests
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from scipy.stats import hypergeom

import apex05_data as A
TBL, OUT = A.REPO / "results/tables", A.REPO / "results/ml"
CACHE = A.REPO / "data/genesets"
DES = TBL / "deseq2"
PADJ, LFC = 0.05, 1.0
GENOS = A.PRIMARY_GENOTYPES


def _locus(x): return re.sub(r"\.\d+$", "", str(x).strip())


def kegg_pathways() -> tuple[dict, dict]:
    """KEGG ath pathway -> set(AGI loci), and pathway -> name. Cached locally."""
    linkf, namef = CACHE / "kegg_ath_pathway_genes.tsv", CACHE / "kegg_ath_pathway_names.tsv"
    if not linkf.exists():
        linkf.write_text(requests.get("https://rest.kegg.jp/link/ath/pathway", timeout=60).text)
    if not namef.exists():
        namef.write_text(requests.get("https://rest.kegg.jp/list/pathway/ath", timeout=60).text)
    link = pd.read_csv(linkf, sep="\t", header=None, names=["pathway", "gene"])
    link["pathway"] = link["pathway"].str.replace("path:", "", regex=False)
    link["locus"] = link["gene"].str.replace("ath:", "", regex=False).map(_locus)
    sets = {p: set(g.locus) for p, g in link.groupby("pathway")}
    names = dict(pd.read_csv(namef, sep="\t", header=None, names=["pathway", "name"]).values)
    names = {k.replace("path:", ""): v.split(" - ")[0] for k, v in names.items()}
    return sets, names


def degs(geno, tissue):
    slug = geno.replace("-", "").lower()
    d = pd.read_csv(DES / f"apex05_deseq2_{slug}_{tissue}.csv")
    d["locus"] = d["gene"].map(_locus)
    tested = set(d["locus"])
    sig = d[(pd.to_numeric(d.padj, errors="coerce") < PADJ) &
            (pd.to_numeric(d.log2FoldChange, errors="coerce").abs() > LFC)]
    lfc = d.groupby("locus")["log2FoldChange"].mean()
    return tested, dict(zip(sig["locus"], sig["log2FoldChange"])), lfc


def plant_reactome(deg_loci):
    try:
        url = ("https://plantreactome.gramene.org/AnalysisService/identifiers/projection"
               "?pageSize=40&page=1&sortBy=ENTITIES_PVALUE&order=ASC")
        r = requests.post(url, data="\n".join(sorted(deg_loci)),
                          headers={"Content-Type": "text/plain"}, timeout=90)
        pw = r.json().get("pathways", [])
        return pd.DataFrame([{"stId": p["stId"], "name": p["name"],
                              "found": p["entities"]["found"], "total": p["entities"]["total"],
                              "pValue": p["entities"]["pValue"], "fdr": p["entities"]["fdr"]}
                             for p in pw])
    except Exception as e:
        print("  Plant Reactome unavailable:", e)
        return pd.DataFrame()


def main():
    sets, names = kegg_pathways()
    kegg_universe = set().union(*sets.values())
    print(f"KEGG ath: {len(sets)} pathways, {len(kegg_universe)} annotated genes")

    enr_rows, grp_rows, reac_frames = [], [], []
    for geno in GENOS:
        for tissue in A.TISSUES:
            tested, sig, _ = degs(geno, tissue)
            bg = tested & kegg_universe
            deg = set(sig) & kegg_universe
            for p, genes in sets.items():
                mk = genes & bg
                hit = deg & mk
                if len(mk) < 5 or not hit:
                    continue
                pval = float(hypergeom.sf(len(hit) - 1, len(bg), len(deg), len(mk)))
                enr_rows.append({"genotype": geno, "tissue": tissue, "pathway": p,
                                 "name": names.get(p, p), "n_pathway": len(mk),
                                 "n_deg": len(hit), "p_hyper": pval})
                for locus in sorted(hit):
                    grp_rows.append({"genotype": geno, "tissue": tissue, "pathway": p,
                                     "name": names.get(p, p), "gene": locus,
                                     "log2FC": round(float(sig[locus]), 3)})
            if geno in GENOS and tissue == "root":
                rc = plant_reactome(set(sig))
                if not rc.empty:
                    reac_frames.append(rc.assign(genotype=geno, tissue=tissue))

    enr = pd.DataFrame(enr_rows).sort_values(["genotype", "tissue", "p_hyper"])
    enr.to_csv(TBL / "apex05_kegg_pathway_enrichment.csv", index=False)
    pd.DataFrame(grp_rows).to_csv(TBL / "apex05_kegg_pathway_loci_grouping.csv", index=False)
    if reac_frames:
        pd.concat(reac_frames).to_csv(TBL / "apex05_plantreactome_pathways.csv", index=False)

    # figure: top KEGG pathways (root) per genotype
    fig, axes = plt.subplots(1, len(GENOS), figsize=(5 * len(GENOS), 5), squeeze=False)
    for ax, geno in zip(axes[0], GENOS):
        sub = enr[(enr.genotype == geno) & (enr.tissue == "root")].nsmallest(10, "p_hyper").iloc[::-1]
        if sub.empty:
            ax.set_title(f"{geno} root: n.s."); ax.axis("off"); continue
        ax.barh(range(len(sub)), -np.log10(sub.p_hyper), color="#D55E00")
        ax.set_yticks(range(len(sub)), [f"{n[:34]} ({d})" for n, d in zip(sub.name, sub.n_deg)], fontsize=7)
        ax.set_title(f"{geno} root", fontsize=10)
        ax.set_xlabel(r"$-\log_{10}p$ (hypergeometric)", fontsize=8)
    fig.suptitle("KEGG pathway enrichment of the spaceflight DEGs (root)", y=1.0)
    fig.tight_layout(rect=(0, 0, 1, 0.98))
    fig.savefig(OUT / "figK1_kegg_pathway_enrichment.png", dpi=300, bbox_inches="tight")
    plt.close(fig)

    print("\nTop KEGG pathways (root):")
    for geno in GENOS:
        top = enr[(enr.genotype == geno) & (enr.tissue == "root")].nsmallest(4, "p_hyper")
        for _, r in top.iterrows():
            print(f"  {geno:7} {r['name'][:40]:40} {r['n_deg']}/{r['n_pathway']} p={r['p_hyper']:.1e}")
    print("\nwrote KEGG enrichment + loci grouping + Plant Reactome tables + figK1")


if __name__ == "__main__":
    main()
