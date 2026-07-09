#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Functional enrichment of the primary-genotype flight DEGs
# =============================================================================
#
#  Over-representation analysis (GO, KEGG, Reactome, WikiPathways) of the
#  spaceflight (FL vs GC) differentially expressed genes for each of the three
#  PRIMARY genotypes — Col-0, cax2-2, rbohD — in root and shoot, so every
#  genotype in the manuscript carries matched functional annotation. (cax2-3 is
#  excluded from the primary analysis; its concordant *CAX2* core is enriched
#  separately in apex05_cax2_core_enrichment.py.)
#
#  METHOD  (identical to the core-enrichment script, for comparability)
#  ------
#  * g:Profiler g:GOSt, organism = Arabidopsis thaliana, g:SCS-corrected p<0.05.
#  * Background = one CUSTOM, tissue-specific universe of genes actually tested
#    for differential expression (non-missing padj in the full FL-vs-GC contrast
#    tables). The expressed transcriptome is essentially genotype-independent, so
#    a single per-tissue background across genotypes is the correct, comparable
#    choice.
#  * Gene key = AGI Locus ID (shared by every DEG workbook).
#  * Queries: per genotype x tissue, the union DEG set and the up/down splits.
#
#  REQUIRES internet (g:Profiler API) + gprofiler-official + openpyxl.
#  INPUTS   results/tables/apex5_{col,cax22,rbohd}_{root,shoot}_*-regulated-*.xlsx
#           archive/excluded_cax2-3/apex05_cax23_{root,shoot}_fl-vs-gc_full.csv (bg)
#  OUTPUTS  results/tables/apex05_primary_enrichment_{genotype}_{tissue}.csv
#           results/tables/apex05_primary_enrichment_all.csv
#           results/ml/figD1_primary_genotype_enrichment.png
#           results/ml/primary_enrichment_summary.json
#  RUN      python analysis/ml/apex05_primary_genotype_enrichment.py
# =============================================================================

from __future__ import annotations
import json, re
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from gprofiler import GProfiler

REPO = Path(__file__).resolve().parents[2]
TBL = REPO / "results" / "tables"
OUT = REPO / "results" / "ml"
OUT.mkdir(parents=True, exist_ok=True)
SOURCES = ["GO:BP", "GO:MF", "GO:CC", "KEGG", "REAC", "WP"]
SRC_COLORS = {"GO:BP": "#0072B2", "GO:MF": "#56B4E9", "GO:CC": "#009E73",
              "KEGG": "#D55E00", "REAC": "#CC79A7", "WP": "#E69F00"}
gp = GProfiler(return_dataframe=True)

# DEG workbooks per genotype x tissue x direction (filenames match manuscript counts).
DEG = {
    "Col-0": {"root":  {"up": "apex5_col_root_edger_up-regulated-531.xlsx",
                        "down": "apex5_col_root_edger_down-regulated-268.xlsx"},
              "shoot": {"up": "apex5_col_shoot_edger_up-regulated-403.xlsx",
                        "down": "apex5_col_shoot_edger_down-regulated-201.xlsx"}},
    "cax2-2": {"root":  {"up": "apex5_cax22_root_up-regulated-100.xlsx",
                         "down": "apex5_cax22_root_down-regulated-223.xlsx"},
               "shoot": {"up": "apex5_cax22_shoot_up-regulated-45.xlsx",
                         "down": "apex5_cax22_shoot_down-regulated-185.xlsx"}},
    "rbohD": {"root":  {"up": "apex5_rbohd_root_up-regulated-393.xlsx",
                        "down": "apex5_rbohd_root_down-regulated-233.xlsx"},
              "shoot": {"up": "apex5_rbohd_shoot_up-regulated-170.xlsx",
                        "down": "apex5_rbohd_shoot_down-regulated-286.xlsx"}},
}
GENOTYPES, TISSUES = list(DEG), ["root", "shoot"]


def _locus(x) -> str:
    return re.sub(r"\.\d+$", "", str(x).strip())


def _read_workbook(fname: str) -> pd.DataFrame:
    """Return DEG loci with a TRUE flight direction from labelled FL/GC columns.

    IMPORTANT: the source workbook file names ('up-regulated' / 'down-regulated')
    are GC-referenced — 'up-regulated' genes are higher in Ground Control, i.e.
    *repressed* by flight (verified: 100% FL<GC), and vice-versa. We therefore
    ignore the file-name direction and classify each gene by its own
    log2(FL/GC): flight_dir = 'up' (induced) if FL>GC else 'down' (repressed).
    """
    d = pd.read_excel(TBL / fname)
    d.columns = [c.strip() for c in d.columns]
    lcol = "Locus ID" if "Locus ID" in d.columns else \
        [c for c in d.columns if "Locus" in c or c == "ID"][0]
    fl = [c for c in d.columns if c.endswith("_FL")][0]
    gc = [c for c in d.columns if c.endswith("_GC")][0]
    out = pd.DataFrame({"locus": d[lcol].map(_locus),
                        "flight_log2FC": np.log2((d[fl] + 1.0) / (d[gc] + 1.0))})
    return out[out["locus"].str.startswith("AT", na=False)]


def load_degs(geno: str, tissue: str) -> pd.DataFrame:
    """Combine both workbooks for a genotype x tissue; one row per locus."""
    both = pd.concat([_read_workbook(DEG[geno][tissue]["up"]),
                      _read_workbook(DEG[geno][tissue]["down"])], ignore_index=True)
    both = (both.sort_values("flight_log2FC", key=lambda s: s.abs(), ascending=False)
                .drop_duplicates("locus"))
    both["flight_dir"] = np.where(both["flight_log2FC"] > 0, "up", "down")
    return both


def tested_universe(tissue: str) -> list[str]:
    d = pd.read_csv(REPO / f"archive/excluded_cax2-3/apex05_cax23_{tissue}_fl-vs-gc_full.csv")
    d.columns = [c.strip().lstrip("﻿") for c in d.columns]
    d["padj"] = pd.to_numeric(d["padj"], errors="coerce")
    return sorted({_locus(v) for v in d.loc[d["padj"].notna(), "ID"]})


def enrich(query, background, label):
    if len(query) < 3:
        print(f"  [{label}] {len(query)} genes — skipped"); return pd.DataFrame()
    res = gp.profile(organism="athaliana", query=query, sources=SOURCES,
                     user_threshold=0.05, significance_threshold_method="g_SCS",
                     background=background, domain_scope="custom", no_evidences=True)
    if res.empty:
        print(f"  [{label}] no significant terms"); return res
    res = res.assign(query_label=label).sort_values("p_value")
    print(f"  [{label}] {len(res):>3} terms  (top: {res.iloc[0]['source']} "
          f"{res.iloc[0]['name'][:40]}, p={res.iloc[0]['p_value']:.1e})")
    return res


def main():
    print("=" * 70)
    print("APEX-05 | Primary-genotype flight-DEG enrichment (Col-0, cax2-2, rbohD)")
    print("=" * 70)
    keep = ["query_label", "genotype", "tissue", "subset", "source", "native",
            "name", "p_value", "term_size", "query_size", "intersection_size"]
    bg = {t: tested_universe(t) for t in TISSUES}
    for t in TISSUES:
        print(f"  background[{t}] = {len(bg[t])} tested loci")

    all_res = []
    for geno in GENOTYPES:
        for tissue in TISSUES:
            degs = load_degs(geno, tissue)
            allg = sorted(degs["locus"])
            fup = sorted(degs.loc[degs.flight_dir == "up", "locus"])     # induced by flight
            fdn = sorted(degs.loc[degs.flight_dir == "down", "locus"])   # repressed by flight
            print(f"\n[{geno} {tissue}] flight-induced={len(fup)} "
                  f"flight-repressed={len(fdn)} union={len(allg)}")
            for subset, genes in (("all", allg), ("flight_up", fup), ("flight_down", fdn)):
                r = enrich(genes, bg[tissue], f"{geno}:{tissue}:{subset}")
                if not r.empty:
                    r = r.assign(genotype=geno, tissue=tissue, subset=subset)
                    all_res.append(r)

    if not all_res:
        print("\nNo significant enrichment."); return
    full = pd.concat(all_res, ignore_index=True)
    full[keep].to_csv(TBL / "apex05_primary_enrichment_all.csv", index=False)
    for geno in GENOTYPES:
        for tissue in TISSUES:
            sub = full[(full.genotype == geno) & (full.tissue == tissue)]
            if not sub.empty:
                slug = geno.replace("-", "").lower()
                sub[keep].to_csv(TBL / f"apex05_primary_enrichment_{slug}_{tissue}.csv", index=False)
    print(f"\n  wrote per-genotype tables + apex05_primary_enrichment_all.csv")

    # ---- Figure: 2 tissues (rows) x 3 genotypes (cols), top terms of 'all' ----
    fig, axes = plt.subplots(len(TISSUES), len(GENOTYPES), figsize=(15, 8.5))
    for i, tissue in enumerate(TISSUES):
        for j, geno in enumerate(GENOTYPES):
            ax = axes[i, j]
            sub = full[(full.genotype == geno) & (full.tissue == tissue) &
                       (full.subset == "all")]
            if sub.empty:
                ax.set_title(f"{geno} {tissue}: n.s.", fontsize=9); ax.axis("off"); continue
            sub = sub.nsmallest(8, "p_value").iloc[::-1]
            y = np.arange(len(sub))
            ax.barh(y, -np.log10(sub["p_value"]),
                    color=[SRC_COLORS.get(s, "#888") for s in sub["source"]])
            ax.set_yticks(y, [n[:38] for n in sub["name"]], fontsize=7)
            ax.set_title(f"{geno} — {tissue} ({len(full[(full.genotype==geno)&(full.tissue==tissue)&(full.subset=='all')])} terms)",
                         fontsize=9)
            ax.set_xlabel(r"$-\log_{10}$ adj $p$", fontsize=8)
            ax.tick_params(axis="x", labelsize=7)
    handles = [plt.Rectangle((0, 0), 1, 1, color=c) for c in SRC_COLORS.values()]
    fig.legend(handles, SRC_COLORS.keys(), ncol=6, loc="lower center",
               frameon=False, bbox_to_anchor=(0.5, -0.02))
    fig.suptitle("Spaceflight-DEG functional enrichment across the primary genotypes",
                 y=1.0, fontsize=13)
    fig.tight_layout(rect=(0, 0.02, 1, 0.99))
    fig.savefig(OUT / "figD1_primary_genotype_enrichment.png", dpi=300, bbox_inches="tight")
    plt.close(fig)
    print("  wrote results/ml/figD1_primary_genotype_enrichment.png")

    # ---- summary json (top terms per genotype x tissue 'all') ----
    summ = {"sources": SOURCES, "threshold": "g_SCS < 0.05",
            "background_loci": {t: len(bg[t]) for t in TISSUES}, "by_query": {}}
    for lab, g in full[full.subset == "all"].groupby("query_label"):
        summ["by_query"][lab] = {
            "n_terms": int(len(g)),
            "top_terms": g.nsmallest(6, "p_value")[["source", "native", "name", "p_value"]]
                          .assign(p_value=lambda d: d["p_value"].map(lambda v: float(f"{v:.2e}")))
                          .to_dict("records")}
    with open(OUT / "primary_enrichment_summary.json", "w") as fh:
        json.dump(summ, fh, indent=2)
    print("  wrote results/ml/primary_enrichment_summary.json\n\nDone.")


if __name__ == "__main__":
    main()
