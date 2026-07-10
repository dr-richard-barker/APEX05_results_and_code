#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Exploratory ligand-receptor (peptide-signalling) flight response
# =============================================================================
#
#  *** EXPLORATORY — NOT cell-cell communication inference. ***
#  True cell-cell communication (PlantCellChat / PlantPhoneDB) requires SINGLE-CELL
#  data with cell-type annotations, which APEX-05 (bulk RNA-seq) does not have.
#  Instead we ask a bulk-level question: are the two partners of a *canonical
#  Arabidopsis peptide ligand-receptor pair* flight-responsive (DESeq2), and in
#  what direction? Co-regulation of a ligand and its receptor under spaceflight is
#  suggestive of a modulated signalling axis, but cannot localise it to cell-cell
#  signalling without single-cell resolution.
#
#  Pairs are well-established peptide-hormone / receptor-kinase modules from the
#  plant signalling literature (CLV3-CLV1, IDA-HAE/HSL2, TDIF-PXY, RGF-RGI,
#  EPF-ER/TMM, RALF-FER, PSK-PSKR, PEP-PEPR, CIF-GSO, PSY1-PSY1R, SCOOP-MIK2, ...).
#  Gene IDs are resolved from symbols authoritatively via g:Profiler (gp.convert)
#  — no hand-typed AGI IDs.
#
#  OUTPUTS  results/tables/apex05_ligand_receptor_flight.csv
#           results/ml/figN1_ligand_receptor.png
#           results/ml/ligand_receptor_summary.json
#  RUN  python analysis/ml/apex05_ligand_receptor.py     (needs internet: g:Profiler)
# =============================================================================

from __future__ import annotations
import re, json
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from gprofiler import GProfiler

import apex05_data as A
DES = A.REPO / "results/tables/deseq2"
TBL, OUT = A.REPO / "results/tables", A.REPO / "results/ml"
PADJ, LFC = 0.05, 1.0
GENOS = A.PRIMARY_GENOTYPES
gp = GProfiler(return_dataframe=True)

# canonical Arabidopsis peptide ligand -> receptor(s) (literature modules)
PAIRS = {
    "CLV3": ["CLV1", "BAM1", "BAM2", "BAM3"],
    "CLE40": ["ACR4", "CLV1"],
    "CLE41": ["PXY"], "CLE44": ["PXY"],
    "IDA": ["HAE", "HSL2"],
    "EPF1": ["ER", "ERL1", "ERL2", "TMM"], "EPF2": ["ER", "TMM"],
    "EPFL9": ["ER", "TMM"],
    "RGF1": ["RGI1", "RGI2", "RGI3"], "RGF2": ["RGI1"], "RGF3": ["RGI1"],
    "RALF1": ["FER", "ANXUR1", "ANXUR2"], "RALF23": ["FER"],
    "PSK1": ["PSKR1", "PSKR2"],
    "PROPEP1": ["PEPR1", "PEPR2"], "PROPEP3": ["PEPR1"],
    "CIF1": ["GSO1", "GSO2"], "CIF2": ["GSO1", "GSO2"],
    "PIP1": ["RLK7"], "PIP2": ["RLK7"],
    "PSY1": ["PSY1R"],
    "PROSCOOP12": ["MIK2"],
}


def _locus(x): return re.sub(r"\.\d+$", "", str(x).strip())


def resolve_symbols():
    syms = sorted(set(PAIRS) | {r for v in PAIRS.values() for r in v})
    conv = gp.convert(organism="athaliana", query=syms)      # default namespace = AGI loci
    m = {}
    for _, r in conv.iterrows():
        if str(r["converted"]) not in ("None", "nan", "N/A", "none"):
            m.setdefault(r["incoming"], _locus(r["converted"]))
    return m


def deseq(geno, tissue):
    slug = geno.replace("-", "").lower()
    d = pd.read_csv(DES / f"apex05_deseq2_{slug}_{tissue}.csv")
    d["locus"] = d["gene"].map(_locus)
    d["padj"] = pd.to_numeric(d["padj"], errors="coerce")
    d["log2FoldChange"] = pd.to_numeric(d["log2FoldChange"], errors="coerce")
    lfc = d.groupby("locus")["log2FoldChange"].mean()
    padj = d.groupby("locus")["padj"].min()
    deg = set(d[(d.padj < PADJ) & (d.log2FoldChange.abs() > LFC)]["locus"])
    return lfc, padj, deg


def main():
    sym2agi = resolve_symbols()
    print(f"resolved {len(sym2agi)}/{len(set(PAIRS)|{r for v in PAIRS.values() for r in v})} symbols to AGI")

    rows = []
    for geno in GENOS:
        for tissue in A.TISSUES:
            lfc, padj, deg = deseq(geno, tissue)
            for lig, recs in PAIRS.items():
                la = sym2agi.get(lig)
                if la is None:
                    continue
                for rec in recs:
                    ra = sym2agi.get(rec)
                    if ra is None:
                        continue
                    lig_de, rec_de = la in deg, ra in deg
                    rows.append({
                        "genotype": geno, "tissue": tissue, "ligand": lig, "receptor": rec,
                        "ligand_log2FC": round(float(lfc.get(la, np.nan)), 3),
                        "receptor_log2FC": round(float(lfc.get(ra, np.nan)), 3),
                        "ligand_padj": float(padj.get(la, np.nan)),
                        "receptor_padj": float(padj.get(ra, np.nan)),
                        "ligand_DE": lig_de, "receptor_DE": rec_de,
                        "any_DE": lig_de or rec_de, "both_DE": lig_de and rec_de})
    cols = ["genotype", "tissue", "ligand", "receptor", "ligand_log2FC",
            "receptor_log2FC", "ligand_padj", "receptor_padj",
            "ligand_DE", "receptor_DE", "any_DE", "both_DE"]
    df = pd.DataFrame(rows, columns=cols)
    if not df.empty:
        df = df.sort_values(["any_DE", "genotype", "tissue"], ascending=[False, True, True])
    df.to_csv(TBL / "apex05_ligand_receptor_flight.csv", index=False)

    # figure: ALL canonical pairs (ligand vs receptor log2FC), per tissue. Shows the
    # peptide-signalling modules cluster near no-change = not flight-modulated in bulk.
    GC = {"Col-0": "#0072B2", "cax2-2": "#E69F00", "rbohD": "#009E73"}
    NOTABLE = 0.585                                      # ~1.5-fold, label threshold
    fig, axes = plt.subplots(1, 2, figsize=(12, 5.6), sharex=True, sharey=True)
    for ax, tissue in zip(axes, A.TISSUES):
        sub = df[df.tissue == tissue]
        ax.axhline(0, color="grey", lw=.6); ax.axvline(0, color="grey", lw=.6)
        for _, r in sub.iterrows():
            mx = max(abs(r.ligand_log2FC), abs(r.receptor_log2FC))
            ax.scatter(r.ligand_log2FC, r.receptor_log2FC, s=70 if mx > NOTABLE else 30,
                       color=GC[r.genotype], edgecolor="k" if r.any_DE else "none",
                       linewidth=1.1, alpha=0.8, zorder=3 if mx > NOTABLE else 2)
            if mx > 1.0:                                  # label only the clear outliers
                ax.annotate(f"{r.ligand}-{r.receptor}", (r.ligand_log2FC, r.receptor_log2FC),
                            textcoords="offset points", xytext=(4, 3), fontsize=6.5)
        ax.set_title(tissue); ax.set_xlabel("ligand flight log2FC (FL/GC)")
    axes[0].set_ylabel("receptor flight log2FC (FL/GC)")
    for g in GENOS:
        axes[1].scatter([], [], color=GC[g], label=g)
    axes[1].scatter([], [], facecolor="grey", edgecolor="k", label="a partner is DE")
    axes[1].legend(frameon=False, fontsize=8)
    fig.suptitle("Exploratory: canonical peptide ligand-receptor pairs cluster near no-change "
                 "under flight\n(bulk co-regulation — NOT cell-cell communication; needs single-cell)", y=1.02)
    fig.tight_layout()
    fig.savefig(OUT / "figN1_ligand_receptor.png", dpi=300, bbox_inches="tight")
    plt.close(fig)

    both = df[df.both_DE] if not df.empty else df
    both_recs = ([] if both.empty else
                 both[["genotype", "tissue", "ligand", "receptor",
                       "ligand_log2FC", "receptor_log2FC"]].to_dict("records"))
    json.dump({"caveat": "Exploratory bulk co-regulation of canonical peptide L-R pairs; "
                         "NOT cell-cell communication (needs single-cell data).",
               "n_pairs_tested": sum(len(v) for v in PAIRS.values()),
               "n_flight_responsive_components": len(df),
               "both_partners_DE": both_recs},
              open(OUT / "ligand_receptor_summary.json", "w"), indent=2)
    n_any = int(df.any_DE.sum()) if not df.empty else 0
    print(f"\nRESULT: of {len(df)} pair x genotype x tissue tests, {n_any} had a DEG "
          f"partner and {int(df.both_DE.sum()) if not df.empty else 0} had BOTH DE.")
    notable = df[(df.ligand_log2FC.abs() > 0.585) | (df.receptor_log2FC.abs() > 0.585)] \
        if not df.empty else df
    if notable.empty:
        print("  No canonical peptide L-R module is strongly flight-responsive in bulk —")
        print("  consistent with cell-type-specific peptide signalling diluted in bulk RNA-seq;")
        print("  proper cell-cell communication inference requires single-cell data.")
    else:
        print("  Sub-threshold candidates (|log2FC|>0.585 in a partner):")
        for _, r in notable.iterrows():
            print(f"    {r.genotype:7} {r.tissue:5} {r.ligand}-{r.receptor}: "
                  f"lig={r.ligand_log2FC:+.2f} rec={r.receptor_log2FC:+.2f}")
    print("\nwrote apex05_ligand_receptor_flight.csv, figN1, summary")


if __name__ == "__main__":
    main()
