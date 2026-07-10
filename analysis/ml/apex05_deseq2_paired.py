#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Paired (well-blocked) DESeq2 — sensitivity analysis vs unpaired
# =============================================================================
#
#  The position-paired design (each plate well appears in both FL and GC; rep->well
#  resolved from S-numbers) allows a BLOCKED differential-expression model that
#  removes between-well variation:  design = ~ location + condition  (vs the
#  primary ~ condition). This is a power-boosting sensitivity check — it does NOT
#  replace the primary unpaired DESeq2 (apex05_deseq2_flight.py); it tests whether
#  blocking on well changes the flight-DEG calls.
#
#  Directions remain flight-referenced (contrast condition FL vs GC).
#
#  OUTPUTS  results/tables/deseq2_paired/apex05_deseq2_paired_<geno>_<tissue>.csv
#           results/tables/apex05_deseq2_paired_vs_unpaired.csv
#           results/ml/figO1_paired_vs_unpaired_DEGs.png
#  RUN  python analysis/ml/apex05_deseq2_paired.py
#  DEPS pydeseq2
# =============================================================================

from __future__ import annotations
import warnings
from pathlib import Path
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

import apex05_data as A
from pydeseq2.dds import DeseqDataSet
from pydeseq2.ds import DeseqStats
from pydeseq2.default_inference import DefaultInference

warnings.filterwarnings("ignore")
OUTD = A.REPO / "results/tables/deseq2_paired"; OUTD.mkdir(parents=True, exist_ok=True)
UNPAIRED = A.REPO / "results/tables/deseq2/apex05_deseq2_DEG_counts.csv"
PADJ, LFC = 0.05, 1.0
GC = {"Col-0": "#0072B2", "cax2-2": "#E69F00", "rbohD": "#009E73"}


def paired_group(counts, sheet, geno, tissue):
    sel = sheet[(sheet.genotype == geno) & (sheet.tissue == tissue)]
    X = counts[sel.index].T
    X = X.loc[:, X.sum(axis=0) > 0]
    meta = pd.DataFrame({"condition": sel["condition"].values,
                         "location": sel["location"].astype(str).values}, index=sel.index)
    inf = DefaultInference(n_cpus=4)
    dds = DeseqDataSet(counts=X, metadata=meta, design="~location + condition",
                       inference=inf, quiet=True)
    dds.deseq2()
    ds = DeseqStats(dds, contrast=["condition", "FL", "GC"], inference=inf, quiet=True)
    ds.summary()
    res = ds.results_df.sort_values("padj"); res.index.name = "gene"
    slug = geno.replace("-", "").lower()
    res.to_csv(OUTD / f"apex05_deseq2_paired_{slug}_{tissue}.csv")
    sig = res[(res.padj < PADJ) & (res.log2FoldChange.abs() > LFC)]
    return int(len(sig)), int((sig.log2FoldChange > 0).sum()), int((sig.log2FoldChange < 0).sum())


def main():
    print("=" * 66)
    print("APEX-05 | PAIRED DESeq2 (~location + condition)  vs unpaired")
    print("=" * 66)
    counts, sheet = A.load_raw()
    unp = pd.read_csv(UNPAIRED).set_index(["genotype", "tissue"])
    rows = []
    for geno in A.PRIMARY_GENOTYPES:
        for tissue in A.TISSUES:
            n, up, dn = paired_group(counts, sheet, geno, tissue)
            n_un = int(unp.loc[(geno, tissue), "n_DEG"])
            rows.append({"genotype": geno, "tissue": tissue,
                         "DEG_unpaired": n_un, "DEG_paired": n,
                         "paired_induced": up, "paired_repressed": dn,
                         "delta": n - n_un})
            print(f"  [{geno:7} {tissue:5}] unpaired={n_un:>4}  paired={n:>4}  "
                  f"(induced {up} / repressed {dn})")
    comp = pd.DataFrame(rows)
    comp.to_csv(A.REPO / "results/tables/apex05_deseq2_paired_vs_unpaired.csv", index=False)

    fig, ax = plt.subplots(figsize=(8, 4.8))
    labels = [f"{r.genotype}\n{r.tissue}" for _, r in comp.iterrows()]
    x = np.arange(len(comp)); w = 0.38
    ax.bar(x - w/2, comp.DEG_unpaired, w, label="unpaired (~condition)", color="#BBBBBB", edgecolor="k", linewidth=.4)
    ax.bar(x + w/2, comp.DEG_paired, w, label="paired (~location+condition)",
           color=[GC[g] for g in comp.genotype], edgecolor="k", linewidth=.4)
    for i, r in comp.iterrows():
        ax.text(i - w/2, r.DEG_unpaired + 3, str(r.DEG_unpaired), ha="center", fontsize=7)
        ax.text(i + w/2, r.DEG_paired + 3, str(r.DEG_paired), ha="center", fontsize=7)
    ax.set_xticks(x, labels, fontsize=8); ax.set_ylabel("flight DEGs (padj<0.05, |log2FC|>1)")
    ax.set_title("Well-blocked (paired) vs unpaired DESeq2 — sensitivity analysis")
    ax.legend(frameon=False, fontsize=9)
    fig.tight_layout()
    fig.savefig(A.REPO / "results/ml/figO1_paired_vs_unpaired_DEGs.png", dpi=300, bbox_inches="tight")
    plt.close(fig)
    print("\nwrote paired tables + comparison + figO1")
    print(comp.to_string(index=False))


if __name__ == "__main__":
    main()
