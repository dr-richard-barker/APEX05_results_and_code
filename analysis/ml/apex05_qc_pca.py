#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | QC of the fixed count matrix: global PCA, outliers, cax2-2 sanity
# =============================================================================
#  Validates the canonical 64-sample matrix before trusting the DE results, and
#  probes the surprising cax2-2 result (very few DESeq2 DEGs): is there a
#  sub-threshold flight signal, and do the samples cluster as expected?
#
#  OUTPUTS  results/ml/figG1_qc_pca.png, results/ml/qc_summary.json
#  RUN  python analysis/ml/apex05_qc_pca.py
# =============================================================================
from __future__ import annotations
import json
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler

import apex05_data as A
OUT = A.REPO / "results/ml"
GC = {"Col-0": "#0072B2", "cax2-2": "#E69F00", "cax2-3": "#D55E00", "rbohD": "#009E73"}


def top_var(logcpm: pd.DataFrame, n=2000):
    v = logcpm.var(axis=1).sort_values(ascending=False)
    return logcpm.loc[v.index[:n]]


def relaxed_deg(geno, tissue, padj=0.1, lfc=np.log2(1.5)):
    slug = geno.replace("-", "").lower()
    d = pd.read_csv(A.REPO / f"results/tables/deseq2/apex05_deseq2_{slug}_{tissue}.csv")
    d = d[(pd.to_numeric(d.padj, errors="coerce") < padj) &
          (pd.to_numeric(d.log2FoldChange, errors="coerce").abs() > lfc)]
    return len(d)


def main():
    counts, sheet = A.load_raw()
    logcpm = A.log2cpm(counts)

    # ---- global PCA (all 64 samples) ----
    X = StandardScaler().fit_transform(top_var(logcpm).to_numpy().T)
    pcs = PCA(n_components=3, random_state=42).fit(X)
    XY = pcs.transform(X)
    ev = pcs.explained_variance_ratio_

    fig, axes = plt.subplots(1, 3, figsize=(16, 5))
    # PC1/PC2 by tissue
    for tis, mk in [("root", "o"), ("shoot", "^")]:
        for g in A.ALL_GENOTYPES:
            m = (sheet.tissue == tis) & (sheet.genotype == g)
            axes[0].scatter(XY[m.values, 0], XY[m.values, 1], marker=mk, s=55,
                            color=GC[g], edgecolor="k", linewidth=.3,
                            label=f"{g} {tis}")
    axes[0].set_title(f"Global PCA — PC1({ev[0]:.0%}) vs PC2({ev[1]:.0%})\n(colour=genotype, ○ root / △ shoot)")
    axes[0].set_xlabel("PC1"); axes[0].set_ylabel("PC2")

    # within each tissue: FL vs GC per genotype (does condition separate?)
    metrics = {}
    for ax, tis in zip(axes[1:], A.TISSUES):
        sub = sheet[sheet.tissue == tis]
        Xt = StandardScaler().fit_transform(top_var(logcpm[sub.index]).to_numpy().T)
        xy = PCA(n_components=2, random_state=42).fit_transform(Xt)
        sub = sub.assign(pc1=xy[:, 0], pc2=xy[:, 1])
        for g in A.ALL_GENOTYPES:
            for cond, fill in [("FL", True), ("GC", False)]:
                m = (sub.genotype == g) & (sub.condition == cond)
                ax.scatter(sub.loc[m, "pc1"], sub.loc[m, "pc2"], s=55,
                           facecolor=GC[g] if fill else "none", edgecolor=GC[g],
                           linewidth=1.3, marker="o")
        ax.set_title(f"{tis}: PCA (filled=FL, open=GC)")
        ax.set_xlabel("PC1"); ax.set_ylabel("PC2")
        # FL vs GC centroid separation per genotype (standardised)
        for g in A.ALL_GENOTYPES:
            fl = sub[(sub.genotype == g) & (sub.condition == "FL")][["pc1", "pc2"]].mean()
            gc = sub[(sub.genotype == g) & (sub.condition == "GC")][["pc1", "pc2"]].mean()
            metrics.setdefault(g, {})[tis] = round(float(np.hypot(*(fl - gc))), 2)
    axes[0].legend(fontsize=5, ncol=2, loc="best")
    fig.suptitle("APEX-05 QC — fixed v2.2 matrix (64 samples)", y=1.02)
    fig.tight_layout()
    fig.savefig(OUT / "figG1_qc_pca.png", dpi=300, bbox_inches="tight")
    plt.close(fig)
    print("  wrote results/ml/figG1_qc_pca.png")

    # ---- cax2-2 relaxed-threshold check + centroid separations ----
    print("\nFL-vs-GC PCA centroid separation (bigger = clearer flight effect):")
    for g in A.ALL_GENOTYPES:
        print(f"  {g:7} root {metrics[g]['root']:>6}   shoot {metrics[g]['shoot']:>6}")
    print("\nRelaxed DEG counts (padj<0.1, |log2FC|>0.585 = 1.5-fold):")
    relaxed = {}
    for g in A.ALL_GENOTYPES:
        relaxed[g] = {t: relaxed_deg(g, t) for t in A.TISSUES}
        print(f"  {g:7} root {relaxed[g]['root']:>4}   shoot {relaxed[g]['shoot']:>4}")

    json.dump({"fl_gc_centroid_separation": metrics, "relaxed_deg_counts": relaxed,
               "pc_explained_var": [round(float(e), 4) for e in ev]},
              open(OUT / "qc_summary.json", "w"), indent=2)
    print("\n  wrote results/ml/qc_summary.json")


if __name__ == "__main__":
    main()
