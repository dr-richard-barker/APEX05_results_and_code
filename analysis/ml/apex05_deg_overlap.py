#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Flight-DEG overlap across the three primary genotypes (Venn)
# =============================================================================
#
#  How shared vs genotype-specific is the spaceflight response? Draws a 3-way
#  Venn of the DESeq2 flight-DEG sets (Col-0, cax2-2, rbohD) for root and shoot,
#  and writes the membership of every region (incl. the Col-0 ∩ rbohD shared core).
#
#  OUTPUTS  results/ml/figP1_deg_overlap_venn.png
#           results/tables/apex05_deg_overlap_membership.csv
#  RUN  python analysis/ml/apex05_deg_overlap.py
#  DEPS matplotlib-venn
# =============================================================================

from __future__ import annotations
import re
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib_venn import venn3, venn3_circles

import apex05_data as A
DES = A.REPO / "results/tables/deseq2"
PADJ, LFC = 0.05, 1.0
GENOS = A.PRIMARY_GENOTYPES
COL = {"Col-0": "#0072B2", "cax2-2": "#E69F00", "rbohD": "#009E73"}


def _locus(x): return re.sub(r"\.\d+$", "", str(x).strip())


def degs(geno, tissue):
    slug = geno.replace("-", "").lower()
    d = pd.read_csv(DES / f"apex05_deseq2_{slug}_{tissue}.csv")
    d["locus"] = d["gene"].map(_locus)
    return set(d[(pd.to_numeric(d.padj, errors="coerce") < PADJ) &
                 (pd.to_numeric(d.log2FoldChange, errors="coerce").abs() > LFC)]["locus"])


def main():
    fig, axes = plt.subplots(1, 2, figsize=(12, 5.6))
    rows = []
    for ax, tissue in zip(axes, A.TISSUES):
        S = {g: degs(g, tissue) for g in GENOS}
        v = venn3([S[g] for g in GENOS], set_labels=GENOS, ax=ax,
                  set_colors=[COL[g] for g in GENOS], alpha=0.55)
        venn3_circles([S[g] for g in GENOS], ax=ax, linewidth=0.8)
        ax.set_title(f"{tissue}  (Col-0 {len(S['Col-0'])}, cax2-2 {len(S['cax2-2'])}, "
                     f"rbohD {len(S['rbohD'])})", fontsize=10)
        # membership of every region
        A_, B_, C_ = S["Col-0"], S["cax2-2"], S["rbohD"]
        regions = {
            "Col-0_only": A_ - B_ - C_, "cax2-2_only": B_ - A_ - C_, "rbohD_only": C_ - A_ - B_,
            "Col-0&cax2-2": (A_ & B_) - C_, "Col-0&rbohD": (A_ & C_) - B_,
            "cax2-2&rbohD": (B_ & C_) - A_, "all_three": A_ & B_ & C_}
        for region, genes in regions.items():
            for gg in sorted(genes):
                rows.append({"tissue": tissue, "region": region, "gene": gg})
    fig.suptitle("Flight-DEG overlap across genotypes (DESeq2, padj<0.05, |log2FC|>1)", y=1.02)
    fig.tight_layout()
    fig.savefig(A.REPO / "results/ml/figP1_deg_overlap_venn.png", dpi=300, bbox_inches="tight")
    plt.close(fig)
    pd.DataFrame(rows).to_csv(A.REPO / "results/tables/apex05_deg_overlap_membership.csv", index=False)

    # print the shared Col-0 & rbohD core sizes
    for tissue in A.TISSUES:
        S = {g: degs(g, tissue) for g in GENOS}
        core = S["Col-0"] & S["rbohD"]
        print(f"{tissue}: Col-0 ∩ rbohD shared flight core = {len(core)} genes; "
              f"all-three = {len(S['Col-0'] & S['cax2-2'] & S['rbohD'])}")
    print("wrote figP1 + apex05_deg_overlap_membership.csv")


if __name__ == "__main__":
    main()
