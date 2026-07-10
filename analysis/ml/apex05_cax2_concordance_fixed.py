#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Re-test CAX2 allele concordance on the CORRECTED (fixed v2.2) data
# =============================================================================
#
#  The original cax2-3 exclusion (docs/cax2_allele_concordance.md) rested on the
#  pre-fix, V11-era DEG tables. Now that a corrected count matrix
#  (run1 v2.2 fix2) is available, this script re-runs the same concordance test
#  on fresh DESeq2 results — does fixed cax2-3 still show a grossly inflated,
#  non-preferentially-overlapping DEG set vs its sibling allele cax2-2?
#
#  Uses flight-referenced DESeq2 log2FC (positive = induced by flight), so no
#  orientation guessing is needed. Compares to the pre-fix metrics if present.
#
#  DEPENDS ON  results/tables/deseq2/apex05_deseq2_*.csv  (run apex05_deseq2_flight.py first)
#  OUTPUTS     results/ml/figF1_cax2_concordance_fixed.png
#              results/ml/cax2_concordance_fixed_metrics.json
#  RUN  python analysis/ml/apex05_cax2_concordance_fixed.py
# =============================================================================

from __future__ import annotations
import json
from pathlib import Path
from itertools import combinations

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

import apex05_data as A
DES = A.REPO / "results/tables/deseq2"
OUT = A.REPO / "results/ml"
PADJ, LFC = 0.05, 1.0
ORDER = ["Col-0", "cax2-2", "cax2-3", "rbohD"]
COLORS = {"Col-0": "#0072B2", "cax2-2": "#E69F00", "cax2-3": "#D55E00", "rbohD": "#009E73"}


def deg(geno: str, tissue: str) -> pd.DataFrame:
    slug = geno.replace("-", "").lower()
    d = pd.read_csv(DES / f"apex05_deseq2_{slug}_{tissue}.csv")
    d = d[(pd.to_numeric(d.padj, errors="coerce") < PADJ) &
          (pd.to_numeric(d.log2FoldChange, errors="coerce").abs() > LFC)]
    return d.set_index("gene")


def jaccard(a, b):
    a, b = set(a), set(b)
    return len(a & b) / len(a | b) if (a or b) else 0.0


def analyse(tissue: str) -> dict:
    degs = {g: deg(g, tissue) for g in ORDER}
    sizes = {g: len(degs[g]) for g in ORDER}
    J = pd.DataFrame(np.eye(4), index=ORDER, columns=ORDER)
    for a, b in combinations(ORDER, 2):
        J.loc[a, b] = J.loc[b, a] = jaccard(degs[a].index, degs[b].index)

    shared = degs["cax2-2"].index.intersection(degs["cax2-3"].index)
    if len(shared):
        s22 = np.sign(degs["cax2-2"].loc[shared, "log2FoldChange"])
        s23 = np.sign(degs["cax2-3"].loc[shared, "log2FoldChange"])
        dir_agree = float((s22.values == s23.values).mean())
    else:
        dir_agree = None
    ratio = sizes["cax2-3"] / max(sizes["cax2-2"], 1)
    print(f"[{tissue}] sizes={sizes}")
    print(f"   cax2-3/cax2-2 size ratio = {ratio:.1f}x ; "
          f"cax2-2~cax2-3 J={J.loc['cax2-2','cax2-3']:.3f} vs "
          f"cax2-2~Col-0 {J.loc['cax2-2','Col-0']:.3f}, cax2-2~rbohD {J.loc['cax2-2','rbohD']:.3f} ; "
          f"shared={len(shared)} dir-agree={dir_agree}")
    return {"tissue": tissue, "deg_sizes": sizes,
            "cax2-3_to_cax2-2_ratio": round(float(ratio), 2),
            "jaccard_cax22_cax23": round(float(J.loc['cax2-2', 'cax2-3']), 4),
            "jaccard_cax22_col0": round(float(J.loc['cax2-2', 'Col-0']), 4),
            "jaccard_cax22_rbohd": round(float(J.loc['cax2-2', 'rbohD']), 4),
            "n_shared_cax2": int(len(shared)),
            "direction_agreement": None if dir_agree is None else round(dir_agree, 3),
            "_sizes": sizes, "_J": J}


def main():
    print("=" * 66)
    print("APEX-05 | CAX2 allele concordance on CORRECTED data (fixed v2.2)")
    print("=" * 66)
    res = {t: analyse(t) for t in A.TISSUES}

    fig, axes = plt.subplots(1, 3, figsize=(13.5, 4.3))
    x = np.arange(4); w = 0.38
    for i, t in enumerate(A.TISSUES):
        vals = [res[t]["_sizes"][g] for g in ORDER]
        axes[0].bar(x + (i - .5) * w, vals, w, color=[COLORS[g] for g in ORDER],
                    edgecolor="k", linewidth=.4, hatch="" if t == "root" else "//",
                    alpha=.8 if t == "root" else 1)
    axes[0].set_xticks(x, ORDER, rotation=25, ha="right")
    axes[0].set_ylabel("DESeq2 DEG count (fixed data)")
    axes[0].set_title("DEG set sizes (solid=root, hatch=shoot)")
    for ax, t in zip(axes[1:], A.TISSUES):
        Jm = res[t]["_J"].loc[ORDER, ORDER]
        im = ax.imshow(Jm.values, cmap="magma_r", vmin=0, vmax=0.5)
        ax.set_xticks(range(4), ORDER, rotation=30, ha="right"); ax.set_yticks(range(4), ORDER)
        ax.set_title(f"{t}: DEG Jaccard")
        for a in range(4):
            for b in range(4):
                ax.text(b, a, f"{Jm.values[a,b]:.2f}", ha="center", va="center",
                        color="white" if Jm.values[a, b] > .25 else "black", fontsize=8)
        fig.colorbar(im, ax=ax, fraction=.046, pad=.04)
    fig.suptitle("CAX2 allele concordance re-tested on the corrected (fix2) matrix", y=1.02)
    fig.tight_layout()
    fig.savefig(OUT / "figF1_cax2_concordance_fixed.png", dpi=300, bbox_inches="tight")
    plt.close(fig)
    print("\n  wrote results/ml/figF1_cax2_concordance_fixed.png")

    # compare to pre-fix metrics if available
    prefix = OUT / "cax2_concordance_metrics.json"
    comparison = None
    if prefix.exists():
        old = json.load(open(prefix))
        comparison = {t: {"prefix_ratio": old.get(t, {}).get("cax2-3_to_cax2-2_size_ratio"),
                          "fixed_ratio": res[t]["cax2-3_to_cax2-2_ratio"]} for t in A.TISSUES}

    out = {"thresholds": {"padj": PADJ, "abs_log2FC": LFC},
           "results": [{k: v for k, v in res[t].items() if not k.startswith("_")} for t in A.TISSUES],
           "prefix_vs_fixed_ratio": comparison}
    with open(OUT / "cax2_concordance_fixed_metrics.json", "w") as fh:
        json.dump(out, fh, indent=2)
    print("  wrote results/ml/cax2_concordance_fixed_metrics.json")

    # verdict
    ratios = [res[t]["cax2-3_to_cax2-2_ratio"] for t in A.TISSUES]
    print("\n" + "-" * 66)
    print(f"VERDICT: cax2-3/cax2-2 DEG-size ratio (fixed) = "
          f"{', '.join(f'{t} {r:.1f}x' for t, r in zip(A.TISSUES, ratios))}")
    still = any(r > 2 for r in ratios)
    print("cax2-3 STILL discordant on corrected data — exclusion stands."
          if still else
          "cax2-3 now comparable to cax2-2 — exclusion may warrant revisiting.")
    print("-" * 66)


if __name__ == "__main__":
    main()
