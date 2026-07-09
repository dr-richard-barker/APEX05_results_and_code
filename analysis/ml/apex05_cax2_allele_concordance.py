#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Concordance of the two CAX2 alleles (cax2-2 vs cax2-3)
# =============================================================================
#
#  QUESTION
#  --------
#  cax2-2 (repo code "cax22") and cax2-3 ("cax23") are TWO INDEPENDENT MUTANT
#  ALLELES OF THE SAME GENE, CAX2. Two independent alleles of one gene should
#  therefore produce broadly *concordant* spaceflight (FL vs GC) transcriptome
#  responses — comparable numbers of differentially expressed genes (DEGs), a
#  high overlap between their DEG sets, and agreeing fold-change directions.
#  This script quantifies that concordance from the released data to inform the
#  decision of whether cax2-3 should be retained in the analysis.
#
#  EVIDENCE USED (all released in this repo)
#  -----------------------------------------
#  1. jVenn/UpSet DEG set-membership (the "CAX23oddity" QC export, V11) for root
#     and shoot — reconstruct each genotype's full DEG set and compute pairwise
#     Jaccard overlap. If cax2-2 overlaps cax2-3 no better than it overlaps the
#     unrelated genotypes, the two alleles are discordant.
#  2. The cax2-3 full FL-vs-GC contrast tables — independently threshold to a
#     DEG count as a cross-check on the set sizes.
#  3. The cax2-2 DEG workbooks (up/down, root/shoot) — provide cax2-2 DEG
#     identities and directions for a direction-concordance test on shared genes.
#
#  Note: these released tables predate the final 60-sample correction and thus
#  reflect cax2-3 *as flagged*; that is exactly the state under evaluation.
#
#  OUTPUT -> results/ml/  (figures, metrics JSON) and a written summary is in
#  docs/cax2_allele_concordance.md.
#  RUN     python analysis/ml/apex05_cax2_allele_concordance.py
# =============================================================================

from __future__ import annotations
import json, re
from pathlib import Path
from itertools import combinations

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

REPO = Path(__file__).resolve().parents[2]
OUT = REPO / "results" / "ml"
OUT.mkdir(parents=True, exist_ok=True)

GENO_TOKENS = {"CAX22": "cax2-2", "CAX23": "cax2-3", "COL00": "Col-0", "RBOHD": "rbohD"}
ORDER = ["Col-0", "cax2-2", "cax2-3", "rbohD"]
COLORS = {"Col-0": "#0072B2", "cax2-2": "#E69F00", "cax2-3": "#D55E00", "rbohD": "#009E73"}
PADJ, LFC = 0.05, 1.0
metrics: dict = {"thresholds": {"padj": PADJ, "abs_log2FC": LFC}}


def _locus(x: str) -> str:
    """Collapse a transcript ID (AT1G01010.1) to its locus (AT1G01010)."""
    return re.sub(r"\.\d+$", "", str(x).strip())


def _save(fig, name):
    fig.savefig(OUT / name, dpi=300, bbox_inches="tight")
    plt.close(fig)
    print(f"  wrote results/ml/{name}")


def reconstruct_sets(jvenn_csv: Path) -> dict[str, set]:
    """Rebuild each genotype's DEG locus set from jVenn exclusive-region columns.

    jVenn exports one column per intersection region (e.g. 'RootCAX22',
    'RootCAX22|RootCAX23', ...), each listing the genes unique to that region.
    A genotype's full set is the union of every column whose name mentions it.
    """
    df = pd.read_csv(jvenn_csv, dtype=str)
    sets: dict[str, set] = {name: set() for name in GENO_TOKENS.values()}
    for col in df.columns:
        ids = {_locus(v) for v in df[col].dropna()}
        for tok, name in GENO_TOKENS.items():
            if tok in col:
                sets[name] |= ids
    return sets


def jaccard(a: set, b: set) -> float:
    return len(a & b) / len(a | b) if (a or b) else 0.0


def analyse_tissue(tissue: str, jvenn_csv: Path) -> dict:
    print(f"\n[{tissue}] DEG set overlap from jVenn membership")
    sets = reconstruct_sets(jvenn_csv)
    sizes = {g: len(sets[g]) for g in ORDER}
    print("  DEG set sizes:", sizes)

    # Pairwise Jaccard matrix.
    J = pd.DataFrame(np.eye(len(ORDER)), index=ORDER, columns=ORDER)
    for a, b in combinations(ORDER, 2):
        J.loc[a, b] = J.loc[b, a] = jaccard(sets[a], sets[b])

    # The key numbers: does cax2-2 overlap its sibling allele cax2-3 more than
    # it overlaps the unrelated genotypes?
    j_alleles = J.loc["cax2-2", "cax2-3"]
    j_cax22_others = {g: J.loc["cax2-2", g] for g in ("Col-0", "rbohD")}
    size_ratio = sizes["cax2-3"] / max(sizes["cax2-2"], 1)
    print(f"  cax2-2 vs cax2-3 Jaccard = {j_alleles:.3f}; "
          f"cax2-2 vs Col-0/rbohD = {j_cax22_others}")
    print(f"  cax2-3 DEG set is {size_ratio:.1f}x the size of cax2-2's")

    return {
        "deg_set_sizes": sizes,
        "jaccard_matrix": J.round(4).to_dict(),
        "cax2-2_vs_cax2-3_jaccard": round(float(j_alleles), 4),
        "cax2-2_vs_others_jaccard": {k: round(float(v), 4) for k, v in j_cax22_others.items()},
        "cax2-3_to_cax2-2_size_ratio": round(float(size_ratio), 2),
        "_sets": sets, "_J": J, "_sizes": sizes,
    }


def cax23_full_deg(tissue: str) -> tuple[set, dict]:
    """Threshold the cax2-3 full contrast table -> DEG loci + sign map."""
    f = REPO / f"data/expression/contrasts_full/apex05_cax23_{tissue}_fl-vs-gc_full.csv"
    d = pd.read_csv(f)
    d.columns = [c.strip().lstrip("﻿") for c in d.columns]
    d["padj"] = pd.to_numeric(d["padj"], errors="coerce")
    d["log2FoldChange"] = pd.to_numeric(d["log2FoldChange"], errors="coerce")
    deg = d[(d["padj"] < PADJ) & (d["log2FoldChange"].abs() > LFC)].copy()
    deg["locus"] = deg["ID"].map(_locus)
    sign = deg.groupby("locus")["log2FoldChange"].mean().apply(np.sign).to_dict()
    return set(sign), sign


def cax22_deg(tissue: str) -> dict:
    """cax2-2 DEG loci + direction from the up/down workbooks."""
    n = {"root": (100, 223), "shoot": (45, 185)}[tissue]
    up = pd.read_excel(REPO / f"results/tables/apex5_cax22_{tissue}_up-regulated-{n[0]}.xlsx")
    dn = pd.read_excel(REPO / f"results/tables/apex5_cax22_{tissue}_down-regulated-{n[1]}.xlsx")
    idcol = "Transcript ID"
    sign = {}
    for _, r in up.iterrows():
        sign[_locus(r[idcol])] = 1.0
    for _, r in dn.iterrows():
        sign[_locus(r[idcol])] = -1.0
    return sign


def direction_concordance(tissue: str) -> dict:
    """For genes DE in BOTH alleles, how often do the FC directions agree?"""
    c23_set, c23_sign = cax23_full_deg(tissue)
    c22_sign = cax22_deg(tissue)
    shared = set(c22_sign) & c23_set
    if not shared:
        return {"n_shared": 0}
    agree = sum(1 for g in shared if c22_sign[g] == c23_sign[g])
    print(f"  [{tissue}] direction concordance on {len(shared)} shared DEGs: "
          f"{agree/len(shared):.0%} agree")
    return {
        "cax2-2_deg": len(c22_sign), "cax2-3_deg_thresholded": len(c23_set),
        "n_shared": len(shared), "frac_same_direction": round(agree / len(shared), 3),
    }


def main():
    print("=" * 68)
    print("APEX-05 | CAX2 allele concordance (cax2-2 vs cax2-3)")
    print("=" * 68)
    tissues = {"root": REPO / "archive/diagnostics/apex05_roots_v11_cax23-oddity.csv",
               "shoot": REPO / "results/tables/apex5_cax23_shoot_v11_cax23-oddity.csv"}

    res = {t: analyse_tissue(t, csv) for t, csv in tissues.items()}
    conc = {t: direction_concordance(t) for t in tissues}

    # ---- Figure 1: DEG set size per genotype (root vs shoot) ----
    fig, ax = plt.subplots(figsize=(6.4, 4.4))
    x = np.arange(len(ORDER)); w = 0.38
    for i, t in enumerate(("root", "shoot")):
        vals = [res[t]["_sizes"][g] for g in ORDER]
        ax.bar(x + (i - 0.5) * w, vals, w, label=t,
               color=[COLORS[g] for g in ORDER], alpha=0.75 if t == "root" else 1.0,
               edgecolor="black", linewidth=0.5, hatch="" if t == "root" else "//")
    for i, t in enumerate(("root", "shoot")):
        for j, g in enumerate(ORDER):
            v = res[t]["_sizes"][g]
            ax.text(x[j] + (i - 0.5) * w, v + 40, str(v), ha="center", va="bottom", fontsize=7)
    ax.set_xticks(x, ORDER); ax.set_ylabel("flight-responsive DEG set size")
    ax.set_title("cax2-3 has a vastly inflated DEG set vs its sibling allele cax2-2")
    ax.legend(title="tissue (solid=root, hatched=shoot)", frameon=False, fontsize=8)
    _save(fig, "figC1_cax2_deg_set_sizes.png")

    # ---- Figure 2: pairwise Jaccard heatmaps ----
    fig, axes = plt.subplots(1, 2, figsize=(9.2, 4.2))
    for ax, t in zip(axes, ("root", "shoot")):
        J = res[t]["_J"].loc[ORDER, ORDER]
        im = ax.imshow(J.values, cmap="magma_r", vmin=0, vmax=0.5)
        ax.set_xticks(range(4), ORDER, rotation=30, ha="right")
        ax.set_yticks(range(4), ORDER)
        ax.set_title(f"{t}: DEG-set Jaccard")
        for i in range(4):
            for k in range(4):
                ax.text(k, i, f"{J.values[i, k]:.2f}", ha="center", va="center",
                        color="white" if J.values[i, k] > 0.25 else "black", fontsize=8)
        fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
    fig.suptitle("Two CAX2 alleles are NOT each other's closest match", y=1.02)
    _save(fig, "figC2_cax2_jaccard.png")

    # ---- metrics ----
    out = {"thresholds": metrics["thresholds"], "direction_concordance": conc}
    for t in tissues:
        r = res[t]
        out[t] = {k: v for k, v in r.items() if not k.startswith("_")}
    with open(OUT / "cax2_concordance_metrics.json", "w") as fh:
        json.dump(out, fh, indent=2)
    print(f"\n  wrote results/ml/cax2_concordance_metrics.json")

    # ---- headline ----
    print("\n" + "-" * 68)
    for t in tissues:
        r = res[t]
        print(f"{t:>5}: cax2-3 DEGs = {r['_sizes']['cax2-3']:>5} "
              f"({r['cax2-3_to_cax2-2_size_ratio']:.1f}x cax2-2); "
              f"cax2-2~cax2-3 J={r['cax2-2_vs_cax2-3_jaccard']:.2f} "
              f"vs cax2-2~Col-0 J={r['cax2-2_vs_others_jaccard']['Col-0']:.2f}, "
              f"cax2-2~rbohD J={r['cax2-2_vs_others_jaccard']['rbohD']:.2f}")
    print("-" * 68)
    print("Done.")


if __name__ == "__main__":
    main()
