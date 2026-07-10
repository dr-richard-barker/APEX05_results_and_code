#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Multi-omics: RNA-seq flight response <-> RSML root architecture
# =============================================================================
#
#  The ALSDA well metadata bridges the two assays (shared plate Location/well;
#  see apex05_data.py). This script joins the transcriptomic flight response to
#  the day-4 primary-root morphometrics and asks the integrative question:
#
#     does a genotype's transcriptional flight response track its root-
#     architecture flight response? In particular, is cax2-2's attenuated
#     transcriptome mirrored by an attenuated phenotype?
#
#  Morphometrics has many seedlings per genotype x condition, so per-genotype
#  FL-vs-GC trait effects are well powered. Directions are flight-referenced
#  (FL vs GC; positive = larger/greater under spaceflight).
#
#  OUTPUTS  results/tables/apex05_morphometric_flight_effects.csv
#           results/tables/apex05_welllevel_rnaseq_rsml_join.csv
#           results/ml/figI1_morphometric_flight_effects.png
#           results/ml/figI2_transcriptome_vs_phenotype.png
#           results/ml/multiomics_summary.json
#  RUN  python analysis/ml/apex05_multiomics_integration.py
# =============================================================================

from __future__ import annotations
import json
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from scipy.stats import mannwhitneyu

import apex05_data as A
RSML = A.REPO / "data/morphometrics/apex05_rsml_day4_morphometrics.csv"
DEGC = A.REPO / "results/tables/deseq2/apex05_deseq2_DEG_counts.csv"
TBL, OUT = A.REPO / "results/tables", A.REPO / "results/ml"
GENO_MAP = {"Col": "Col-0", "cax22": "cax2-2", "cax23": "cax2-3", "rbohD": "rbohD"}
PRIMARY = A.PRIMARY_GENOTYPES
GC = {"Col-0": "#0072B2", "cax2-2": "#E69F00", "rbohD": "#009E73"}


def cohens_d(a, b):
    a, b = np.asarray(a, float), np.asarray(b, float)
    s = np.sqrt(((a.size - 1) * a.var(ddof=1) + (b.size - 1) * b.var(ddof=1)) /
                (a.size + b.size - 2))
    return (a.mean() - b.mean()) / s if s else np.nan


def main():
    m = pd.read_csv(RSML, skipinitialspace=True)
    m.columns = [c.strip() for c in m.columns]
    m["genotype"] = m["Genotype"].map(GENO_MAP)
    # informative primary-root traits = numeric with real variance
    cand = ["length", "vector_length", "surface", "volume", "diameter",
            "insertion_position", "insertion_angle", "n_child", "child_density"]
    traits = [t for t in cand if pd.to_numeric(m[t], errors="coerce").std() > 1e-9]
    print("informative traits:", traits)

    # ---- morphometric flight effects per genotype x trait ----
    rows = []
    for g in PRIMARY:
        sub = m[m.genotype == g]
        for t in traits:
            fl = pd.to_numeric(sub[sub.Treatment == "FL"][t], errors="coerce").dropna()
            gc = pd.to_numeric(sub[sub.Treatment == "GC"][t], errors="coerce").dropna()
            if len(fl) < 5 or len(gc) < 5:
                continue
            p = mannwhitneyu(fl, gc, alternative="two-sided").pvalue
            rows.append({"genotype": g, "trait": t, "n_FL": len(fl), "n_GC": len(gc),
                         "mean_FL": round(fl.mean(), 4), "mean_GC": round(gc.mean(), 4),
                         "pct_change": round(100 * (fl.mean() - gc.mean()) / gc.mean(), 1)
                         if gc.mean() else np.nan,
                         "cohens_d": round(cohens_d(fl, gc), 3),
                         "mannwhitney_p": p})
    eff = pd.DataFrame(rows)
    eff.to_csv(TBL / "apex05_morphometric_flight_effects.csv", index=False)

    # ---- well-level join (RNA-seq root wells <-> morphometric well means) ----
    counts, sheet = A.load_raw()
    rna = sheet[sheet.tissue == "root"][["genotype", "condition", "location"]].reset_index()
    wl = (m.groupby(["Location", "Treatment", "genotype"])[traits]
          .mean().reset_index()
          .rename(columns={"Location": "location", "Treatment": "condition"}))
    join = rna.merge(wl, on=["location", "condition", "genotype"], how="left")
    join.to_csv(TBL / "apex05_welllevel_rnaseq_rsml_join.csv", index=False)
    print(f"well-level join: {join[traits[0]].notna().sum()}/{len(join)} RNA-seq root "
          f"libraries matched to morphometrics")

    # ---- Figure I1: morphometric flight effect heatmap (genotype x trait) ----
    piv = eff.pivot(index="trait", columns="genotype", values="cohens_d").reindex(
        index=traits, columns=[g for g in PRIMARY if g in eff.genotype.values])
    fig, ax = plt.subplots(figsize=(5.6, 4.6))
    vmax = np.nanmax(np.abs(piv.values))
    im = ax.imshow(piv.values, cmap="RdBu_r", vmin=-vmax, vmax=vmax, aspect="auto")
    ax.set_xticks(range(piv.shape[1]), piv.columns)
    ax.set_yticks(range(piv.shape[0]), piv.index)
    for i, t in enumerate(piv.index):
        for j, g in enumerate(piv.columns):
            r = eff[(eff.trait == t) & (eff.genotype == g)]
            star = "*" if len(r) and r.iloc[0].mannwhitney_p < 0.05 else ""
            ax.text(j, i, f"{piv.values[i,j]:.2f}{star}", ha="center", va="center",
                    fontsize=8, color="white" if abs(piv.values[i, j]) > vmax * .6 else "black")
    ax.set_title("Root-architecture flight effect\n(Cohen's d, FL vs GC; * p<0.05)")
    fig.colorbar(im, ax=ax, fraction=.046, pad=.04)
    fig.tight_layout()
    fig.savefig(OUT / "figI1_morphometric_flight_effects.png", dpi=300, bbox_inches="tight")
    plt.close(fig)

    # ---- Figure I2: transcriptome vs phenotype flight-response magnitude ----
    degc = pd.read_csv(DEGC)
    deg_root = {r.genotype: r.n_DEG for _, r in degc[degc.tissue == "root"].iterrows()}
    pheno_mag = eff.groupby("genotype")["cohens_d"].apply(lambda s: s.abs().mean())
    fig, ax = plt.subplots(figsize=(5.4, 4.6))
    summ_rows = []
    for g in PRIMARY:
        x, y = deg_root.get(g, np.nan), float(pheno_mag.get(g, np.nan))
        ax.scatter(x, y, s=140, color=GC[g], edgecolor="k", zorder=3)
        ax.annotate(g, (x, y), textcoords="offset points", xytext=(8, 4), fontsize=10)
        summ_rows.append({"genotype": g, "root_DEGs": x, "mean_abs_morph_d": round(y, 3)})
    ax.set_xlabel("transcriptomic flight response  (root DESeq2 DEGs)")
    ax.set_ylabel("root-architecture flight response  (mean |Cohen's d|)")
    ax.set_title("Flight-response magnitude: transcriptome vs root architecture\n"
                 "(cax2-2's transcriptome is disproportionately attenuated vs its phenotype)")
    ax.margins(0.25)
    fig.tight_layout()
    fig.savefig(OUT / "figI2_transcriptome_vs_phenotype.png", dpi=300, bbox_inches="tight")
    plt.close(fig)

    json.dump({"informative_traits": traits,
               "morphometric_flight_effects": eff.to_dict("records"),
               "transcriptome_vs_phenotype": summ_rows,
               "note": "flight-referenced (FL vs GC). Morphometrics = day-4 primary "
                       "roots, many seedlings/genotype; transcriptome = DESeq2 root DEGs."},
              open(OUT / "multiomics_summary.json", "w"), indent=2)

    print("\nMorphometric flight effects (Cohen's d, FL vs GC):")
    print(eff.pivot(index="trait", columns="genotype", values="cohens_d").to_string())
    print("\nTranscriptome vs phenotype magnitude:")
    for r in summ_rows:
        print(f"  {r['genotype']:7} root DEGs={r['root_DEGs']:>4}  mean|d|={r['mean_abs_morph_d']}")


if __name__ == "__main__":
    main()
