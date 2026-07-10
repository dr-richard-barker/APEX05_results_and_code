#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | PhysioSpace-style stress resemblance of the flight response
# =============================================================================
#
#  "Which stresses does the spaceflight response resemble?" Rather than fabricate
#  a stress reference, we use the curated Gene Ontology "response to <stimulus>"
#  categories as the reference stress programmes and test how strongly each
#  genotype's flight DEGs (DESeq2) resemble each stress programme (g:Profiler
#  over-representation, tissue-specific tested-gene background). GO terms are
#  grouped into interpretable stress axes (cold, heat, salt, water/osmotic,
#  oxidative, wounding, hypoxia, light/UV, ABA/JA/SA/ethylene, biotic, metal).
#
#  This is an over-representation ("resemblance") read-out, not a directional
#  PhysioSpace projection — labelled as such.
#
#  OUTPUTS  results/tables/apex05_stress_resemblance.csv
#           results/ml/figL1_stress_resemblance.png
#           results/ml/stress_resemblance_summary.json
#  RUN  python analysis/ml/apex05_physiospace_stress.py    (needs internet: g:Profiler)
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

# stress axis -> keywords matched in the GO term name ("response to ...")
STRESS_AXES = {
    "cold/freezing": ["cold", "freezing", "chilling"],
    "heat": ["heat", "high temperature"],
    "salt": ["salt", "sodium"],
    "water/osmotic/drought": ["water deprivation", "osmotic", "drought", "desiccation"],
    "oxidative/ROS": ["oxidative", "reactive oxygen", "hydrogen peroxide", "superoxide"],
    "wounding": ["wounding"],
    "hypoxia/anaerobic": ["hypoxia", "anaerobic", "decreased oxygen", "oxygen levels"],
    "light/UV/radiation": ["light", "uv", "radiation", "photo"],
    "ABA": ["abscisic"],
    "jasmonate": ["jasmonic", "jasmonate"],
    "salicylate": ["salicylic", "salicylate"],
    "ethylene": ["ethylene"],
    "biotic/defense": ["bacterium", "fungus", "oomycetes", "defense", "biotic",
                       "insect", "herbivore", "virus", "pathogen"],
    "metal/ion": ["metal", "cadmium", "iron", "zinc", "copper", "aluminum"],
}


def _locus(x): return re.sub(r"\.\d+$", "", str(x).strip())


def deg_and_bg(geno, tissue):
    slug = geno.replace("-", "").lower()
    d = pd.read_csv(DES / f"apex05_deseq2_{slug}_{tissue}.csv")
    d["locus"] = d["gene"].map(_locus)
    bg = sorted(set(d["locus"]))
    deg = sorted(set(d[(pd.to_numeric(d.padj, errors="coerce") < PADJ) &
                       (pd.to_numeric(d.log2FoldChange, errors="coerce").abs() > LFC)]["locus"]))
    return deg, bg


def axis_of(term_name: str):
    t = term_name.lower()
    if not t.startswith("response to"):
        return None
    for axis, kws in STRESS_AXES.items():
        if any(k in t for k in kws):
            return axis
    return None


def main():
    rows = []
    for geno in GENOS:
        for tissue in A.TISSUES:
            deg, bg = deg_and_bg(geno, tissue)
            if len(deg) < 3:
                print(f"  [{geno} {tissue}] {len(deg)} DEGs — too few, skipped")
                continue
            res = gp.profile(organism="athaliana", query=deg, sources=["GO:BP"],
                             user_threshold=0.05, significance_threshold_method="g_SCS",
                             background=bg, domain_scope="custom", no_evidences=True)
            if res.empty:
                continue
            res["axis"] = res["name"].map(axis_of)
            res = res.dropna(subset=["axis"])
            # best (smallest p) term per stress axis
            for axis, g in res.groupby("axis"):
                best = g.loc[g["p_value"].idxmin()]
                rows.append({"genotype": geno, "tissue": tissue, "stress_axis": axis,
                             "neg_log10_p": round(-np.log10(best["p_value"]), 2),
                             "top_term": best["name"], "p_value": best["p_value"]})
            print(f"  [{geno} {tissue}] {res.axis.nunique()} stress axes matched "
                  f"(of {len(deg)} DEGs)")

    df = pd.DataFrame(rows)
    df.sort_values(["tissue", "genotype", "p_value"]).to_csv(
        TBL / "apex05_stress_resemblance.csv", index=False)

    # heatmap: stress axis x (genotype,tissue)
    fig, axes = plt.subplots(1, 2, figsize=(12, 6))
    order = list(STRESS_AXES.keys())
    for ax, tissue in zip(axes, A.TISSUES):
        sub = df[df.tissue == tissue]
        mat = sub.pivot_table(index="stress_axis", columns="genotype",
                              values="neg_log10_p").reindex(index=order,
                              columns=[g for g in GENOS if g in sub.genotype.values])
        im = ax.imshow(mat.values, cmap="magma_r", aspect="auto",
                       vmin=0, vmax=np.nanmax(df.neg_log10_p))
        ax.set_xticks(range(mat.shape[1]), mat.columns, rotation=20, ha="right")
        ax.set_yticks(range(len(order)), order, fontsize=8)
        ax.set_title(f"{tissue}")
        for i in range(mat.shape[0]):
            for j in range(mat.shape[1]):
                v = mat.values[i, j]
                if not np.isnan(v) and v > 0:
                    ax.text(j, i, f"{v:.1f}", ha="center", va="center", fontsize=7,
                            color="white" if v > np.nanmax(df.neg_log10_p) * .6 else "black")
        fig.colorbar(im, ax=ax, fraction=.046, pad=.04, label=r"$-\log_{10}p$")
    fig.suptitle("Stress-programme resemblance of the spaceflight response "
                 "(GO 'response to', g:SCS-corrected)", y=1.0)
    fig.tight_layout()
    fig.savefig(OUT / "figL1_stress_resemblance.png", dpi=300, bbox_inches="tight")
    plt.close(fig)

    json.dump({"method": "GO:BP 'response to' over-representation grouped into stress axes; "
                         "resemblance (overlap), not directional projection",
               "axes": list(STRESS_AXES.keys()),
               "records": df.sort_values("p_value").to_dict("records")[:40]},
              open(OUT / "stress_resemblance_summary.json", "w"), indent=2, default=str)
    print("\nTop stress resemblances:")
    for _, r in df.sort_values("p_value").head(12).iterrows():
        print(f"  {r['genotype']:7} {r['tissue']:5} {r['stress_axis']:22} "
              f"-log10p={r['neg_log10_p']:.1f}  ({r['top_term'][:40]})")
    print("\nwrote apex05_stress_resemblance.csv, figL1, summary")


if __name__ == "__main__":
    main()
