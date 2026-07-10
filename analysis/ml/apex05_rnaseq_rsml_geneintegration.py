#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Deep RNA-seq <-> root-architecture integration (paired, per well/gene)
# =============================================================================
#
#  Extends the genotype-level multi-omics comparison (Stage 3b) to the WELL and
#  GENE level, using the position-paired design: each of the 12 primary-genotype
#  root wells has a matched flight (FL) and ground-control (GC) RNA-seq library
#  AND matched RSML root morphometrics, so a per-well flight response can be
#  computed for both modalities and correlated.
#
#   (A) Per-well coupling: transcriptomic flight magnitude (mean |Δ log2CPM| over
#       the union flight-DEG panel, FL−GC) vs root-architecture flight magnitude
#       (|delta root length|, FL−GC). Does a stronger transcriptional flight response
#       go with a stronger root-shortening response? (n = 12 wells.)
#   (B) Per-gene linkage (exploratory): for each union flight-DEG, Spearman
#       correlation of its per-well Δ expression with per-well Δ root length,
#       BH-FDR corrected — candidate root-architecture-coupled flight genes.
#
#  CAVEAT: n = 12 wells and genotype is a partial confound (genotypes differ in
#  both Δexpression and Δphenotype); (B) is exploratory and BH-corrected.
#
#  OUTPUTS  results/tables/apex05_rnaseq_rsml_gene_correlation.csv
#           results/ml/figI3_welllevel_coupling.png
#           results/ml/rnaseq_rsml_integration_summary.json
#  RUN  python analysis/ml/apex05_rnaseq_rsml_geneintegration.py   (g:Profiler optional)
# =============================================================================

from __future__ import annotations
import re, json
import numpy as np
import pandas as pd
from scipy.stats import spearmanr, false_discovery_control
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

import apex05_data as A
RSML = A.REPO / "data/morphometrics/apex05_rsml_day4_morphometrics.csv"
DES = A.REPO / "results/tables/deseq2"
TBL, OUT = A.REPO / "results/tables", A.REPO / "results/ml"
GENOS = A.PRIMARY_GENOTYPES
GENO_MAP = {"Col": "Col-0", "cax22": "cax2-2", "cax23": "cax2-3", "rbohD": "rbohD"}
GC = {"Col-0": "#0072B2", "cax2-2": "#E69F00", "rbohD": "#009E73"}
PADJ, LFC, TRAIT = 0.05, 1.0, "length"


def _locus(x): return re.sub(r"\.\d+$", "", str(x).strip())


def union_flight_degs():
    genes = set()
    for g in GENOS:
        slug = g.replace("-", "").lower()
        d = pd.read_csv(DES / f"apex05_deseq2_{slug}_root.csv")
        d["locus"] = d["gene"].map(_locus)
        genes |= set(d[(pd.to_numeric(d.padj, errors="coerce") < PADJ) &
                       (pd.to_numeric(d.log2FoldChange, errors="coerce").abs() > LFC)]["locus"])
    return genes


def main():
    counts, sheet = A.load_raw()
    logcpm = A.log2cpm(counts)
    logcpm.index = [_locus(i) for i in logcpm.index]
    logcpm = logcpm.groupby(level=0).mean()

    # morphometrics: per well x condition mean traits
    m = pd.read_csv(RSML, skipinitialspace=True); m.columns = [c.strip() for c in m.columns]
    m["genotype"] = m["Genotype"].map(GENO_MAP)
    trait_well = m.groupby(["Location", "Treatment", "genotype"])[TRAIT].mean()

    # per-well paired deltas (FL - GC) for expression and root length
    root = sheet[(sheet.tissue == "root") & (sheet.genotype.isin(GENOS))]
    deg = sorted(union_flight_degs() & set(logcpm.index))
    expr_delta, len_delta, well_meta = {}, {}, []
    for (geno, loc), grp in root.groupby(["genotype", "location"]):
        fl = grp[grp.condition == "FL"].index; gc = grp[grp.condition == "GC"].index
        if len(fl) != 1 or len(gc) != 1:
            continue
        try:
            dlen = trait_well[(loc, "FL", geno)] - trait_well[(loc, "GC", geno)]
        except KeyError:
            continue
        key = f"{geno}:{loc}"
        expr_delta[key] = (logcpm[fl[0]] - logcpm[gc[0]]).reindex(deg)
        len_delta[key] = dlen
        well_meta.append({"well": key, "genotype": geno, "location": loc,
                          "delta_length": round(float(dlen), 4),
                          "transcriptomic_flight_mag": round(float(expr_delta[key].abs().mean()), 4)})
    wm = pd.DataFrame(well_meta)
    E = pd.DataFrame(expr_delta).loc[deg]           # genes x wells (Δ expression)
    L = pd.Series(len_delta)                        # wells (Δ length)

    # ---- (A) well-level coupling ----
    x = wm["transcriptomic_flight_mag"].to_numpy()
    y = wm["delta_length"].abs().to_numpy()
    rho, p = spearmanr(x, y)
    print(f"[A] well-level coupling (n={len(wm)}): "
          f"transcriptomic flight magnitude vs |delta root length| Spearman rho={rho:.2f} p={p:.3f}")

    fig, ax = plt.subplots(figsize=(6.2, 5))
    for g in GENOS:
        s = wm[wm.genotype == g]
        ax.scatter(s.transcriptomic_flight_mag, s.delta_length.abs(), s=110,
                   color=GC[g], edgecolor="k", label=g, zorder=3)
    ax.set_xlabel("transcriptomic flight magnitude  (mean |Δ log2CPM| over flight-DEGs)")
    ax.set_ylabel("root-architecture flight magnitude  (|delta root length|, FL−GC)")
    ax.set_title(f"Per-well transcriptome ↔ root-architecture coupling\n"
                 f"(n={len(wm)} wells; Spearman ρ={rho:.2f}, p={p:.2f}; "
                 f"cax2-2 = low transcriptome, retained phenotype)")
    ax.legend(frameon=False, fontsize=9)
    fig.tight_layout()
    fig.savefig(OUT / "figI3_welllevel_coupling.png", dpi=300, bbox_inches="tight")
    plt.close(fig)

    # ---- (B) per-gene linkage to root length (exploratory, BH-FDR) ----
    Lv = L.reindex(E.columns).to_numpy()
    rows = []
    for gene in E.index:
        dv = E.loc[gene].to_numpy()
        if np.std(dv) < 1e-9:
            continue
        r, pv = spearmanr(dv, Lv)
        if not np.isnan(r):
            rows.append({"gene": gene, "spearman_rho": round(float(r), 3), "p": float(pv)})
    gc_df = pd.DataFrame(rows)
    if not gc_df.empty:
        gc_df["fdr_bh"] = false_discovery_control(gc_df["p"].to_numpy(), method="bh")
        gc_df = gc_df.sort_values("p")
        gc_df.to_csv(TBL / "apex05_rnaseq_rsml_gene_correlation.csv", index=False)
        n_sig = int((gc_df.fdr_bh < 0.1).sum())
        print(f"[B] gene vs delta-length (n={len(wm)} wells): {len(gc_df)} flight-DEGs tested, "
              f"{n_sig} at BH-FDR<0.1")
    else:
        n_sig = 0

    json.dump({"trait": TRAIT, "n_wells": int(len(wm)),
               "welllevel_coupling": {"spearman_rho": round(float(rho), 3), "p": round(float(p), 4)},
               "gene_linkage_FDR<0.1": n_sig,
               "note": "Paired FL-GC per-well deltas; primary genotypes; per-gene linkage "
                       "exploratory (n=12, genotype partial confound, BH-corrected).",
               "wells": wm.to_dict("records")},
              open(OUT / "rnaseq_rsml_integration_summary.json", "w"), indent=2)
    print("\nwrote figI3, apex05_rnaseq_rsml_gene_correlation.csv, summary")


if __name__ == "__main__":
    main()
