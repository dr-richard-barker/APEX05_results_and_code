#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Per-cell-type stress decoding (PhysioSpace-style, cell-type resolved)
# =============================================================================
#
#  Combines the cell-type projection (Stage 3, PCMDB markers) with the stress
#  resemblance (Stage 6) to answer "does the columella / epidermis / mesophyll
#  flight response resemble hypoxia, oxidative or defence stress?".
#
#  METHOD (powered for small cell-type sets): the stress-axis enrichment is run on
#  the FULL genotype x tissue DEG set (well powered, g:SCS-corrected, same GO
#  "response to" axes as apex05_physiospace_stress.py); we then take the MEMBER
#  DEGs of each significant stress term and attribute them to cell types via the
#  PCMDB markers. Each cell-type x stress-axis cell = the number of flight-DEGs
#  that are BOTH a marker of that cell type AND a member of that stress programme.
#  This avoids testing tiny per-cell-type sets directly (which is underpowered).
#  cax2-2 has too few DEGs to decode (honest near-empty result).
#
#  OUTPUTS  results/tables/apex05_celltype_stress_decoding.csv
#           results/ml/figL2_celltype_stress_decoding.png
#           results/ml/celltype_stress_summary.json
#  RUN  python analysis/ml/apex05_celltype_stress_decoding.py   (needs internet: g:Profiler)
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
from apex05_physiospace_stress import STRESS_AXES, axis_of      # reuse stress axes

DES = A.REPO / "results/tables/deseq2"
MARK = A.REPO / "data/genesets/pcmdb_arabidopsis_celltype_markers.csv"
TBL, OUT = A.REPO / "results/tables", A.REPO / "results/ml"
PADJ, LFC, MIN_HITS = 0.05, 1.0, 1       # min shared genes to report a cell x stress cell
GENOS = A.PRIMARY_GENOTYPES
gp = GProfiler(return_dataframe=True)


def _locus(x): return re.sub(r"\.\d+$", "", str(x).strip())


def markers():
    m = pd.read_csv(MARK)
    return {t: {ct: {_locus(x) for x in g.gene} for ct, g in sub.groupby("cell_type")}
            for t, sub in m.groupby("tissue")}


def deg_tested(geno, tissue):
    slug = geno.replace("-", "").lower()
    d = pd.read_csv(DES / f"apex05_deseq2_{slug}_{tissue}.csv")
    d["locus"] = d["gene"].map(_locus)
    tested = sorted(set(d["locus"]))
    deg = set(d[(pd.to_numeric(d.padj, errors="coerce") < PADJ) &
               (pd.to_numeric(d.log2FoldChange, errors="coerce").abs() > LFC)]["locus"])
    return deg, tested


def stress_members(deg, tested):
    """{stress_axis: set(member flight-DEG loci)} from GO enrichment on the full DEG set."""
    if len(deg) < 5:
        return {}
    res = gp.profile(organism="athaliana", query=sorted(deg), sources=["GO:BP"],
                     user_threshold=0.05, significance_threshold_method="g_SCS",
                     background=sorted(tested), domain_scope="custom", no_evidences=False)
    if res.empty or "intersections" not in res.columns:
        return {}
    res["axis"] = res["name"].map(axis_of)
    res = res.dropna(subset=["axis"])
    out: dict[str, set] = {}
    for _, r in res.iterrows():
        genes = set()
        for x in (r["intersections"] or []):
            genes |= set(x) if isinstance(x, (list, tuple)) else {x}
        out.setdefault(r["axis"], set()).update(_locus(g) for g in genes)
    return out


def main():
    mk = markers()
    rows = []
    for geno in GENOS:
        for tissue in A.TISSUES:
            deg, tested = deg_tested(geno, tissue)
            sm = stress_members(deg, tested)                 # axis -> member DEG loci
            for ct, genes in mk.get(tissue, {}).items():
                ct_deg = deg & genes                         # this cell type's flight DEGs
                if not ct_deg:
                    continue
                for ax, members in sm.items():
                    hits = ct_deg & members
                    if len(hits) >= MIN_HITS:
                        rows.append({"genotype": geno, "tissue": tissue, "cell_type": ct,
                                     "n_celltype_flight_DEG": len(ct_deg), "stress_axis": ax,
                                     "n_shared": len(hits), "genes": ";".join(sorted(hits))})
            print(f"  [{geno} {tissue}] stress axes: {sorted(sm, key=lambda a: -len(sm[a]))[:4]}")
    df = pd.DataFrame(rows)
    df.sort_values(["tissue", "genotype", "cell_type", "n_shared"],
                   ascending=[True, True, True, False]).to_csv(
        TBL / "apex05_celltype_stress_decoding.csv", index=False)

    # figure: cell-type x stress-axis heatmaps, one panel per genotype x tissue that has data
    panels = [(g, t) for g in GENOS for t in A.TISSUES
              if not df[(df.genotype == g) & (df.tissue == t)].empty]
    axes_order = list(STRESS_AXES.keys())
    if panels:
        fig, axs = plt.subplots(1, len(panels), figsize=(5.2 * len(panels), 5.2), squeeze=False)
        vmax = df.n_shared.max()
        for ax, (g, t) in zip(axs[0], panels):
            sub = df[(df.genotype == g) & (df.tissue == t)]
            mat = sub.pivot_table(index="cell_type", columns="stress_axis",
                                  values="n_shared", aggfunc="max")
            mat = mat.reindex(columns=[a for a in axes_order if a in mat.columns])
            im = ax.imshow(mat.values, cmap="magma_r", aspect="auto", vmin=0, vmax=vmax)
            ax.set_xticks(range(mat.shape[1]), mat.columns, rotation=35, ha="right", fontsize=8)
            ax.set_yticks(range(mat.shape[0]), mat.index, fontsize=8)
            ax.set_title(f"{g} — {t}", fontsize=10)
            for i in range(mat.shape[0]):
                for j in range(mat.shape[1]):
                    v = mat.values[i, j]
                    if not np.isnan(v):
                        ax.text(j, i, f"{int(v)}", ha="center", va="center", fontsize=8,
                                color="white" if v > vmax * .6 else "black")
            fig.colorbar(im, ax=ax, fraction=.046, pad=.04, label="flight-DE genes")
        fig.suptitle("Per-cell-type stress decoding of the spaceflight response\n"
                     "(count of flight-DEGs that are both a cell-type marker and a stress-programme member)", y=1.03)
        fig.tight_layout()
        fig.savefig(OUT / "figL2_celltype_stress_decoding.png", dpi=300, bbox_inches="tight")
        plt.close(fig)

    json.dump({"method": "stress enrichment on the full DEG set (g:SCS), member DEGs attributed "
                         "to PCMDB cell types; cell x axis = count of shared flight-DEGs.",
               "records": ([] if df.empty else
                           df.sort_values("n_shared", ascending=False).to_dict("records"))},
              open(OUT / "celltype_stress_summary.json", "w"), indent=2)
    n_ct = 0 if df.empty else df[["genotype", "tissue", "cell_type"]].drop_duplicates().shape[0]
    print(f"\n{len(df)} cell-type × stress associations across {n_ct} decoded cell types.")
    print("Top associations:")
    for _, r in (df.sort_values("n_shared", ascending=False).head(12).iterrows()
                 if not df.empty else []):
        print(f"  {r.genotype:7} {r.tissue:5} {r.cell_type:28} {r.stress_axis:20} "
              f"n={r.n_shared}  ({r.genes[:45]})")
    print("\nwrote apex05_celltype_stress_decoding.csv, figL2, summary")


if __name__ == "__main__":
    main()
