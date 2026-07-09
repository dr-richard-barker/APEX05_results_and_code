#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Cell-type resolution of the bulk flight response (PCMDB projection)
# =============================================================================
#
#  APEX-05 is bulk RNA-seq, so we resolve the spaceflight response to cell types
#  by projecting onto a published reference (PCMDB marker sets built by
#  apex05_celltype_markers.py). Two complementary, honest read-outs — NOT
#  proportion deconvolution (which needs a reference expression matrix):
#
#   (A) Cell-type DEG enrichment: are a genotype's flight DEGs (DESeq2) enriched
#       for a cell type's markers? -> which cell-type programmes flight engages.
#       Hypergeometric, background = tested genes; robust to marker-set size.
#   (B) Cell-type signature shift: per sample, mean z-scored log2CPM of a cell
#       type's markers; FL-vs-GC standardised difference (Cohen's d) per genotype
#       -> whether flight shifts the representation/activity of each cell type.
#
#  Primary genotypes only (Col-0, cax2-2, rbohD); root vs leaf cell types matched
#  to tissue. Feeds the ggPlantmap anatomical figure (Stage 4).
#
#  OUTPUTS  results/tables/apex05_celltype_deg_enrichment.csv
#           results/tables/apex05_celltype_signature_shift.csv
#           results/ml/figH1_celltype_deg_enrichment.png
#           results/ml/figH2_celltype_signature_shift.png
#           results/ml/celltype_summary.json
#  RUN  python analysis/ml/apex05_celltype_projection.py
# =============================================================================

from __future__ import annotations
import json
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from scipy.stats import hypergeom

import apex05_data as A
MARK = A.REPO / "data/genesets/pcmdb_arabidopsis_celltype_markers.csv"
DES = A.REPO / "results/tables/deseq2"
TBL = A.REPO / "results/tables"
OUT = A.REPO / "results/ml"
PADJ, LFC = 0.05, 1.0
GENOS = A.PRIMARY_GENOTYPES


def load_markers():
    m = pd.read_csv(MARK)
    return {t: {ct: set(g.gene) for ct, g in sub.groupby("cell_type")}
            for t, sub in m.groupby("tissue")}


def deseq(geno, tissue):
    slug = geno.replace("-", "").lower()
    d = pd.read_csv(DES / f"apex05_deseq2_{slug}_{tissue}.csv")
    tested = set(d["gene"])
    deg = set(d[(pd.to_numeric(d.padj, errors="coerce") < PADJ) &
               (pd.to_numeric(d.log2FoldChange, errors="coerce").abs() > LFC)]["gene"])
    return tested, deg


def main():
    markers = load_markers()
    counts, sheet = A.load_raw()
    logcpm = A.log2cpm(counts)

    enrich_rows, shift_rows = [], []
    for tissue in A.TISSUES:
        cts = markers[tissue]
        # z-score genes across all samples of this tissue (all genotypes)
        tsamp = sheet[sheet.tissue == tissue].index
        z = logcpm[tsamp]
        z = z.sub(z.mean(axis=1), axis=0).div(z.std(axis=1).replace(0, np.nan), axis=0)

        for geno in GENOS:
            tested, deg = deseq(geno, tissue)
            gsamp = sheet[(sheet.tissue == tissue) & (sheet.genotype == geno)]
            fl = gsamp[gsamp.condition == "FL"].index
            gc = gsamp[gsamp.condition == "GC"].index
            for ct, genes in cts.items():
                mk = genes & tested                      # markers in the tested universe
                if len(mk) < 5:
                    continue
                # (A) DEG enrichment
                k = len(deg & mk)
                p = float(hypergeom.sf(k - 1, len(tested), len(deg), len(mk))) if deg else 1.0
                enrich_rows.append({"genotype": geno, "tissue": tissue, "cell_type": ct,
                                    "n_markers": len(mk), "deg_in_set": k,
                                    "expected": round(len(deg) * len(mk) / len(tested), 2),
                                    "p_hyper": p})
                # (B) signature shift (Cohen's d, FL vs GC)
                mkz = [g for g in mk if g in z.index]
                s = z.loc[mkz]
                fl_s, gc_s = s[fl].mean(axis=0), s[gc].mean(axis=0)
                pooled = np.sqrt((fl_s.var(ddof=1) + gc_s.var(ddof=1)) / 2) or np.nan
                d = float((fl_s.mean() - gc_s.mean()) / pooled) if pooled == pooled else np.nan
                shift_rows.append({"genotype": geno, "tissue": tissue, "cell_type": ct,
                                   "n_markers": len(mkz), "cohens_d_FL_vs_GC": round(d, 3)})

    enr = pd.DataFrame(enrich_rows)
    enr["neg_log10_p"] = -np.log10(enr["p_hyper"].clip(lower=1e-300))
    enr.sort_values(["tissue", "genotype", "p_hyper"]).to_csv(
        TBL / "apex05_celltype_deg_enrichment.csv", index=False)
    shift = pd.DataFrame(shift_rows)
    shift.to_csv(TBL / "apex05_celltype_signature_shift.csv", index=False)

    # ---- Figure H1: DEG cell-type enrichment heatmaps (root, shoot) ----
    _heatmap(enr, "neg_log10_p", "figH1_celltype_deg_enrichment.png",
             "Flight-DEG cell-type enrichment  (−log10 p, hypergeometric)",
             cmap="magma_r", center=None, sig=enr)
    # ---- Figure H2: signature shift heatmaps ----
    _heatmap(shift, "cohens_d_FL_vs_GC", "figH2_celltype_signature_shift.png",
             "Cell-type signature shift under flight  (Cohen's d, FL−GC)",
             cmap="RdBu_r", center=0.0, sig=None)

    summ = {"reference": "PCMDB (Zenodo 10.5281/zenodo.5101271)",
            "thresholds": {"padj": PADJ, "abs_log2FC": LFC, "min_markers": 5},
            "top_enrichments": (enr.sort_values("p_hyper")
                                .head(15)[["genotype", "tissue", "cell_type",
                                           "deg_in_set", "n_markers", "p_hyper"]]
                                .assign(p_hyper=lambda x: x.p_hyper.map(lambda v: float(f"{v:.2e}")))
                                .to_dict("records"))}
    json.dump(summ, open(OUT / "celltype_summary.json", "w"), indent=2)
    print("wrote enrichment + shift tables, figures, summary")
    print("\nTop flight-DEG cell-type enrichments:")
    for r in summ["top_enrichments"][:10]:
        print(f"  {r['genotype']:7} {r['tissue']:5} {r['cell_type']:28} "
              f"{r['deg_in_set']:>3}/{r['n_markers']:<4} p={r['p_hyper']:.1e}")


def _heatmap(df, value, fname, title, cmap, center, sig):
    fig, axes = plt.subplots(1, 2, figsize=(12, 6),
                             gridspec_kw={"width_ratios": [1.4, 1]})
    for ax, tissue in zip(axes, A.TISSUES):
        sub = df[df.tissue == tissue]
        mat = sub.pivot_table(index="cell_type", columns="genotype", values=value)
        mat = mat.reindex(columns=[g for g in GENOS if g in mat.columns])
        order = mat.abs().max(axis=1).sort_values(ascending=False).index
        mat = mat.loc[order]
        vmax = np.nanmax(np.abs(mat.values)) if center == 0 else np.nanmax(mat.values)
        vmin = -vmax if center == 0 else 0
        im = ax.imshow(mat.values, cmap=cmap, aspect="auto", vmin=vmin, vmax=vmax)
        ax.set_xticks(range(mat.shape[1]), mat.columns, rotation=20, ha="right")
        ax.set_yticks(range(mat.shape[0]), mat.index, fontsize=7)
        ax.set_title(f"{tissue}")
        if sig is not None:                              # star significant enrichments
            for i, ct in enumerate(mat.index):
                for j, g in enumerate(mat.columns):
                    row = sig[(sig.tissue == tissue) & (sig.cell_type == ct) & (sig.genotype == g)]
                    if len(row) and row.iloc[0]["p_hyper"] < 0.05:
                        ax.text(j, i, "*", ha="center", va="center", color="white", fontsize=11)
        fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04)
    fig.suptitle(title, y=1.0)
    fig.tight_layout()
    fig.savefig(OUT / fname, dpi=300, bbox_inches="tight")
    plt.close(fig)


if __name__ == "__main__":
    main()
