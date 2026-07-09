#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | DESeq2 spaceflight differential expression (fixed v2.2 matrix)
# =============================================================================
#
#  Runs a proper DESeq2 FL-vs-GC test per genotype x tissue on the canonical
#  fixed count matrix (pydeseq2), for all four genotypes. Directions are
#  flight-referenced (contrast FL vs GC -> positive log2FC = induced by flight).
#
#  cax2-3 is included here as a QC re-test on the CORRECTED data (see
#  apex05_cax2_concordance_fixed.py); the primary manuscript uses Col-0, cax2-2,
#  rbohD.
#
#  OUTPUTS
#    results/tables/deseq2/apex05_deseq2_<geno>_<tissue>.csv   (full results)
#    results/tables/deseq2/apex05_deseq2_DEG_counts.csv        (summary)
#    results/ml/deseq2_flight_summary.json
#  RUN  python analysis/ml/apex05_deseq2_flight.py
#  DEPS pydeseq2
# =============================================================================

from __future__ import annotations
import json, warnings
from pathlib import Path

import numpy as np
import pandas as pd

import apex05_data as A
from pydeseq2.dds import DeseqDataSet
from pydeseq2.ds import DeseqStats
from pydeseq2.default_inference import DefaultInference

warnings.filterwarnings("ignore")
OUTDIR = A.REPO / "results/tables/deseq2"
OUTDIR.mkdir(parents=True, exist_ok=True)
PADJ, LFC = 0.05, 1.0


def run_group(counts: pd.DataFrame, cond: pd.Series, geno: str, tissue: str) -> dict:
    # pydeseq2 wants samples x genes, integer; drop all-zero genes for speed.
    X = counts.T
    X = X.loc[:, X.sum(axis=0) > 0]
    meta = pd.DataFrame({"condition": cond.values}, index=cond.index)
    inf = DefaultInference(n_cpus=4)
    dds = DeseqDataSet(counts=X, metadata=meta, design="~condition",
                       inference=inf, quiet=True)
    dds.deseq2()
    ds = DeseqStats(dds, contrast=["condition", "FL", "GC"], inference=inf, quiet=True)
    ds.summary()
    res = ds.results_df.sort_values("padj")
    res.index.name = "gene"
    slug = geno.replace("-", "").lower()
    res.to_csv(OUTDIR / f"apex05_deseq2_{slug}_{tissue}.csv")

    sig = res[(res.padj < PADJ) & (res.log2FoldChange.abs() > LFC)]
    up = int((sig.log2FoldChange > 0).sum())      # induced by flight
    dn = int((sig.log2FoldChange < 0).sum())      # repressed by flight
    print(f"  [{geno:7} {tissue:5}] tested={len(res):5}  DEG={len(sig):4} "
          f"(induced {up} / repressed {dn})")
    return {"genotype": geno, "tissue": tissue, "n_tested": int(len(res)),
            "n_DEG": int(len(sig)), "induced": up, "repressed": dn}


def main():
    print("=" * 66)
    print("APEX-05 | DESeq2 FL-vs-GC (fixed v2.2 matrix)  padj<0.05 |log2FC|>1")
    print("=" * 66)
    counts, sheet = A.load_raw()
    rows = []
    for geno in A.ALL_GENOTYPES:
        for tissue in A.TISSUES:
            c, cond = A.group(counts, sheet, geno, tissue)
            rows.append(run_group(c, cond, geno, tissue))
    summ = pd.DataFrame(rows)
    summ.to_csv(OUTDIR / "apex05_deseq2_DEG_counts.csv", index=False)
    with open(A.REPO / "results/ml/deseq2_flight_summary.json", "w") as fh:
        json.dump({"thresholds": {"padj": PADJ, "abs_log2FC": LFC},
                   "note": "flight-referenced: +log2FC = induced by flight; "
                           "cax2-3 included as QC re-test on corrected data",
                   "results": rows}, fh, indent=2)
    print("\nDEG counts:\n", summ.to_string(index=False))
    print(f"\nwrote per-group tables + summary to results/tables/deseq2/")


if __name__ == "__main__":
    main()
