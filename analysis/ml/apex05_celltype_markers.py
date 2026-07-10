#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Build Arabidopsis cell-type marker sets from PCMDB (reference)
# =============================================================================
#
#  Since APEX-05 is BULK RNA-seq, cell-type resolution is obtained by projecting
#  onto a PUBLISHED single-cell reference — here the Plant Cell Marker DataBase
#  (PCMDB; Jin et al., Nucleic Acids Research 2022; data DOI 10.5281/zenodo.5101271).
#  No cell-type assignment is invented: every marker→cell-type link is PCMDB's.
#
#  We build cell-type-SPECIFIC marker sets (a marker must map to a single cell
#  type) so downstream scoring/enrichment is interpretable:
#    * ROOT  cell types — from PCMDB 'Experimental' (curated) markers.
#    * LEAF/SHOOT vegetative cell types — from PCMDB 'SingleCellSeq' markers,
#      restricted to the leaf set and to genes specific within it.
#
#  INPUT   <scratch>/PCMDB_all_marker_info.xlsx  (downloaded from Zenodo 5101271)
#          — pass its path as argv[1], or place it next to the repo.
#  OUTPUT  data/genesets/pcmdb_arabidopsis_celltype_markers.csv
#          data/genesets/pcmdb_arabidopsis_celltype_markers.README.md
#  RUN  python analysis/ml/apex05_celltype_markers.py [path/to/PCMDB_all_marker_info.xlsx]
# =============================================================================

from __future__ import annotations
import sys, re
from pathlib import Path
import pandas as pd

import apex05_data as A
OUT = A.REPO / "data/genesets/pcmdb_arabidopsis_celltype_markers.csv"
MIN_ROOT, MIN_LEAF = 15, 20
LEAF_TYPES = ["leaf mesophyll", "leaf epidermis", "guard cell",
              "bundle sheath", "leaf pavement cell"]


def _find_workbook(argv) -> Path:
    if len(argv) > 1 and Path(argv[1]).exists():
        return Path(argv[1])
    for c in [A.REPO.parent / "PCMDB_all_marker_info.xlsx",
              Path.cwd() / "PCMDB_all_marker_info.xlsx",
              A.REPO / "PCMDB_all_marker_info.xlsx"]:
        if c.exists():
            return c
    raise SystemExit("PCMDB_all_marker_info.xlsx not found — download from "
                     "https://zenodo.org/records/5101271 and pass its path.")


def _celltype(df):
    df = df.copy()
    df["cell_type"] = df["third"].fillna(df["second"]).str.replace(r"PO:\d+-", "", regex=True)
    df["system"] = df["first"].apply(
        lambda s: "root" if "root" in str(s) else ("shoot" if "shoot" in str(s) else "other"))
    return df.dropna(subset=["Gene_id", "cell_type"])


def _specific(df):
    """Keep genes that mark exactly one cell type within the given frame."""
    n = df.groupby("Gene_id")["cell_type"].transform("nunique")
    return df[n == 1]


def main():
    wb = _find_workbook(sys.argv)
    print(f"reading {wb.name}")
    exp = _celltype(pd.read_excel(wb, sheet_name="Experimental")
                    .query("species_type == 'Arabidopsis thaliana'"))
    scs = _celltype(pd.read_excel(wb, sheet_name="SingleCellSeq")
                    .query("species_type == 'Arabidopsis thaliana'"))

    # ROOT — curated Experimental, specific within root cell types
    root = _specific(exp[exp.system == "root"])[["Gene_id", "cell_type"]].drop_duplicates()
    root["tissue"], root["source"] = "root", "PCMDB:Experimental"

    # LEAF/SHOOT — SingleCellSeq leaf types, specific within the leaf set
    leaf = scs[scs.cell_type.isin(LEAF_TYPES)]
    leaf = _specific(leaf)[["Gene_id", "cell_type"]].drop_duplicates()
    leaf["tissue"], leaf["source"] = "shoot", "PCMDB:SingleCellSeq"

    markers = pd.concat([root, leaf], ignore_index=True)
    markers = markers.rename(columns={"Gene_id": "gene"})
    # drop tiny cell-type sets
    keep = markers.groupby(["tissue", "cell_type"])["gene"].transform("size")
    thr = markers["tissue"].map({"root": MIN_ROOT, "shoot": MIN_LEAF})
    markers = markers[keep >= thr].sort_values(["tissue", "cell_type", "gene"])
    markers.to_csv(OUT, index=False)

    sizes = markers.groupby(["tissue", "cell_type"])["gene"].nunique().sort_values(ascending=False)
    print(f"\nwrote {OUT.relative_to(A.REPO)}: "
          f"{markers.gene.nunique()} genes, "
          f"{markers.groupby(['tissue','cell_type']).ngroups} cell-type sets")
    print(sizes.to_string())

    (OUT.with_suffix("")).with_name(OUT.stem + ".README.md").write_text(
        "# Arabidopsis cell-type marker sets (derived from PCMDB)\n\n"
        "Source: **Plant Cell Marker DataBase (PCMDB)**, Jin *et al.*, "
        "*Nucleic Acids Research* 2022; data DOI **10.5281/zenodo.5101271** "
        "(`PCMDB_all_maker_info.xlsx`).\n\n"
        "Built by `analysis/ml/apex05_celltype_markers.py`. Each marker maps to a "
        "single cell type (specificity-filtered). Root cell types come from the "
        "curated *Experimental* markers; leaf/shoot vegetative cell types from the "
        "*SingleCellSeq* markers restricted to the leaf set. Columns: `gene` (AGI), "
        "`cell_type`, `tissue`, `source`. Used for cell-type marker scoring and "
        "flight-DEG cell-type enrichment (`apex05_celltype_deconvolution.py`).\n",
        encoding="utf-8")
    print(f"wrote provenance README")


if __name__ == "__main__":
    main()
