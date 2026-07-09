#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Shared count-matrix loader (canonical per-sample data)
# =============================================================================
#
#  Single source of truth for the fixed, all-genotype gene-count matrix
#  (run1 v2.2 fix2). Parses the sample sheet from the column names, merges the
#  Col-0 technical replicate, and exposes raw counts / CPM per genotype x tissue.
#  Imported by the DESeq2, LASSO, concordance and deconvolution scripts so they
#  all agree on samples, labels and normalisation.
#
#  Column format:  {GENO}{TISSUE}{COND}_{rep}   e.g. CAX22ROOTFL_1
#    GENO  : COL00 -> Col-0 | CAX22 -> cax2-2 | CAX23 -> cax2-3 | RBOHD -> rbohD
#    TISSUE: ROOT | SHOOT      COND: FL (flight) | GC (ground control)
#
#  Col-0 note: reps are labelled 1,2,3,3.1. Despite the odd label, '3.1' is a
#  DISTINCT BIOLOGICAL replicate (the 4th Col-0 well): in log2CPM it correlates
#  with rep 3 at r = 0.993 — no more than any other rep pair (median 0.994), and
#  the ALSDA sample metadata lists four Col-0 wells (A1, B6, B9, C3). (Raw-count
#  Pearson r ~0.996 is NOT diagnostic — it is dominated by a few high-count genes
#  and is ~0.99 between any two libraries.) All four reps are therefore kept as
#  separate biological replicates: 64 samples, n = 4 per genotype × tissue × cond.
#
#  WELL / IMAGING BRIDGE: data/metadata/apex05_sample_well_metadata.csv (ALSDA)
#  carries the plate Location (well ID) per sample — the key the RSML root-imaging
#  data is indexed on. Each well appears in both FL and GC (position-paired
#  design), enabling paired/blocked statistics and RNA-seq <-> morphometrics
#  integration. Mapping the count-matrix rep numbers to well IDs is provisional
#  (see reconcile_wells()) and flagged for confirmation.
#
#  Directions are FLIGHT-REFERENCED downstream: contrast FL vs GC, so a positive
#  log2FC = induced by spaceflight.
# =============================================================================

from __future__ import annotations
import re
from pathlib import Path

import numpy as np
import pandas as pd

REPO = Path(__file__).resolve().parents[2]
CANON = REPO / "data/expression/counts_raw/apex05_gene_counts_all-genotypes_v2.2.csv"

WELL_META = REPO / "data/metadata/apex05_sample_well_metadata.csv"
GENO_MAP = {"COL00": "Col-0", "CAX22": "cax2-2", "CAX23": "cax2-3", "RBOHD": "rbohD"}
PRIMARY_GENOTYPES = ["Col-0", "cax2-2", "rbohD"]          # cax2-3 excluded from primary
ALL_GENOTYPES = ["Col-0", "cax2-2", "cax2-3", "rbohD"]
TISSUES = ["root", "shoot"]
_COLRE = re.compile(r"^(COL00|CAX22|CAX23|RBOHD)(ROOT|SHOOT)(FL|GC)_(\d+(?:\.\d+)?)$")


def _parse(col: str):
    m = _COLRE.match(col)
    if not m:
        return None
    geno, tissue, cond, rep = m.groups()
    return {"sample_id": col, "genotype": GENO_MAP[geno], "tissue": tissue.lower(),
            "condition": cond, "rep": rep}


def load_raw() -> tuple[pd.DataFrame, pd.DataFrame]:
    """Return (counts [genes x samples, AGI index], sample_sheet).

    All 64 columns are kept as separate biological replicates (Col-0 rep '3.1'
    is a distinct well, not a technical replicate — see module header).
    """
    df = pd.read_csv(CANON).rename(columns={"ID": "gene"}).set_index("gene")
    rows = [r for r in (_parse(c) for c in df.columns) if r]
    sheet = pd.DataFrame(rows).set_index("sample_id")
    sheet["lib_size"] = df[sheet.index].sum(axis=0).astype(int)
    sheet = _attach_wells(sheet)
    df = df[sheet.index]                        # keep only recognised sample columns
    return df.astype(int), sheet


def _attach_wells(sheet: pd.DataFrame) -> pd.DataFrame:
    """Attach plate Location (well), S-number and ALSDA sample name to each library.

    Mapping is DERIVED FROM THE S-NUMBERS in the ALSDA metadata: within each
    genotype the four wells have contiguous, staggered S-number blocks, so
    ordering the wells by their minimum S-number yields rep 1..4. Because the rep
    index is assigned per WELL, FL rep-i and GC rep-i resolve to the SAME well
    (the position-paired design), which is what enables paired DE and the
    RNA-seq <-> RSML imaging join.
    """
    sheet = sheet.copy()
    for col in ("location", "s_number", "alsda_sample"):
        sheet[col] = pd.NA
    if not WELL_META.exists():
        sheet["well_mapping"] = "unmapped"
        return sheet
    meta = pd.read_csv(WELL_META)
    meta["S"] = meta["Sample name"].str.extract(r"_S(\d+)").astype(int)
    meta["genotype"] = meta["Genotype"].map(GENO_MAP)
    meta["tissue"] = meta["Tissue"].str.lower()

    # (genotype, rep) -> well, via ascending minimum S-number per well
    rep_well = {}
    for g, sub in meta.groupby("genotype"):
        wells = sub.groupby("Location")["S"].min().sort_values().index.tolist()
        reps = sorted(sheet[sheet.genotype == g]["rep"].unique(), key=float)
        rep_well.update({(g, rep): w for rep, w in zip(reps, wells)})

    for sid, row in sheet.iterrows():
        well = rep_well.get((row.genotype, row.rep))
        sheet.loc[sid, "location"] = well
        mr = meta[(meta.Location == well) & (meta.tissue == row.tissue) &
                  (meta.Cond == row.condition)]
        if len(mr) == 1:
            sheet.loc[sid, "s_number"] = int(mr["S"].iloc[0])
            sheet.loc[sid, "alsda_sample"] = mr["Sample name"].iloc[0]
    sheet["well_mapping"] = "S-number-derived"
    return sheet


def group(counts: pd.DataFrame, sheet: pd.DataFrame, genotype: str, tissue: str):
    """Raw counts (genes x samples) + condition Series for one genotype x tissue."""
    sel = sheet[(sheet.genotype == genotype) & (sheet.tissue == tissue)]
    return counts[sel.index], sel["condition"]


def cpm(counts: pd.DataFrame) -> pd.DataFrame:
    """Counts-per-million (library-size normalised)."""
    return counts / counts.sum(axis=0) * 1e6


def log2cpm(counts: pd.DataFrame, prior: float = 1.0) -> pd.DataFrame:
    return np.log2(cpm(counts) + prior)


if __name__ == "__main__":
    counts, sheet = load_raw()
    out = REPO / "data/metadata/apex05_rnaseq_sample_sheet.csv"
    sheet.to_csv(out)
    print(f"canonical matrix: {counts.shape[0]} genes x {counts.shape[1]} samples")
    print(sheet.groupby(["genotype", "tissue", "condition"]).size().unstack(fill_value=0))
    print(f"\nwrote sample sheet -> {out.relative_to(REPO)}")
