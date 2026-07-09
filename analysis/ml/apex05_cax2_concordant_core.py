#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | CAX2 concordant-core flight signature (cax2-2 ∩ cax2-3, same dir.)
# =============================================================================
#
#  PURPOSE
#  -------
#  cax2-3 was excluded from the primary analysis because its spaceflight DEG set
#  is grossly inflated and no more similar to its sibling allele cax2-2 than to
#  unrelated genotypes (see docs/cax2_allele_concordance.md). However, a *small*
#  core of genes is differentially expressed in BOTH CAX2 alleles with AGREEING
#  fold-change direction. That core is the **high-confidence CAX2 spaceflight
#  signature** — the part of the cax2-3 signal corroborated by cax2-2 — and is a
#  defensible, conservative alternative to discarding cax2-3 entirely.
#
#  This script extracts and annotates that concordant core.
#
#  DIRECTION HANDLING (no guessing)
#  --------------------------------
#  * cax2-2 flight fold-change is computed from the workbook's EXPLICITLY LABELLED
#    columns  log2( baseMeanA_FL / baseMeanB_GC )  -> unambiguous FL-vs-GC sign.
#  * cax2-3's full table reports log2FoldChange = log2(B/A) with A/B unlabelled.
#    Its flight orientation is resolved EMPIRICALLY: we pick the sign that
#    maximises agreement with cax2-2 on shared DEGs, and report which was chosen
#    and the resulting agreement. (The core is, by construction, the agreeing set.)
#
#  INPUTS
#    results/tables/apex5_cax22_{root,shoot}_{up,down}-regulated-*.xlsx   (cax2-2)
#    archive/excluded_cax2-3/apex05_cax23_{root,shoot}_fl-vs-gc_full.csv  (cax2-3)
#  OUTPUTS
#    results/tables/apex05_cax2_concordant-core_{root,shoot}.csv
#    results/tables/apex05_cax2_concordant-core_combined.csv
#    results/ml/figC3_cax2_concordant_core.png
#    results/ml/cax2_concordant_core_summary.json
#  RUN   python analysis/ml/apex05_cax2_concordant_core.py
# =============================================================================

from __future__ import annotations
import json, re
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

REPO = Path(__file__).resolve().parents[2]
TBL = REPO / "results" / "tables"
OUT = REPO / "results" / "ml"
OUT.mkdir(parents=True, exist_ok=True)
PADJ, LFC, PC = 0.05, 1.0, 1.0          # DE thresholds; pseudocount for ratios
WB = {"root": (100, 223), "shoot": (45, 185)}   # up/down workbook suffixes


def _locus(x) -> str:
    return re.sub(r"\.\d+$", "", str(x).strip())


def load_cax22(tissue: str) -> pd.DataFrame:
    """cax2-2 DEGs with a true FL-vs-GC log2FC from labelled baseMean columns."""
    up, dn = WB[tissue]
    frames = []
    for tag, n in (("up", up), ("down", dn)):
        d = pd.read_excel(TBL / f"apex5_cax22_{tissue}_{tag}-regulated-{n}.xlsx")
        d.columns = [c.strip() for c in d.columns]
        fl = [c for c in d.columns if c.endswith("_FL")][0]
        gc = [c for c in d.columns if c.endswith("_GC")][0]
        d["locus"] = d["Transcript ID"].map(_locus)
        d["cax22_flight_log2FC"] = np.log2((d[fl] + PC) / (d[gc] + PC))
        d["cax22_workbook_call"] = tag
        frames.append(d[["locus", "Symbol", "Transcript ID",
                         "cax22_flight_log2FC", "cax22_workbook_call"]])
    out = pd.concat(frames, ignore_index=True)
    # one row per locus (a locus can appear via multiple transcripts)
    return (out.sort_values("cax22_flight_log2FC", key=lambda s: s.abs(), ascending=False)
               .drop_duplicates("locus"))


def load_cax23(tissue: str) -> pd.DataFrame:
    """cax2-3 DEGs (thresholded) with raw log2FoldChange = log2(B/A)."""
    d = pd.read_csv(REPO / f"archive/excluded_cax2-3/apex05_cax23_{tissue}_fl-vs-gc_full.csv")
    d.columns = [c.strip().lstrip("﻿") for c in d.columns]
    for c in ("log2FoldChange", "padj"):
        d[c] = pd.to_numeric(d[c], errors="coerce")
    d["locus"] = d["ID"].map(_locus)
    deg = d[(d["padj"] < PADJ) & (d["log2FoldChange"].abs() > LFC)].dropna(subset=["log2FoldChange"])
    # collapse transcripts -> locus (mean log2FC, min padj)
    return (deg.groupby("locus")
               .agg(cax23_raw_log2FC=("log2FoldChange", "mean"),
                    cax23_padj=("padj", "min"))
               .reset_index())


def analyse(tissue: str) -> tuple[pd.DataFrame, dict]:
    c22, c23 = load_cax22(tissue), load_cax23(tissue)
    shared = c22.merge(c23, on="locus", how="inner")

    # Resolve cax2-3 flight orientation against cax2-2 ground truth.
    s22 = np.sign(shared["cax22_flight_log2FC"])
    agree_pos = (s22 == np.sign(shared["cax23_raw_log2FC"])).mean()
    orient = 1 if agree_pos >= 0.5 else -1
    shared["cax23_flight_log2FC"] = orient * shared["cax23_raw_log2FC"]
    shared["concordant"] = np.sign(shared["cax22_flight_log2FC"]) == np.sign(shared["cax23_flight_log2FC"])
    agreement = float(shared["concordant"].mean())

    core = shared[shared["concordant"]].copy()
    core["flight_direction"] = np.where(core["cax22_flight_log2FC"] > 0, "up", "down")
    core = core[["locus", "Symbol", "Transcript ID", "flight_direction",
                 "cax22_flight_log2FC", "cax23_flight_log2FC", "cax23_padj"]] \
        .rename(columns={"Transcript ID": "transcript_id"}) \
        .sort_values(["flight_direction", "cax22_flight_log2FC"],
                     key=lambda s: s.abs() if s.name == "cax22_flight_log2FC" else s,
                     ascending=[True, False])
    core.insert(0, "tissue", tissue)

    summ = {
        "cax2-2_DEGs": int(len(c22)), "cax2-3_DEGs_thresholded": int(len(c23)),
        "shared_DEGs": int(len(shared)),
        "cax2-3_orientation_vs_cax2-2": "same" if orient == 1 else "flipped",
        "direction_agreement_on_shared": round(agreement, 3),
        "concordant_core_size": int(len(core)),
        "core_up_in_flight": int((core["flight_direction"] == "up").sum()),
        "core_down_in_flight": int((core["flight_direction"] == "down").sum()),
    }
    print(f"[{tissue}] shared={len(shared)}  core={len(core)} "
          f"(up {summ['core_up_in_flight']} / down {summ['core_down_in_flight']}); "
          f"cax2-3 orientation {summ['cax2-3_orientation_vs_cax2-2']}; "
          f"agreement {agreement:.0%}")
    return core, {"summary": summ, "_shared": shared}


def main():
    print("=" * 66)
    print("APEX-05 | CAX2 concordant-core flight signature")
    print("=" * 66)
    cores, meta = {}, {}
    for t in ("root", "shoot"):
        cores[t], meta[t] = analyse(t)

    # ---- write per-tissue + combined tables ----
    for t in ("root", "shoot"):
        cores[t].to_csv(TBL / f"apex05_cax2_concordant-core_{t}.csv", index=False)
        print(f"  wrote results/tables/apex05_cax2_concordant-core_{t}.csv")
    combined = pd.concat([cores["root"], cores["shoot"]], ignore_index=True)
    combined.to_csv(TBL / "apex05_cax2_concordant-core_combined.csv", index=False)
    root_loci, shoot_loci = set(cores["root"].locus), set(cores["shoot"].locus)
    shared_both = sorted(root_loci & shoot_loci)
    print(f"  wrote results/tables/apex05_cax2_concordant-core_combined.csv "
          f"({len(combined)} rows; {len(shared_both)} loci in BOTH tissues)")

    # ---- figure: cax2-2 vs cax2-3 flight log2FC per tissue ----
    fig, axes = plt.subplots(1, 2, figsize=(10, 4.6))
    for ax, t in zip(axes, ("root", "shoot")):
        sh = meta[t]["_shared"]
        con = sh["concordant"].to_numpy()
        ax.axhline(0, color="grey", lw=0.6); ax.axvline(0, color="grey", lw=0.6)
        ax.scatter(sh.loc[~con, "cax22_flight_log2FC"], sh.loc[~con, "cax23_flight_log2FC"],
                   s=18, color="#BBBBBB", alpha=0.7, label="discordant")
        ax.scatter(sh.loc[con, "cax22_flight_log2FC"], sh.loc[con, "cax23_flight_log2FC"],
                   s=22, color="#D55E00", alpha=0.85, label="concordant core")
        lim = np.nanmax(np.abs([sh["cax22_flight_log2FC"], sh["cax23_flight_log2FC"]])) * 1.05
        ax.plot([-lim, lim], [-lim, lim], ls="--", color="#0072B2", lw=0.8)
        ax.set_xlim(-lim, lim); ax.set_ylim(-lim, lim)
        ax.set_xlabel("cax2-2 flight log2FC (FL/GC)")
        ax.set_ylabel("cax2-3 flight log2FC (oriented)")
        ax.set_title(f"{t}: {int(con.sum())}/{len(con)} shared DEGs concordant")
        ax.legend(frameon=False, fontsize=8, loc="upper left")
    fig.suptitle("CAX2 concordant core: genes flight-responsive in BOTH alleles, same direction", y=1.02)
    fig.savefig(OUT / "figC3_cax2_concordant_core.png", dpi=300, bbox_inches="tight")
    plt.close(fig)
    print("  wrote results/ml/figC3_cax2_concordant_core.png")

    # ---- summary json ----
    out = {"thresholds": {"padj": PADJ, "abs_log2FC": LFC, "pseudocount": PC}}
    for t in ("root", "shoot"):
        out[t] = meta[t]["summary"]
    out["loci_in_both_tissues"] = shared_both
    with open(OUT / "cax2_concordant_core_summary.json", "w") as fh:
        json.dump(out, fh, indent=2)
    print("  wrote results/ml/cax2_concordant_core_summary.json")
    print("\nDone.")


if __name__ == "__main__":
    main()
