#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Functional enrichment of the CAX2 concordant-core flight signature
# =============================================================================
#
#  Over-representation analysis (GO, KEGG, Reactome, WikiPathways) of the
#  CAX2 concordant core — the genes differentially expressed under spaceflight
#  in BOTH CAX2 alleles with agreeing direction (built by
#  apex05_cax2_concordant_core.py).
#
#  METHOD
#  ------
#  * Tool: g:Profiler g:GOSt (organism = Arabidopsis thaliana), which applies
#    its g:SCS multiple-testing correction; we keep terms with corrected p < 0.05.
#  * Background: a CUSTOM universe of genes actually *tested* for differential
#    expression (loci with a non-missing padj in the full FL-vs-GC contrast
#    tables), tissue-specific. Using the tested transcriptome — not the whole
#    genome — as background is what makes the enrichment statistically honest.
#  * Queries: root core, shoot core, and each split by flight direction (up/down).
#
#  REQUIRES internet access (queries the g:Profiler API) and gprofiler-official.
#  INPUTS   results/tables/apex05_cax2_concordant-core_{root,shoot}.csv
#           archive/excluded_cax2-3/apex05_cax23_{root,shoot}_fl-vs-gc_full.csv (background)
#  OUTPUTS  results/tables/apex05_cax2_core_enrichment_{root,shoot}.csv
#           results/tables/apex05_cax2_core_enrichment_all.csv
#           results/ml/figC4_cax2_core_enrichment.png
#           results/ml/cax2_core_enrichment_summary.json
#  RUN      python analysis/ml/apex05_cax2_core_enrichment.py
# =============================================================================

from __future__ import annotations
import json, re
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from gprofiler import GProfiler

REPO = Path(__file__).resolve().parents[2]
TBL = REPO / "results" / "tables"
OUT = REPO / "results" / "ml"
OUT.mkdir(parents=True, exist_ok=True)
SOURCES = ["GO:BP", "GO:MF", "GO:CC", "KEGG", "REAC", "WP"]
SRC_COLORS = {"GO:BP": "#0072B2", "GO:MF": "#56B4E9", "GO:CC": "#009E73",
              "KEGG": "#D55E00", "REAC": "#CC79A7", "WP": "#E69F00"}
gp = GProfiler(return_dataframe=True)


def _locus(x) -> str:
    return re.sub(r"\.\d+$", "", str(x).strip())


def tested_universe(tissue: str) -> list[str]:
    """Loci with a non-missing padj in the full contrast table = tested genes."""
    d = pd.read_csv(REPO / f"archive/excluded_cax2-3/apex05_cax23_{tissue}_fl-vs-gc_full.csv")
    d.columns = [c.strip().lstrip("﻿") for c in d.columns]
    d["padj"] = pd.to_numeric(d["padj"], errors="coerce")
    loci = {_locus(v) for v in d.loc[d["padj"].notna(), "ID"]}
    return sorted(loci)


def enrich(query: list[str], background: list[str], label: str) -> pd.DataFrame:
    if len(query) < 3:
        print(f"  [{label}] only {len(query)} genes — skipped")
        return pd.DataFrame()
    res = gp.profile(organism="athaliana", query=query, sources=SOURCES,
                     user_threshold=0.05, significance_threshold_method="g_SCS",
                     background=background, domain_scope="custom",
                     no_evidences=True)
    if res.empty:
        print(f"  [{label}] no significant terms")
        return res
    res = res.assign(query_label=label).sort_values("p_value")
    print(f"  [{label}] {len(res)} significant terms "
          f"(top: {res.iloc[0]['source']} — {res.iloc[0]['name']}, "
          f"p={res.iloc[0]['p_value']:.1e})")
    return res


def main():
    print("=" * 66)
    print("APEX-05 | CAX2 concordant-core functional enrichment (g:Profiler)")
    print("=" * 66)
    keep = ["query_label", "source", "native", "name", "p_value",
            "term_size", "query_size", "intersection_size"]
    all_res = []

    for tissue in ("root", "shoot"):
        core = pd.read_csv(TBL / f"apex05_cax2_concordant-core_{tissue}.csv")
        bg = tested_universe(tissue)
        print(f"\n[{tissue}] core={len(core)} genes | background={len(bg)} tested loci")
        genes_all = core["locus"].tolist()
        genes_up = core.loc[core["flight_direction"] == "up", "locus"].tolist()
        genes_dn = core.loc[core["flight_direction"] == "down", "locus"].tolist()

        parts = [enrich(genes_all, bg, f"{tissue}:all"),
                 enrich(genes_up, bg, f"{tissue}:up"),
                 enrich(genes_dn, bg, f"{tissue}:down")]
        tissue_res = pd.concat([p for p in parts if not p.empty], ignore_index=True) \
            if any(not p.empty for p in parts) else pd.DataFrame(columns=keep)
        if not tissue_res.empty:
            tissue_res[keep].to_csv(
                TBL / f"apex05_cax2_core_enrichment_{tissue}.csv", index=False)
            print(f"  wrote results/tables/apex05_cax2_core_enrichment_{tissue}.csv")
            all_res.append(tissue_res)

    if not all_res:
        print("\nNo significant enrichment found in any list.")
        return
    full = pd.concat(all_res, ignore_index=True)
    full[keep].to_csv(TBL / "apex05_cax2_core_enrichment_all.csv", index=False)

    # ---- Figure: top terms per tissue (the 'all' query) ----
    fig, axes = plt.subplots(1, 2, figsize=(12, 5.2))
    for ax, tissue in zip(axes, ("root", "shoot")):
        sub = full[full["query_label"] == f"{tissue}:all"].copy()
        if sub.empty:
            ax.set_title(f"{tissue}: no significant terms"); ax.axis("off"); continue
        sub = sub.nsmallest(12, "p_value").iloc[::-1]
        y = np.arange(len(sub))
        ax.barh(y, -np.log10(sub["p_value"]),
                color=[SRC_COLORS.get(s, "#888888") for s in sub["source"]])
        ax.set_yticks(y, [f"{n[:46]}" for n in sub["name"]], fontsize=8)
        ax.set_xlabel(r"$-\log_{10}$ adjusted $p$")
        ax.set_title(f"{tissue} core (n={ (full['query_label']==f'{tissue}:all').sum() } terms)")
    handles = [plt.Rectangle((0, 0), 1, 1, color=c) for c in SRC_COLORS.values()]
    fig.legend(handles, SRC_COLORS.keys(), ncol=6, loc="lower center",
               frameon=False, bbox_to_anchor=(0.5, -0.03))
    fig.suptitle("Functional enrichment of the CAX2 concordant-core flight signature", y=1.0)
    fig.tight_layout(rect=(0, 0.03, 1, 1))
    fig.savefig(OUT / "figC4_cax2_core_enrichment.png", dpi=300, bbox_inches="tight")
    plt.close(fig)
    print("  wrote results/ml/figC4_cax2_core_enrichment.png")

    # ---- summary json ----
    summ = {"sources": SOURCES, "threshold": "g_SCS < 0.05", "by_query": {}}
    for lab, g in full.groupby("query_label"):
        summ["by_query"][lab] = {
            "n_terms": int(len(g)),
            "top_terms": g.nsmallest(8, "p_value")[["source", "native", "name", "p_value"]]
                          .assign(p_value=lambda d: d["p_value"].map(lambda v: float(f"{v:.3e}")))
                          .to_dict("records"),
        }
    with open(OUT / "cax2_core_enrichment_summary.json", "w") as fh:
        json.dump(summ, fh, indent=2)
    print("  wrote results/ml/cax2_core_enrichment_summary.json")
    print("\nDone.")


if __name__ == "__main__":
    main()
