#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | LASSO sparse flight signature — ML support for the DESeq2 statistics
# =============================================================================
#
#  Complements the DESeq2 differential-expression calls with an orthogonal,
#  multivariate view: an L1-penalised (LASSO) logistic model that selects a
#  MINIMAL set of genes jointly predictive of spaceflight (FL) vs ground control
#  (GC), per genotype x tissue, on the canonical fixed count matrix. If LASSO
#  independently converges on a compact subset of the DESeq2 DEGs and predicts
#  flight state under cross-validation, that is orthogonal support for the DE
#  signal.
#
#  Runs on the PRIMARY genotypes (Col-0, cax2-2, rbohD); cax2-3 is excluded.
#  Data via apex05_data (shared loader); DESeq2 DEG reference from
#  results/tables/deseq2/ (run apex05_deseq2_flight.py first).
#
#  SMALL-n honesty: 6-8 samples per group. We pick the most parsimonious penalty
#  C that still maximises leave-one-out accuracy, and report a STABILITY-SELECTED
#  signature (genes chosen in >= 75% of leave-one-out refits). Accuracy is LOO.
#
#  OUTPUTS  results/tables/apex05_lasso_<geno>_<tissue>_signature.csv
#           results/ml/figE1_lasso_flight_signature.png
#           results/ml/lasso_flight_summary.json
#  RUN  python analysis/ml/apex05_lasso_flight_signature.py
# =============================================================================

from __future__ import annotations
import json
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from scipy.stats import hypergeom
from sklearn.linear_model import LogisticRegression
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import cross_val_score, LeaveOneOut

import apex05_data as A
TBL = A.REPO / "results" / "tables"
DES = TBL / "deseq2"
OUT = A.REPO / "results" / "ml"
OUT.mkdir(parents=True, exist_ok=True)
RNG = 42
CS = np.logspace(-2.0, 1.0, 16)
MIN_MEAN_LOG2CPM = 1.0
STABILITY_MIN = 0.75
PADJ, LFC = 0.05, 1.0


def deseq_deg(geno: str, tissue: str) -> set:
    """DESeq2 DEG loci for the overlap test (AGI)."""
    slug = geno.replace("-", "").lower()
    f = DES / f"apex05_deseq2_{slug}_{tissue}.csv"
    if not f.exists():
        return set()
    d = pd.read_csv(f)
    d = d[(pd.to_numeric(d.padj, errors="coerce") < PADJ) &
          (pd.to_numeric(d.log2FoldChange, errors="coerce").abs() > LFC)]
    return set(d["gene"])


def symbol_map() -> dict:
    """AGI -> symbol from any DEG workbook that carries both (best-effort labels)."""
    out = {}
    for f in TBL.glob("apex5_*regulated-*.xlsx"):
        try:
            d = pd.read_excel(f)
        except Exception:
            continue
        d.columns = [c.strip() for c in d.columns]
        if "Locus ID" in d.columns and "Symbol" in d.columns:
            for _, r in d.iterrows():
                s = str(r["Symbol"])
                if s not in ("-", "nan", ""):
                    out.setdefault(str(r["Locus ID"]).strip(), s)
    return out


def fit_coef(X, y, C):
    return LogisticRegression(solver="saga", l1_ratio=1.0, C=C, max_iter=20000,
                              tol=1e-3, random_state=RNG).fit(X, y).coef_.ravel()


def best_C(X, y):
    best = (CS[-1], -1.0)
    for C in CS:
        acc = cross_val_score(LogisticRegression(solver="saga", l1_ratio=1.0, C=C,
                              max_iter=20000, tol=1e-3, random_state=RNG),
                              X, y, cv=LeaveOneOut()).mean()
        if acc > best[1] + 1e-9:
            best = (C, acc)
    return best


def analyse(geno: str, tissue: str, counts, sheet, symbols) -> dict | None:
    c, cond = A.group(counts, sheet, geno, tissue)
    if c.shape[1] < 4 or cond.nunique() < 2:
        return None
    logcpm = A.log2cpm(c)                       # genes x samples
    keep = logcpm.mean(axis=1) > MIN_MEAN_LOG2CPM
    genes = logcpm.index[keep].to_numpy()
    X = logcpm.loc[keep].to_numpy().T           # samples x genes
    y = (cond.values == "FL").astype(int)

    Xs = StandardScaler().fit_transform(X)
    C, loo = best_C(Xs, y)
    coef = fit_coef(Xs, y, C)

    freq = np.zeros(X.shape[1])
    for i in range(X.shape[0]):
        idx = np.arange(X.shape[0]) != i
        freq += fit_coef(StandardScaler().fit_transform(X[idx]), y[idx], C) != 0
    freq /= X.shape[0]
    stable = np.where(freq >= STABILITY_MIN)[0]

    deg = deseq_deg(geno, tissue)
    in_deg = np.array([g in deg for g in genes])
    sig_in_deg = int(in_deg[stable].sum())
    K = int(in_deg.sum())
    p_hyper = float(hypergeom.sf(sig_in_deg - 1, len(genes), K, len(stable))) \
        if K and len(stable) else None

    sig = pd.DataFrame({
        "genotype": geno, "tissue": tissue, "gene": genes[stable],
        "symbol": [symbols.get(g, "") for g in genes[stable]],
        "lasso_coef": coef[stable].round(4),
        "selection_freq": freq[stable].round(3),
        "in_deseq2_DEG": in_deg[stable],
    }).sort_values("lasso_coef", key=lambda s: s.abs(), ascending=False)
    slug = geno.replace("-", "").lower()
    sig.to_csv(TBL / f"apex05_lasso_{slug}_{tissue}_signature.csv", index=False)

    print(f"  [{geno:7} {tissue:5}] genes={len(genes):5} C={C:.3g} LOO-acc={loo:.2f} "
          f"signature={len(stable):3} in-DESeq2={sig_in_deg}/{len(stable)}"
          + (f" hyperg p={p_hyper:.1e}" if p_hyper is not None else " (no DEG ref)"))
    return {"genotype": geno, "tissue": tissue, "n_genes": int(len(genes)),
            "penalty_C": round(float(C), 4), "loo_accuracy": round(float(loo), 3),
            "signature_size": int(len(stable)), "deseq2_DEGs": K,
            "signature_in_deseq2": sig_in_deg,
            "overlap_frac": round(sig_in_deg / len(stable), 3) if len(stable) else None,
            "hypergeom_p": p_hyper, "_sig": sig}


def main():
    print("=" * 66)
    print("APEX-05 | LASSO flight signature (ML support for DESeq2)")
    print("=" * 66)
    counts, sheet = A.load_raw()
    symbols = symbol_map()
    results = []
    for geno in A.PRIMARY_GENOTYPES:
        for tissue in A.TISSUES:
            r = analyse(geno, tissue, counts, sheet, symbols)
            if r:
                results.append(r)

    n = len(results)
    ncol = 2
    nrow = int(np.ceil(n / ncol))
    fig, axes = plt.subplots(nrow, ncol, figsize=(11, 3.4 * nrow), squeeze=False)
    for ax, r in zip(axes.ravel(), results):
        sig = r["_sig"].head(15).iloc[::-1]
        colors = ["#0072B2" if v else "#BBBBBB" for v in sig["in_deseq2_DEG"]]
        labels = [s or g for s, g in zip(sig["symbol"], sig["gene"])]
        ax.barh(np.arange(len(sig)), sig["lasso_coef"], color=colors)
        ax.set_yticks(np.arange(len(sig)), labels, fontsize=6.5)
        ax.axvline(0, color="k", lw=.6)
        ax.set_title(f"{r['genotype']} {r['tissue']} — LOO {r['loo_accuracy']:.0%}, "
                     f"{r['signature_size']} genes ({r['signature_in_deseq2']} in DESeq2)", fontsize=8.5)
        ax.set_xlabel("LASSO coef (→ flight)", fontsize=8)
    for ax in axes.ravel()[n:]:
        ax.axis("off")
    fig.legend([plt.Rectangle((0, 0), 1, 1, color=c) for c in ("#0072B2", "#BBBBBB")],
               ["also a DESeq2 DEG", "LASSO-only"], ncol=2, loc="lower center",
               frameon=False, bbox_to_anchor=(0.5, -0.01))
    fig.suptitle("LASSO sparse flight signature corroborates the DESeq2 DEGs", y=1.0)
    fig.tight_layout(rect=(0, 0.03, 1, 0.99))
    fig.savefig(OUT / "figE1_lasso_flight_signature.png", dpi=300, bbox_inches="tight")
    plt.close(fig)
    print("\n  wrote results/ml/figE1_lasso_flight_signature.png")

    with open(OUT / "lasso_flight_summary.json", "w") as fh:
        json.dump({"config": {"min_mean_log2cpm": MIN_MEAN_LOG2CPM,
                              "stability_min_fold_frac": STABILITY_MIN, "cv": "leave-one-out"},
                   "results": [{k: v for k, v in r.items() if not k.startswith("_")} for r in results]},
                  fh, indent=2)
    print("  wrote results/ml/lasso_flight_summary.json\nDone.")


if __name__ == "__main__":
    main()
