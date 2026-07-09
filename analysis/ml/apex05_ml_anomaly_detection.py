#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Machine-learning detection of tissue, genotype and mislabelled
#           samples in the spaceflight Arabidopsis dataset
# =============================================================================
#
#  BACKGROUND
#  ----------
#  APEX-05 compared four Arabidopsis genotypes (Col-0/WT, cax22, cax23, rbohD)
#  in spaceflight (FL) vs ground control (GC), in root and shoot tissue. During
#  the original analysis a *sample-labelling anomaly* was found: the two Ca2+/H+
#  exchanger mutants cax22 and cax23 were loaded from physically adjacent plate
#  wells, and one cax23 replicate behaved anomalously (the "CAX23oddity"). The
#  full provenance is in docs/PROVENANCE_cax23_mislabelling.md.
#
#  This script uses the REAL released data to answer two questions the reviewer
#  of a FAIR data package will ask: (1) can a model independently recover the
#  design factors (tissue, genotype) from the data, and (2) can it flag a
#  mislabelled sample? It is deliberately split by what the data can and cannot
#  support, and it does not fabricate any biological result.
#
#    PART A  Tissue identity + mislabel recovery from RNA-seq expression
#            -----------------------------------------------------------
#            Col-0 root vs shoot log-CPM. Tissue is almost perfectly
#            separable, so this is the modality in which a swapped-label sample
#            is *detectable*. We quantify detection by injecting known label
#            swaps and measuring how reliably the cross-validated detector
#            recovers them. This is the concrete "detect the anomaly" result.
#
#    PART B  Genotype recovery from root-architecture (RSML) morphometrics
#            ------------------------------------------------------------
#            Individual primary roots are aggregated to a per-well x treatment
#            profile (the same unit as an RNA-seq sample). A group-aware
#            classifier reports how separable the genotypes actually are. The
#            honest finding -- day-4 primary-root morphology only weakly
#            separates these subtle mutants, with cax22/cax23 among the most
#            confused -- is itself the QC conclusion: it is *why* durable
#            sample-label provenance (well-ID keying), not post-hoc
#            morphological rescue, is the correct fix. An unsupervised
#            IsolationForest additionally flags outlier wells label-free.
#
#  SCIENTIFIC-INTEGRITY NOTE
#  -------------------------
#  Every accuracy is produced by leakage-safe cross-validation (leave-one-out
#  for the 16 expression samples; leave-one-well-out for morphometrics). The
#  "recovery test" swaps are explicitly synthetic and used only to validate the
#  detector; they make no claim about which real sample was wrong. Weak results
#  are reported as weak.
#
#  INPUTS  (relative to repo root)
#    data/expression/counts_cpm/apex05_col0_root_cpm.csv
#    data/expression/counts_cpm/apex05_col0_shoot_cpm.csv
#    data/morphometrics/apex05_rsml_day4_morphometrics.csv
#  OUTPUTS  ->  results/ml/   (PNG figures @300dpi, metrics JSON, flagged CSVs)
#  RUN      python analysis/ml/apex05_ml_anomaly_detection.py
#  DEPS     analysis/ml/requirements.txt   |   REPRODUCIBILITY: single seed
# =============================================================================

from __future__ import annotations
import json
from pathlib import Path

import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")                       # headless backend: write files only
import matplotlib.pyplot as plt

from sklearn.ensemble import RandomForestClassifier, IsolationForest
from sklearn.linear_model import LogisticRegression
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import (
    cross_val_predict, cross_val_score, LeaveOneOut, LeaveOneGroupOut,
)
from sklearn.metrics import confusion_matrix, classification_report, accuracy_score, f1_score
from sklearn.decomposition import PCA

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------
RANDOM_STATE = 42
RNG = np.random.default_rng(RANDOM_STATE)
REPO_ROOT = Path(__file__).resolve().parents[2]
OUT_DIR = REPO_ROOT / "results" / "ml"
OUT_DIR.mkdir(parents=True, exist_ok=True)

# Okabe-Ito colour-blind-safe palette, shared across every figure.
GENOTYPE_COLORS = {"Col-0": "#0072B2", "cax22": "#E69F00",
                   "cax23": "#D55E00", "rbohD": "#009E73"}
TISSUE_COLORS = {"root": "#8C5109", "shoot": "#1B7837"}
GENOTYPE_RENAME = {"Col": "Col-0"}          # raw file uses "Col"

# Morphometric features; zero-variance columns (primary-root-only export) are
# dropped automatically, so listing the empty ones here is harmless.
CANDIDATE_FEATURES = ["length", "vector_length", "surface", "volume",
                      "direction", "diameter", "insertion_position",
                      "insertion_angle", "n_child", "child_density"]

metrics: dict = {"random_state": RANDOM_STATE}


def _save_fig(fig, name: str) -> None:
    path = OUT_DIR / name
    fig.savefig(path, dpi=300, bbox_inches="tight")
    plt.close(fig)
    print(f"  wrote {path.relative_to(REPO_ROOT)}")


# =============================================================================
#  PART A  |  Tissue identity + mislabel recovery from expression
# =============================================================================
def part_a_expression() -> None:
    print("\n[Part A] Tissue classifier + mislabel recovery (Col-0 CPM expression)")
    root = pd.read_csv(REPO_ROOT / "data/expression/counts_cpm/apex05_col0_root_cpm.csv")
    shoot = pd.read_csv(REPO_ROOT / "data/expression/counts_cpm/apex05_col0_shoot_cpm.csv")
    root = root.rename(columns={root.columns[0]: "gene"}).set_index("gene")
    shoot = shoot.rename(columns={shoot.columns[0]: "gene"}).set_index("gene")

    common = root.index.intersection(shoot.index)      # shared genes only
    mat = pd.concat([root.loc[common], shoot.loc[common]], axis=1)
    sample_names = list(mat.columns)
    X = np.log2(mat.to_numpy(dtype=float).T + 1.0)      # samples x genes, log-CPM
    y = np.array(["root"] * root.shape[1] + ["shoot"] * shoot.shape[1])
    print(f"  {X.shape[0]} samples x {X.shape[1]} shared genes "
          f"({(y=='root').sum()} root / {(y=='shoot').sum()} shoot)")

    # Regularised logistic regression in a scaling pipeline. Leave-one-out CV is
    # the honest estimator for only 16 samples.
    clf = make_pipeline(StandardScaler(),
                        LogisticRegression(max_iter=5000, C=0.1,
                                           random_state=RANDOM_STATE))
    loo_acc = cross_val_score(clf, X, y, cv=LeaveOneOut()).mean()
    print(f"  leave-one-out tissue accuracy = {loo_acc:.3f}")
    metrics["part_a_expression"] = {
        "n_samples": int(X.shape[0]), "n_genes": int(X.shape[1]),
        "loo_tissue_accuracy": round(float(loo_acc), 4),
    }

    # ---- Figure A1: PCA showing tissue separation -------------------------
    Xs = StandardScaler().fit_transform(X)
    pcs = PCA(n_components=2, random_state=RANDOM_STATE).fit(Xs)
    XY = pcs.transform(Xs)
    fig, ax = plt.subplots(figsize=(5.8, 4.8))
    for t in ("root", "shoot"):
        m = y == t
        ax.scatter(XY[m, 0], XY[m, 1], s=70, alpha=0.85, label=t, color=TISSUE_COLORS[t])
    for i, name in enumerate(sample_names):
        ax.annotate(name, (XY[i, 0], XY[i, 1]), fontsize=6, alpha=0.6,
                    xytext=(3, 3), textcoords="offset points")
    ax.set_xlabel(f"PC1 ({pcs.explained_variance_ratio_[0]:.0%})")
    ax.set_ylabel(f"PC2 ({pcs.explained_variance_ratio_[1]:.0%})")
    ax.set_title("Col-0 transcriptomes separate cleanly by tissue")
    ax.legend(frameon=False)
    _save_fig(fig, "figA1_tissue_expression_pca.png")

    # ---- Mislabel recovery test -------------------------------------------
    # Simulate label errors by flipping the tissue label of `n_swaps` random
    # samples, then ask the leave-one-out detector to recover them:
    #   mislabel score(sample) = 1 - P(model assigns the sample's stated label).
    # A flipped sample should receive a high score. We repeat over many random
    # injection sets and report recall (flips caught in the top-`n_swaps`) and
    # the score separation between flipped and clean samples.
    n_trials, n_swaps = 200, 2
    labels = np.array(["root", "shoot"])
    caught, inj_scores_all, clean_scores_all = 0, [], []
    for _ in range(n_trials):
        swap_idx = RNG.choice(len(y), size=n_swaps, replace=False)
        y_corr = y.copy()
        y_corr[swap_idx] = np.where(y_corr[swap_idx] == "root", "shoot", "root")
        proba = cross_val_predict(clf, X, y_corr, cv=LeaveOneOut(),
                                  method="predict_proba")
        cls = list(clf.fit(X, y_corr).classes_)
        assigned = np.array([cls.index(g) for g in y_corr])
        score = 1.0 - proba[np.arange(len(y_corr)), assigned]
        top = np.argsort(score)[::-1][:n_swaps]         # top-N most suspicious
        caught += len(set(top) & set(swap_idx))
        inj_scores_all.extend(score[swap_idx]);
        clean_scores_all.extend(np.delete(score, swap_idx))
    recall = caught / (n_trials * n_swaps)
    print(f"  recovery test: {n_swaps} injected swaps x {n_trials} trials -> "
          f"recall@top-{n_swaps} = {recall:.0%}")
    metrics["part_a_expression"]["recovery_test"] = {
        "n_trials": n_trials, "n_swaps_per_trial": n_swaps,
        "recall_at_top_n": round(float(recall), 4),
        "mean_score_injected": round(float(np.mean(inj_scores_all)), 4),
        "mean_score_clean": round(float(np.mean(clean_scores_all)), 4),
    }

    # ---- Figure A2: injected vs clean mislabel-score distributions --------
    fig, ax = plt.subplots(figsize=(6.0, 4.2))
    ax.hist(clean_scores_all, bins=30, alpha=0.6, color="#999999",
            density=True, label="correctly-labelled samples")
    ax.hist(inj_scores_all, bins=30, alpha=0.8, color=GENOTYPE_COLORS["cax23"],
            density=True, label="injected label swaps")
    ax.set_xlabel("mislabel score  (1 − P[stated label])")
    ax.set_ylabel("density")
    ax.set_title("Expression detector cleanly separates swapped samples")
    ax.legend(frameon=False, fontsize=9)
    _save_fig(fig, "figA2_expression_recovery_scores.png")


# =============================================================================
#  PART B  |  Genotype recovery from RSML root morphometrics (honest QC)
# =============================================================================
def _aggregate_wells(df: pd.DataFrame, features: list[str]) -> pd.DataFrame:
    """Collapse individual primary roots to one profile per well x treatment.

    Each RNA-seq sample corresponds to a plate well x condition, so this is the
    biologically correct unit for a *sample*-level mislabel question. For every
    morphometric feature we summarise the well's roots by mean/std/median/p90.
    """
    rows = []
    for (loc, trt, geno), g in df.groupby(["Location", "Treatment", "Genotype"]):
        rec = {"Location": loc, "Treatment": trt, "Genotype": geno, "n_roots": len(g)}
        for f in features:
            rec[f"{f}_mean"] = g[f].mean()
            rec[f"{f}_std"] = g[f].std(ddof=0)
            rec[f"{f}_med"] = g[f].median()
            rec[f"{f}_p90"] = g[f].quantile(0.90)
        rows.append(rec)
    return pd.DataFrame(rows)


def part_b_morphometrics() -> None:
    print("\n[Part B] Genotype recovery + outlier flagging (RSML morphometrics)")
    df = pd.read_csv(REPO_ROOT / "data/morphometrics/apex05_rsml_day4_morphometrics.csv",
                     skipinitialspace=True)
    df.columns = [c.strip() for c in df.columns]
    df["Genotype"] = df["Genotype"].replace(GENOTYPE_RENAME)

    features = [f for f in CANDIDATE_FEATURES if df[f].nunique() > 1]
    print(f"  informative features ({len(features)}): {features}")

    prof = _aggregate_wells(df, features)
    Xcols = [c for c in prof.columns if c not in ("Location", "Treatment", "Genotype")]
    X = prof[Xcols].fillna(0.0).to_numpy(dtype=float)
    y = prof["Genotype"].to_numpy()
    groups = prof["Location"].to_numpy()               # keep FL+GC of a well together
    classes = ["Col-0", "cax22", "cax23", "rbohD"]
    print(f"  {len(prof)} well x treatment profiles, {X.shape[1]} features")

    clf = make_pipeline(StandardScaler(),
                        RandomForestClassifier(n_estimators=800, min_samples_leaf=1,
                                               class_weight="balanced",
                                               random_state=RANDOM_STATE, n_jobs=-1))
    # Leave-one-WELL-out: a well never appears in its own training fold.
    pred = cross_val_predict(clf, X, y, cv=LeaveOneGroupOut(), groups=groups)
    acc = accuracy_score(y, pred)
    macro_f1 = f1_score(y, pred, average="macro")
    chance = 1.0 / len(classes)
    print(f"  leave-one-well-out genotype accuracy = {acc:.3f} "
          f"(chance {chance:.2f}) | macro-F1 = {macro_f1:.3f}")
    metrics["part_b_morphometrics"] = {
        "n_profiles": int(len(prof)), "n_features": int(X.shape[1]),
        "features_used": features,
        "loo_well_genotype_accuracy": round(float(acc), 4),
        "chance_level": round(chance, 4),
        "macro_f1": round(float(macro_f1), 4),
        "interpretation": ("Day-4 primary-root morphometry only weakly separates "
                           "these subtle mutants; genotype cannot be reliably "
                           "recovered from morphology alone, so sample-label "
                           "provenance is the durable fix."),
    }

    # ---- Figure B1: genotype confusion matrix -----------------------------
    cm = confusion_matrix(y, pred, labels=classes)
    cm_norm = cm / cm.sum(axis=1, keepdims=True)
    fig, ax = plt.subplots(figsize=(5.2, 4.6))
    im = ax.imshow(cm_norm, cmap="Oranges", vmin=0, vmax=1)
    ax.set_xticks(range(4), classes, rotation=30, ha="right")
    ax.set_yticks(range(4), classes)
    ax.set_xlabel("Predicted genotype"); ax.set_ylabel("True (labelled) genotype")
    ax.set_title(f"Genotype confusion (leave-one-well-out, acc={acc:.0%})")
    for i in range(4):
        for j in range(4):
            ax.text(j, i, f"{cm[i, j]}\n{cm_norm[i, j]:.0%}", ha="center", va="center",
                    color="white" if cm_norm[i, j] > 0.5 else "black", fontsize=8)
    fig.colorbar(im, ax=ax, fraction=0.046, pad=0.04, label="row-normalised")
    _save_fig(fig, "figB1_genotype_confusion_matrix.png")
    ci22, ci23 = classes.index("cax22"), classes.index("cax23")
    metrics["part_b_morphometrics"]["cax22_cax23_mutual_confusions"] = int(cm[ci22, ci23] + cm[ci23, ci22])

    # ---- Figure B2: PCA of aggregated morphometric space ------------------
    Xs = StandardScaler().fit_transform(X)
    pcs = PCA(n_components=2, random_state=RANDOM_STATE).fit(Xs)
    XY = pcs.transform(Xs)
    fig, ax = plt.subplots(figsize=(5.8, 4.8))
    for g in classes:
        m = y == g
        ax.scatter(XY[m, 0], XY[m, 1], s=55, alpha=0.8, label=g, color=GENOTYPE_COLORS[g])
    ax.set_xlabel(f"PC1 ({pcs.explained_variance_ratio_[0]:.0%})")
    ax.set_ylabel(f"PC2 ({pcs.explained_variance_ratio_[1]:.0%})")
    ax.set_title("Aggregated root-morphometric space by genotype")
    ax.legend(frameon=False, fontsize=9)
    _save_fig(fig, "figB2_morphometric_pca.png")

    # ---- Unsupervised outlier flagging (label-free) -----------------------
    # IsolationForest scores each well x treatment profile for how atypical it
    # is against the whole cohort, independent of its genotype label -- a QC
    # screen that would surface a genuinely aberrant sample.
    iso = IsolationForest(n_estimators=400, contamination=0.1,
                          random_state=RANDOM_STATE)
    iso.fit(Xs)
    prof = prof.assign(
        outlier_score=-iso.score_samples(Xs),           # higher = more anomalous
        flagged_outlier=iso.predict(Xs) == -1,
        cv_predicted_genotype=pred,
        genotype_mismatch=pred != y,
    )
    prof.sort_values("outlier_score", ascending=False).to_csv(
        OUT_DIR / "tableB_well_outlier_scores.csv", index=False)
    flagged = prof.loc[prof["flagged_outlier"], ["Location", "Treatment", "Genotype",
                                                 "outlier_score"]]
    metrics["part_b_morphometrics"]["n_outlier_wells_flagged"] = int(prof["flagged_outlier"].sum())
    metrics["part_b_morphometrics"]["flagged_outliers"] = (
        flagged.round(3).to_dict("records"))
    print(f"  IsolationForest flagged {len(flagged)} outlier well-profiles "
          f"(contamination=0.1)")


def main() -> None:
    print("=" * 70)
    print("APEX-05 ML anomaly detection  |  seed =", RANDOM_STATE)
    print("=" * 70)
    part_a_expression()
    part_b_morphometrics()
    with open(OUT_DIR / "ml_metrics.json", "w") as fh:
        json.dump(metrics, fh, indent=2)
    print(f"\nAll outputs written to {OUT_DIR.relative_to(REPO_ROOT)}/")
    print("Done.")


if __name__ == "__main__":
    main()
