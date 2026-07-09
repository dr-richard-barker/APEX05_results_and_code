# Machine-learning QC & anomaly detection — APEX-05

This folder holds three self-contained, reproducible analyses on the released
APEX-05 data:

| Script | Purpose |
| :-- | :-- |
| `apex05_ml_anomaly_detection.py` | Recover design factors (tissue, genotype) and demonstrate mislabel detection — the class of error behind the historical *cax2-3* anomaly. |
| `apex05_cax2_allele_concordance.py` | QC comparison of the two *CAX2* alleles (cax2-2 vs cax2-3); the basis for excluding cax2-3. See [`../../docs/cax2_allele_concordance.md`](../../docs/cax2_allele_concordance.md). |
| `apex05_cax2_concordant_core.py` | Extract the *CAX2* concordant core — genes DE under flight in **both** alleles, same direction (210 root / 146 shoot); the defensible *CAX2* flight signature. |
| `apex05_cax2_core_enrichment.py` | GO/KEGG/Reactome/WP over-representation of the core (g:Profiler; tested-gene background). Root = photosynthesis/thylakoid; shoot = cell wall + phenylpropanoid + peroxidase. **Needs internet.** |

The first script is documented in detail below; provenance of the anomaly is in
[`../../docs/PROVENANCE_cax23_mislabelling.md`](../../docs/PROVENANCE_cax23_mislabelling.md).

## Run it

```bash
python -m pip install -r analysis/ml/requirements.txt
python analysis/ml/apex05_ml_anomaly_detection.py      # ~4 min, writes to results/ml/
```

A single random seed (`RANDOM_STATE = 42`) makes the run fully reproducible.

## What it does, and what the current data show

| Part | Question | Method | Result on released data |
| :-- | :-- | :-- | :-- |
| **A — Expression** | Can we detect a swapped-label sample? | Col-0 root vs shoot log-CPM (16 samples, 8,068 shared genes); leave-one-out logistic regression; 200 controlled label-swap injections | Tissue **100%** leave-one-out accuracy; injected swaps recovered with **~97%** recall@top-2 and near-perfect score separation (injected ≈ 0.98 vs clean ≈ 0.09) |
| **B — Morphometrics** | Can we recover genotype from root architecture? | RSML primary-root features aggregated to per-well×treatment profiles (48 profiles, 25 features); leave-one-well-out random forest; IsolationForest outlier screen | Genotype **~44%** accuracy (chance 25%) — **weak**; genotypes overlap in morphometric space. This is the QC conclusion motivating provenance-based label correction |

### Interpretation (important)
- **Part A is the "detect the anomaly" result.** Transcriptomes carry an
  unambiguous tissue signal, so a mislabelled sample is separable — the same
  principle by which the original cax23 oddity surfaced in expression space.
- **Part B is deliberately reported as a negative/QC result.** Day-4
  primary-root morphology cannot reliably re-derive genotype for these subtle
  mutants. That is *why* the durable fix was to key samples on plate-well IDs
  (auditable provenance), not to rescue labels from phenotype. The script does
  **not** overstate this.

The synthetic label swaps in Part A are used **only** to validate the detector;
they make no claim about which real sample was mislabelled.

## Outputs (`results/ml/`)

| File | Content |
| :-- | :-- |
| `figA1_tissue_expression_pca.png` | PCA of Col-0 transcriptomes, coloured by tissue |
| `figA2_expression_recovery_scores.png` | Mislabel-score distributions: injected swaps vs clean samples |
| `figB1_genotype_confusion_matrix.png` | Leave-one-well-out genotype confusion |
| `figB2_morphometric_pca.png` | PCA of aggregated root-morphometric space by genotype |
| `tableB_well_outlier_scores.csv` | Per-well IsolationForest outlier + CV-mismatch scores |
| `ml_metrics.json` | All headline metrics for the run |

## Inputs
- `data/expression/counts_cpm/apex05_col0_root_cpm.csv`, `…_shoot_cpm.csv`
- `data/morphometrics/apex05_rsml_day4_morphometrics.csv`
