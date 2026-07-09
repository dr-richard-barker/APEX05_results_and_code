# Changelog

All notable changes to the APEX-05 data & code package are recorded here.
This project adheres to [Semantic Versioning](https://semver.org/) for releases.

## [Unreleased] — FAIR reorganisation, ML QC, and manuscript scaffold

### Changed — CAX2 allele nomenclature & cax2-3 exclusion (2026-07-09)
- **Corrected nomenclature.** `cax22`/`cax23` are **cax2-2 / cax2-3 — two
  independent alleles of the *same* gene *CAX2***, not mutants of two different
  genes (CAX2 vs CAX3). Fixed across README, `data/README.md`, the provenance
  doc, and the manuscript.
- **Excluded cax2-3 from the primary analysis** on QC grounds. A new concordance
  analysis (`analysis/ml/apex05_cax2_allele_concordance.py`,
  `docs/cax2_allele_concordance.md`, Fig. 6) showed cax2-3's flight-response DEG
  set is 9.6× (root) / 17.1× (shoot) larger than cax2-2's and no more similar to
  cax2-2 than to unrelated genotypes, despite 97% direction agreement on the
  small shared core. Primary genotype set is now **Col-0, cax2-2, rbohD**.
- **Retained, not deleted.** cax2-3's derived result tables were relocated to
  `archive/excluded_cax2-3/` (with a README) for auditability; cax2-3 remains
  inside the two QC analyses that justify the exclusion. Raw cax2-3 measurements
  stay in `data/`.

### Added — functional enrichment (2026-07-09)
- **CAX2 concordant-core enrichment** (`analysis/ml/apex05_cax2_core_enrichment.py`):
  root core = photosynthesis/thylakoid; shoot core = cell wall + phenylpropanoid
  + peroxidase/H₂O₂ (g:Profiler, tissue-specific tested-gene background).
- **Primary-genotype enrichment** (`analysis/ml/apex05_primary_genotype_enrichment.py`)
  for Col-0, cax2-2, rbohD × root/shoot: Col-0 broad stress/transport, rbohD
  redox/detox, cax2-2 photosynthesis(root)/cell-wall(shoot). Figures 8–9,
  tables `results/tables/apex05_*_enrichment_*.csv`.
- **Flight-direction convention corrected.** Verified that the source DEG
  workbooks' `up-regulated`/`down-regulated` labels are **ground-control-referenced**
  (100% of "up-regulated" genes are higher in GC = *repressed* by flight).
  All new analyses classify genes by true FL/GC direction; the manuscript now
  reports flight-referenced counts with an explicit note. **Author to confirm the
  intended convention before submission.**

### Added
- **Machine-learning QC pipeline** (`analysis/ml/apex05_ml_anomaly_detection.py`,
  scikit-learn) that recovers tissue identity from expression (100% leave-one-out
  accuracy), detects injected label swaps at ~97% recall, and reports the honest
  limitation that day-4 root morphology only weakly separates genotype (~44%,
  chance 25%). Reproducible with `seed = 42`; outputs in `results/ml/`.
- **Sample-labelling provenance record** (`docs/PROVENANCE_cax23_mislabelling.md`)
  reconstructing the *cax22*↔*cax23* adjacent-well anomaly, its detection via the
  "CAX23oddity" diagnostic, and the two-stage fix (dropping the anomalous *cax23*
  ground-control replicate and the ambiguous *cax22* well A7 → clean 60-sample
  master keyed on plate-well IDs).
- **npj Microgravity manuscript scaffold** with figure legends and a supplementary
  index (`manuscript/`).
- FAIR metadata: `CITATION.cff`, `.zenodo.json`, `.gitattributes`.
- Human-readable header blocks and inline annotation across the R analysis
  scripts (`analysis/R/`).

### Changed
- **Repository reorganised into a clean, self-describing FAIR tree.** The ad-hoc
  `APEX5_desktop_clutter/` dump and flat `data/` were consolidated into
  `data/{metadata,expression,morphometrics,genesets,summaries}`,
  `analysis/{R,ml}`, `results/{tables,plots,ml}`, `media/`, `manuscript/`, and
  `archive/{targets_file_versions,diagnostics,superseded}`.
- Files renamed to a consistent `apex05_*` lowercase-kebab convention.
- All moves performed with `git mv` so file history is preserved.

### Deprecated / Archived
- Targets-File revisions V3–V13 moved to `archive/targets_file_versions/`.
- Redundant `.txt` twins of `.csv` tables and "delete-me" gene lists moved to
  `archive/superseded/`.
- The unrelated APEX-04 methylation supplement moved to `archive/`.

### Data-integrity notes
- The published 60-sample design (`data/metadata/apex05_sample_metadata.csv`) is
  the authoritative sample set. Historical files in `archive/` are retained for
  provenance only and should **not** be used for new analyses.
- See `docs/PROVENANCE_cax23_mislabelling.md` for caveats about inconsistently
  named legacy files (e.g. the two different `cax23_removed` files).
