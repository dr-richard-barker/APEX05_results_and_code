# Changelog

All notable changes to the APEX-05 data & code package are recorded here.
This project adheres to [Semantic Versioning](https://semver.org/) for releases.

## [Unreleased] — FAIR reorganisation, ML QC, and manuscript scaffold

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
