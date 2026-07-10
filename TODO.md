# APEX-05 — TODO / roadmap

Outstanding items to take this package from working draft to submission + archive.

## Final manuscript (WT / cax2-2 / rbohD) — multi-step
Full staged plan + feasibility in [`docs/final_manuscript_plan.md`](docs/final_manuscript_plan.md).
- [ ] **D1 decision:** how to handle the single-cell-dependent aims (dataset is bulk) —
  published Arabidopsis atlas (recommended) / provide single-cell data / drop those stages.
- [ ] **D2 decision:** source per-sample counts for cax2-2 & rbohD (OSDR accession / upload / Col-0-only).
- [x] Stage 0: canonical 64-sample count matrix (fixed v2.2) ingested + shared loader `analysis/ml/apex05_data.py`.
- [x] Stage 0b: ALSDA well metadata ingested (`data/metadata/apex05_sample_well_metadata.csv`); corrected design (64 samples, Col-0 `_3.1` = 4th biological well, not technical).
- [x] **rep→well mapping resolved from S-numbers** (verified; FL/GC paired to same well). Enables imaging joins + paired DE.
- [x] Stage 1: DESeq2 FL-vs-GC per genotype × tissue (`analysis/ml/apex05_deseq2_flight.py`), flight-referenced.
- [ ] Optional: paired/blocked DE (well as block) once rep→well is confirmed.
- [x] Stage 3b: RNA-seq DE integrated with RSML root morphometrics (well-joined). Flight shortens roots (all genotypes); cax2-2 transcriptome disproportionately attenuated vs retained phenotype.
- [x] Stage 2: LASSO flight-signature (all 3 genotypes on fixed matrix; 100% LOO; DESeq2 overlap strong Col-0/rbohD, minimal cax2-2).
- [x] Stage 3: cell-type resolution via PCMDB marker projection (DEG enrichment + signature shift). Col-0/rbohD root → epidermis/columella/cortex; cax2-2 flat.
- [x] Stage 4: ggPlantmap anatomical view — root epidermis+cortex painted for Col-0/rbohD; cax2-2 blank (figM1/figM2).
- [ ] Stage 5: **GO** per cluster (extends existing g:Profiler enrichment).
- [x] Stage 6: PhysioSpace-style stress resemblance (GO axes) — flight resembles hypoxia, oxidative/ROS, defense (SA/JA); cax2-2 none.
- [x] Stage 7 (exploratory): canonical peptide L-R flight-responsiveness — mostly no-change in bulk; defense peptides notable in rbohD shoot. True cell-cell comm needs single-cell (flagged).
- [x] Stage 8: ggKEGG pathway maps w/ DE overlay + KEGG enrichment/loci-grouping (Col-0/rbohD → glutathione + phenylpropanoid); Plant Reactome saved (sparse for Arabidopsis).
- [x] Stage 9: Final manuscript written (`manuscript/apex05_FINAL_manuscript.md`) — 3-genotype, corrected data, all stages integrated.
- [x] Stage 10: Final-manuscript website tab added (`website/build_site.py` → docs/final-manuscript.html).

## Website
- [x] **Manuscript website auto-deploys** via GitHub Actions
  ([`.github/workflows/pages.yml`](.github/workflows/pages.yml)) on every push to
  `main` touching the manuscript/figures/site. Live:
  https://dr-richard-barker.github.io/APEX05_results_and_code/ . Edit sources and
  push — no manual rebuild. See [`website/README.md`](website/README.md).

## Manuscript — author inputs still needed (`[TO CONFIRM]` in `manuscript/`)
- [ ] Differential-expression thresholds (e.g. FDR < 0.05, |log2FC| > 1).
- [ ] Leaf-area statistics: means/CI and the genotype × treatment interaction.
- [ ] Growth-hardware / imaging / flight-profile / media details (Methods).
- [ ] Precise `cax2-2` / `cax2-3` allele identifiers (T-DNA / insertion lines) and
  the *CAX2* AGI locus.
- [ ] Author list, affiliations, ORCIDs; competing-interests statement.
- [ ] OSDR / GeneLab accession(s) for the Data-availability section.
- [ ] Enrichment of the CAX2 concordant core narrative (functional interpretation).

## Archival / release
- [ ] Tag a release and deposit to Zenodo for a DOI (`CITATION.cff`, `.zenodo.json`
  are in place); add the DOI badge to `README.md`.

## Optional
- [ ] Regenerate the 4-way UpSet/Venn comparison figures as 3-genotype versions
  (needs the R DEG pipeline + counts).
