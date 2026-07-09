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
- [ ] Stage 3b: **integrate RNA-seq DE with RSML root morphometrics per well** (multi-omics).
- [ ] Stage 2: **LASSO** flight-signature to support DESeq (Col-0 now; mutants after D2).
- [ ] Stage 3: cell-type resolution via reference atlas (deconvolution + autoencoder) — after D1.
- [ ] Stage 4: **ggPlantmap** anatomical visualisation of the resolved response.
- [ ] Stage 5: **GO** per cluster (extends existing g:Profiler enrichment).
- [ ] Stage 6: **PhysioSpace** stress-resemblance projection (needs a plant stress reference).
- [ ] Stage 7: **PlantCellChat** ligand–receptor / cell–cell communication — after D1 (needs cell types).
- [ ] Stage 8: **ggKEGG + Reactome** systems-biology pathway grouping (doable now).
- [ ] Stage 9: write the **Final manuscript** (3-genotype biological narrative).
- [ ] Stage 10: add a **"Final manuscript" website tab** and wire into `website/build_site.py`.

## Website
- [ ] **Maintain the manuscript website.** Rebuild after every manuscript/figure
  change (`python website/build_site.py`, then commit `docs/`). Keep the live
  GitHub Pages site (https://dr-richard-barker.github.io/APEX05_results_and_code/)
  in sync with `manuscript/`. See [`website/README.md`](website/README.md).

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
