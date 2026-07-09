# APEX-05 — TODO / roadmap

Outstanding items to take this package from working draft to submission + archive.

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
