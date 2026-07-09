# Excluded genotype — cax2-3

These files are the **primary-analysis outputs for the *cax2-3* allele**, which
has been **excluded from the APEX-05 primary analysis** on quality-control
grounds. They are **retained here (not deleted) for transparency and FAIR
reproducibility** — the exclusion decision must remain auditable.

## Why cax2-3 was excluded

`cax2-2` (`cax22`) and `cax2-3` (`cax23`) are **two independent mutant alleles of
the same gene, *CAX2***, and should therefore give concordant spaceflight
responses. They do not. A concordance analysis of the released DEG tables found:

- cax2-3's flight-responsive DEG set is **9.6× (root) / 17.1× (shoot)** larger
  than cax2-2's, and larger than every other genotype including wild type;
- cax2-2 overlaps cax2-3 (DEG-set Jaccard 0.069 root / 0.045 shoot) **no more
  than it overlaps the unrelated genotypes** Col-0 and rbohD;
- yet the small shared core agrees **97%** in fold-change direction.

This is the signature of large technical/background variance in the cax2-3
libraries on top of a small genuine signal — consistent with the documented
adjacent-well labelling anomaly — **not** a real biological divergence between
two alleles of one gene.

- Full write-up: [`../../docs/cax2_allele_concordance.md`](../../docs/cax2_allele_concordance.md)
- Analysis code: [`../../analysis/ml/apex05_cax2_allele_concordance.py`](../../analysis/ml/apex05_cax2_allele_concordance.py)
- Labelling-anomaly provenance: [`../../docs/PROVENANCE_cax23_mislabelling.md`](../../docs/PROVENANCE_cax23_mislabelling.md)

## Where cax2-3 is still (deliberately) present

cax2-3 remains **only inside the two QC analyses whose purpose is to justify this
exclusion** — removing it from those would delete the evidence:

- the CAX2-allele concordance analysis (`analysis/ml/apex05_cax2_allele_concordance.py`,
  which reads the contrast tables in *this* folder);
- the morphometric genotype-confusion QC (`analysis/ml/apex05_ml_anomaly_detection.py`,
  Part B), which demonstrates that day-4 root phenotype cannot separate the
  genotypes and shows the cax2-2/cax2-3 confusion.

The raw cax2-3 RNA-seq/RSML measurements remain in `data/` under their shared
per-genotype targets and morphometrics files; only cax2-3's *derived
primary-analysis result tables* were relocated here.

## Files

| File | What it is |
| :-- | :-- |
| `apex05_cax23_root_fl-vs-gc_full.csv` | cax2-3 root FL-vs-GC full DESeq contrast table |
| `apex05_cax23_shoot_fl-vs-gc_full.csv` | cax2-3 shoot FL-vs-GC full DESeq contrast table |
| `apex05_cax23_root_metascape.csv/.xlsx` | cax2-3 root Metascape enrichment input |
| `apex5_cax23_root_hisat_targets.csv` | cax2-3 root HISAT alignment targets |
