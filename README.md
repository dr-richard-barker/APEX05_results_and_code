# APEX-05 — *Arabidopsis* spaceflight transcriptomics, root architecture & ML sample QC

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![FAIR](https://img.shields.io/badge/data-FAIR-blue.svg)](#fair-compliance)

A **FAIR**, reproducible data-and-code package for the **Advanced Plant
Experiment-05 (APEX-05)**. The study compares four *Arabidopsis thaliana*
genotypes flown on the International Space Station (**spaceflight, FL**) against
matched **ground controls (GC)**, profiling both the **shoot and root
transcriptome** (RNA-seq) and **root system architecture** (RSML morphometrics).

| Genotype (code) | Gene / allele | Pathway | Status |
| :-- | :-- | :-- | :-- |
| **Col-0** | wild type | reference | primary |
| ***rbohD*** | *RBOHD* — Respiratory Burst Oxidase Homolog D | reactive-oxygen (ROS) production | primary |
| ***cax2-2*** (`cax22`) | *CAX2* — cation/H⁺ exchanger, **allele 2** | calcium transport | primary |
| ***cax2-3*** (`cax23`) | *CAX2* — cation/H⁺ exchanger, **allele 3** | calcium transport | **excluded (QC)** |

> **Note on the *cax* lines.** `cax22` and `cax23` are shorthand for **`cax2-2`**
> and **`cax2-3`** — **two independent mutant alleles of the *same* gene, `CAX2`**
> (not mutations in two different genes). A QC comparison found their spaceflight
> responses **strongly discordant** (cax2-3's DEG set is ~10–17× inflated and no
> more similar to cax2-2 than to unrelated genotypes), so **cax2-3 is excluded
> from the primary analysis** and cax2-2 is the *CAX2* representative. The primary
> genotype set is **Col-0, cax2-2, rbohD**. Evidence and decision:
> [`docs/cax2_allele_concordance.md`](docs/cax2_allele_concordance.md); retained
> cax2-3 result files: [`archive/excluded_cax2-3/`](archive/excluded_cax2-3/).

> **Headline biology.** Wild-type leaf area is reduced under spaceflight, and the
> ROS- and calcium-signalling mutants respond differently — implicating both
> pathways in the spaceflight growth response. Hundreds of flight-responsive
> genes were identified per genotype and tissue. See [`manuscript/`](manuscript).

> **Headline method.** A machine-learning QC layer
> ([`analysis/ml/`](analysis/ml)) recovers tissue identity from expression with
> 100% leave-one-out accuracy and detects mislabelled samples at ~97% recall,
> and documents the resolution of a *cax23* adjacent-well sample-labelling
> anomaly ([`docs/PROVENANCE_cax23_mislabelling.md`](docs/PROVENANCE_cax23_mislabelling.md)).

---

## Repository layout

```
APEX05_results_and_code/
├── data/
│   ├── metadata/            # 60-sample metadata (well-ID keyed), HISAT targets
│   │   └── targets_by_group/
│   ├── expression/
│   │   ├── counts_cpm/      # per-sample Col-0 root & shoot CPM matrices
│   │   ├── transcript_tables/  # per-genotype DESeq/edgeR transcript tables
│   │   ├── contrasts_full/  # full FL-vs-GC DESeq contrast tables
│   │   └── contrasts_gc/    # ground-control genotype contrasts
│   ├── morphometrics/       # RSML root-architecture measurements
│   ├── genesets/            # GMT / Metascape / AraCyc gene-set inputs
│   └── summaries/           # regulated-gene summaries
├── analysis/
│   ├── R/                   # annotated DESeq2 / edgeR / KEGG / RSML scripts
│   └── ml/                  # Python scikit-learn QC & anomaly-detection pipeline
├── results/
│   ├── tables/              # differential-expression & enrichment tables
│   ├── plots/               # figures (venn/upset/pathway/PCA)
│   └── ml/                  # machine-learning figures + metrics
├── manuscript/              # npj-styled manuscript scaffold, legends, supp index
├── docs/                    # provenance & data-integrity documentation
├── reports/                 # storyboard / presentation assets
├── media/                   # time-lapse imaging
└── archive/                 # historical Targets versions, diagnostics, superseded files
```

Full per-directory descriptions are in [`data/README.md`](data/README.md).

---

## Quick start

### Machine-learning QC (Python, self-contained, ~4 min)
```bash
python -m pip install -r analysis/ml/requirements.txt
python analysis/ml/apex05_ml_anomaly_detection.py     # writes results/ml/
```
See [`analysis/ml/README.md`](analysis/ml/README.md) for what each output means.

### Transcriptomics (R ≥ 4.0)
```r
# CRAN
install.packages(c("tidyverse","ggplot2","plotly","pheatmap","RColorBrewer",
                   "ggrepel","Hmisc","vegan","readxl","UpSetR","gridExtra"))
# Bioconductor
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("DESeq2","edgeR","pathview","fgsea","gage"))
```
Then run the annotated scripts in [`analysis/R/`](analysis/R) (each carries a
header describing its inputs, outputs and usage).

---

## The *cax23* sample-labelling anomaly (and how it was fixed)

*cax22* and *cax23* were grown in **physically adjacent plate wells**, which made
an adjacent-well genotype swap possible. A DEG-overlap QC screen (the
"CAX23oddity") flagged it; the fix was to key every sample on its **plate-well
ID**, drop the anomalous *cax23* ground-control replicate and the ambiguous
*cax22* well A7, and publish a clean **60-sample** master. The full,
file-by-file reconstruction is in
[`docs/PROVENANCE_cax23_mislabelling.md`](docs/PROVENANCE_cax23_mislabelling.md).
The ML pipeline independently demonstrates that this class of error is
detectable in expression space but *not* recoverable from root morphology alone —
which is why provenance, not phenotype, is the durable fix.

---

## FAIR compliance

- **Findable** — self-describing directory tree; 60-sample
  [`metadata`](data/metadata/apex05_sample_metadata.csv); `CITATION.cff` and
  `.zenodo.json` for a citable Zenodo DOI.
- **Accessible** — public GitHub repository; open formats (CSV/TSV/TXT/PNG);
  cross-platform line endings via `.gitattributes`.
- **Interoperable** — standard bioinformatics formats (GMT, jVenn, AraCyc/KEGG),
  gene IDs as AGI/Entrez/UniProt; documented column dictionaries.
- **Reusable** — MIT licensed; annotated code with pinned dependencies;
  reproducible ML pipeline (fixed seed); complete provenance and a `CHANGELOG.md`.

## Citation
See [`CITATION.cff`](CITATION.cff). Raw reads: NASA OSDR/GeneLab (accession *to
be confirmed*). Companion manuscript: [`manuscript/`](manuscript).

## License
[MIT](LICENSE).
