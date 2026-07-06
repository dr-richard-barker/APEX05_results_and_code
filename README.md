# APEX05 Results and Code Repository (FAIR Compliant)

This repository contains data, metadata, and analysis scripts for the **APEX05 (Advanced Plant Experiment 05)** project. The study investigates transcriptomics (RNA-Seq) and root system architecture (RSML) of *Arabidopsis thaliana* genotypes grown under spaceflight (Space Flight / FL) and Ground Control (GC) conditions.

The genotypes analyzed include:
- **Col-0 / COL00** (Wild Type)
- **cax22** (Cation Exchanger 2 Mutant)
- **cax23** (Cation Exchanger 3 Mutant)
- **rbohd** (Respiratory Burst Oxidase Homolog D Mutant)

---

## Directory Structure

The repository is organized into the following functional directories:

| Directory | Description | Key Contents |
| :--- | :--- | :--- |
| [`data/`](file:///c:/Users/drric/OneDrive/Documents/APEX05_NEO/APEX05_results_and_code/data) | Primary input data & metadata | Targets tables, transcript tables, list mappings, and `metadata.csv` |
| [`analysis/`](file:///c:/Users/drric/OneDrive/Documents/APEX05_NEO/APEX05_results_and_code/analysis) | Cleaned R & R Markdown analysis scripts | DESeq2/KEGG pathway analyses, RSML root nav mapping, and sorting utilities |
| [`results/`](file:///c:/Users/drric/OneDrive/Documents/APEX05_NEO/APEX05_results_and_code/results) | Output results of the data analysis | Tabular data tables and visualizations/plots |
| [`archive/`](file:///c:/Users/drric/OneDrive/Documents/APEX05_NEO/APEX05_results_and_code/archive) | Legacy files and target revisions | Outdated target files, diagnostic images, and deprecated reports |
| [`APEX5_desktop_clutter/`](file:///c:/Users/drric/OneDrive/Documents/APEX05_NEO/APEX05_results_and_code/APEX5_desktop_clutter) | Raw tables & supplementary archives | Original xlsx data models, upset scripts, and the raw RSML zip folder |
| [`reports/`](file:///c:/Users/drric/OneDrive/Documents/APEX05_NEO/APEX05_results_and_code/reports) | Presentations and summaries | The APEX5 storyboard visualization |

---

## Reproducibility & Setup Guide

### Required Software
- **R** (version >= 4.0 recommended)
- **RStudio** (optional, recommended for Rmd notebooks)

### Required R Packages
To install the required CRAN and Bioconductor packages, run the following commands in R:

```R
# CRAN Packages
install.packages(c("tidyverse", "ggplot2", "plotly", "pheatmap", "RColorBrewer", "ggrepel", "Hmisc", "vegan", "readxl", "UpSetR", "shiny", "gridExtra"))

# Bioconductor Packages
if (!requireNamespace("BiocManager", quietly = TRUE)) install.packages("BiocManager")
BiocManager::install(c("DESeq2", "pathview", "PGSEA", "gage", "fgsea", "PREDA", "runibic", "QUBIC"))
```

### Running the Analysis
1. **Line Ending Standardization**:
   - Files in this repo have been normalized to cross-platform standard line endings (`\n`). If you import additional raw target list files using Mac Classic (`\r`) endings, run the normalization script to ensure R parses them correctly.
2. **Metadata & Targets**:
   - The sample design is detailed in [`data/metadata.csv`](file:///c:/Users/drric/OneDrive/Documents/APEX05_NEO/APEX05_results_and_code/data/metadata.csv).
3. **Differential Expression & KEGG Pathways**:
   - Run the R Markdown notebooks in [`analysis/`](file:///c:/Users/drric/OneDrive/Documents/APEX05_NEO/APEX05_results_and_code/analysis) cell-by-cell or compile them to HTML/PDF.
   - For KEGG mapping, run [`analysis/apex5_kegg_pathway-analysis_v1.Rmd`](file:///c:/Users/drric/OneDrive/Documents/APEX05_NEO/APEX05_results_and_code/analysis/apex5_kegg_pathway-analysis_v1.Rmd) which pulls fold-change tables relatives to the data folders.

---

## FAIR Compliance Checklist

We have audited and updated this repository to adhere to **FAIR Data Principles**:

- [x] **Findable (F)**: Created a detailed [`data/metadata.csv`](file:///c:/Users/drric/OneDrive/Documents/APEX05_NEO/APEX05_results_and_code/data/metadata.csv) file indexing all 60 samples with explicit genotypes, tissues (root/shoot), flight condition codes, and replicates.
- [x] **Accessible (A)**: Shared on a public GitHub remote. Normalised line endings across 97 target list, CSV, and TSV files from deprecated Mac Classic CR (`\r`) to cross-platform standard newlines (`\n`) so they render and parse correctly on Windows, Mac, and Linux.
- [x] **Interoperable (I)**: Fixed parser-breaking syntax errors in legacy R and Rmd files (removed trailing shell characters, resolved unquoted spaces in column vectors, corrected unclosed Rmd code blocks). Parameterized absolute hardcoded laptop paths (`/Users/richardbarker/...`) with relative inputs.
- [x] **Reusable (R)**: Documented package dependencies, annotated legacy GUI RobiNA exports with header warnings, and configured templates with automatic simulation fallbacks if counts files are missing. Licensed under the MIT License (see `LICENSE`).
