# Data Directory - APEX05

This directory contains the primary inputs, mapping target files, and sample metadata used throughout the APEX05 transcriptomics and root growth analysis.

---

## File Descriptions

### Metadata and Design Lists
- **[`metadata.csv`](file:///c:/Users/drric/OneDrive/Documents/APEX05_NEO/APEX05_results_and_code/data/metadata.csv)**: Standardized, fully-populated sample metadata spreadsheet indexing all 60 samples. Columns include: `sample_id`, `genotype`, `tissue` (Root/Shoot), `treatment` (Space Flight vs Ground Control), `replicate`, `raw_file_path`, and descriptive `notes`.
- **[`apex5_hiseq_hisat_targets.txt`](file:///c:/Users/drric/OneDrive/Documents/APEX05_NEO/APEX05_results_and_code/data/apex5_hiseq_hisat_targets.txt)**: Master targets list file mapping sample labels to groups, bag locations, conditions, and genotypes for read alignment.
- **`apex5_*_hisat_targets.txt`**: Sub-targets maps filtered by specific genotypes and tissues (e.g., `apex5_col0_root_hisat_targets.txt`, `apex5_cax22_shoot_hisat_targets.txt`, etc.).

### Transcript & Differential Expression Data
- **`apex5_col_root_drb_transcripts_01.txt`** and **`apex5_col_shoot_drb_transcripts_01.txt`**: Gene/transcript table for Wild Type (Col-0) Root/Shoot containing baseMeans, log2 Fold Changes, raw p-values, adjusted p-values, and gene Symbols.
- **`apex5_rbohd_root_drb_transcripts_01.txt`** and **`apex5_rbohd_shoot_drb_transcripts_01.txt`**: Gene/transcript table for the *rbohd* mutant Root/Shoot under Flight vs Ground Control conditions.
- **`apex5_col-vs-rbohd_root_gc_gc.txt`** and **`apex5_col-vs-rbohd_shoot_gc_gc.txt`**: Contrast analyses comparing Wild Type Col-0 vs *rbohd* under Ground Control conditions.

### Pathway and Enrichment Sets
- **`apex5_deseq2_idep_all-gene-lists-gmt.txt`**: Gene Matrix Transposed (GMT) pathway database formatted for the iDEP portal.
- **`apex5_rbohd_fl-gc_aracyc-drb_cellwall.txt`**: Curated list of cell wall biosynthesis and structure genes mapped to AraCyc pathways for *rbohd* analysis.
- **`apex5_edger_jvenn_metascape-genes.txt`**: Unified gene list formatted for enrichment mapping in Metascape and Venn comparison in jVenn.

### Summaries of Regulated Genes
- **`apex5_root_up-regulated_summary.txt`** / **`apex5_root_down-regulated_summary.txt`**: Tabulated summaries of significantly upregulated and downregulated genes in root tissues.
- **`apex5_shoot_up-regulated_summary.txt`** / **`apex5_shoot_down-regulated_summary.txt`**: Tabulated summaries of significantly upregulated and downregulated genes in shoot tissues.
- **`apex5_shoot_up-regulated_amigo-analysis.txt`**: Functional annotations and GO categories extracted from AMIGO for shoot-upregulated genes.
