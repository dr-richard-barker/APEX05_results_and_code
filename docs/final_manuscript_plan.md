# APEX-05 — Final manuscript: staged analysis & synthesis plan

Goal: a **final APEX-05 manuscript** focused on the **three primary genotypes
(Col-0 / WT, cax2-2, rbohD)** — cax2-3 excluded (see
[`cax2_allele_concordance.md`](cax2_allele_concordance.md)) — with a LASSO ML
layer supporting the DESeq statistics, cell-type-resolved interpretation,
stress-signature and cell–cell-communication analysis, and systems-biology
pathway synthesis, integrated as a new website tab.

> **Status:** planning. Nothing below is fabricated; each stage lists its data
> requirement and whether that requirement is currently met. Two decisions
> (§Decisions) gate the single-cell-dependent stages.

## Data model (updated after the fixed matrix + ALSDA metadata)

- **Canonical counts:** `data/expression/counts_raw/apex05_gene_counts_all-genotypes_v2.2.csv`
  — 33,602 genes × **64 samples** (fixed v2.2; includes a corrected cax2-3).
- **Design:** 4 genotypes × **4 wells (biological reps)** × 2 tissues × 2 conditions.
  Col-0's `_3.1` column is the **4th biological well, not a technical replicate**
  (log2CPM r to rep 3 = 0.993, same as any pair; ALSDA lists 4 Col-0 wells).
- **Well / imaging bridge:** `data/metadata/apex05_sample_well_metadata.csv` (ALSDA)
  gives each sample's plate **Location (well)** — the key the **RSML root-imaging**
  data (`data/morphometrics/`) is indexed on. Each well appears in **both FL and
  GC** (position-paired), so:
  - RNA-seq ↔ morphometrics can be **joined per well** for multi-omics integration;
  - DE can use a **paired/blocked design** (well as block) for more power.
- **Shared loader:** `analysis/ml/apex05_data.py` builds the sample sheet
  (`data/metadata/apex05_rnaseq_sample_sheet.csv`) and attaches Location,
  S-number and ALSDA sample name to each library. The **rep→well mapping is
  resolved from the S-numbers** (each well has a contiguous, staggered S-number
  block; wells ordered by minimum S-number → rep 1..4). Verified: every
  (genotype, rep) → a single well, so FL rep-i and GC rep-i are the **same well**
  (position-paired). This unblocks well-level imaging joins and paired DE.

## Hard constraints discovered

1. **Bulk RNA-seq, not single-cell.** No single-cell data exists in this dataset.
   "Single-cell autoencoder clusters" and "PlantCellChat cell–cell communication"
   cannot be run on our data directly — they need cell-resolved counts. Honest
   options are in §Decisions.
2. **Per-sample counts exist only for Col-0** (`data/expression/counts_cpm/`).
   cax2-2 and rbohD are available only as DEG / group-mean contrast tables.
   Any per-sample ML (LASSO, deconvolution) across the mutants needs their
   per-sample count matrices (OSDR or PI-provided).
3. **Tooling.** Of the requested R packages only `ggkegg` is installed; `glmnet`,
   `Seurat`, `CellChat`/PlantCellChat, `ggPlantmap`, `PhysioSpaceMethods`,
   `DESeq2`/`edgeR` are not. Python (sklearn, scanpy) is available.

## Staged plan

| # | Stage | Method / tool | Data requirement | Status |
|:-|:-|:-|:-|:-|
| 0 | **Assemble 3-genotype per-sample count matrix** (root, shoot) | OSDR fetch or PI upload | per-sample counts for WT/cax2-2/rbohD | **BLOCKED — decision D2** |
| 0b | **Integrate ALSDA well metadata**; confirm rep→well map; enable RNA-seq↔RSML join | `apex05_data.py` + ALSDA sheet | metadata (have) | **DONE (mapping provisional)** |
| 1 | DESeq2 FL-vs-GC on the 64-sample fixed matrix, per genotype × tissue (paired-by-well option) | pydeseq2 (`apex05_deseq2_flight.py`) | counts (have) | **DONE (unpaired); paired pending rep→well** |
| 3b | **Multi-omics: RNA-seq flight response <-> RSML root architecture** (`apex05_multiomics_integration.py`) | `apex05_data` + `data/morphometrics/` | well map (done) | **DONE.** Flight shortens roots (all genotypes, strongest rbohD); cax2-2's transcriptome is disproportionately attenuated vs its retained morphological response |
| 2 | **LASSO flight-signature** to support DESeq (sparse, cross-validated; overlap with DEGs) | sklearn L1 logistic (`analysis/ml/apex05_lasso_flight_signature.py`) | per-sample counts | **Col-0 DONE; mutants auto-extend on upload** |
| 3 | **Cell-type resolution of the bulk signal** — marker-based projection onto PCMDB (published *A. thaliana* cell-type markers): flight-DEG cell-type enrichment + per-sample signature shift | scanpy/sklearn + PCMDB (`apex05_celltype_markers.py`, `apex05_celltype_projection.py`) | PCMDB (fetched) + counts | **DONE.** Col-0/rbohD root → epidermis, **columella (gravity-sensing)**, cortex; rbohD shoot → mesophyll; cax2-2 flat |
| 4 | **ggPlantmap** anatomical view of the cell-type-resolved flight response (`apex05_ggplantmap_anatomy.R`) | ggPlantmap (R, installed) | stage 3 | **DONE.** Root cross-section painted by cell-type enrichment: Col-0/rbohD localise to epidermis+cortex; cax2-2 blank (figM1/figM2) |
| 5 | **GO** of clusters/DEGs | g:Profiler (done) + per-cluster | gene lists | partly done |
| 6 | **PhysioSpace-style stress resemblance** — per **organ** (`apex05_physiospace_stress.py`, Fig 7) and per **cell type** (`apex05_celltype_stress_decoding.py`, Fig S3) | g:Profiler GO 'response to' axes + PCMDB markers | DESeq2 DEGs | **DONE.** Organ-specific: root → **hypoxia/oxidative**, shoot → **defence (SA/JA/wounding)**; cax2-2 none. Cell-type-resolved: defence/hormone signature chiefly in **leaf mesophyll**. (Resemblance/overlap, not directional projection) |
| 7 | **Exploratory ligand–receptor** (canonical peptide modules; `apex05_ligand_receptor.py`) | g:Profiler ID resolution + DESeq2 | DESeq2 DEGs | **DONE (exploratory).** No canonical peptide L-R pair strongly flight-modulated in bulk; defense peptides (PROPEP3/PIP2/IDA) notable in rbohD shoot. True cell–cell communication needs single-cell (flagged) |
| 8 | **ggKEGG + KEGG/Reactome** systems-biology pathway grouping of loci | ggkegg (`apex05_ggkegg_pathways.R`) + KEGG-membership grouping (`apex05_kegg_systems.py`) | DESeq2 DEGs | **DONE.** ggKEGG maps (phenylpropanoid/glutathione/hormone) w/ DE overlay; KEGG enrichment+loci grouping (Col-0/rbohD → glutathione+phenylpropanoid). Plant Reactome sparse (rice-projected) → saved for transparency |
| 9 | **Write the Final manuscript** (3-genotype biological narrative integrating 1–8) | — | stages above | pending |
| 10 | **New website tab** "Final manuscript" | website/build_site.py | stage 9 | scaffolded |

## Decisions needed (gate stages 3, 7 and the mutant half of 0/2)

- **D1 — single-cell dependency.** The dataset is bulk. To honour the
  cell-type/cluster and cell–cell-communication aims we can: (a) use a **published
  Arabidopsis single-cell atlas** (root + shoot) as a reference to deconvolve the
  bulk samples and project the flight response onto cell types (bulk-appropriate,
  no fabrication) — the recommended path; (b) ingest **single-cell data you
  provide**; or (c) **drop** the single-cell/cell-chat stages and keep the
  bulk-level LASSO + GO + PhysioSpace + KEGG/Reactome synthesis.
- **D2 — mutant per-sample counts.** LASSO and deconvolution need per-sample
  matrices for cax2-2 and rbohD, which aren't in the repo. Either (a) fetch from
  **OSDR/GeneLab** (need the accession), (b) you **upload** them, or (c) run
  per-sample ML on **Col-0 only** and treat the mutants via their released DEG
  tables (signature scoring/projection).

## Progress log

- **Stage 2 — LASSO (Col-0), done.** An L1-penalised logistic model
  (`analysis/ml/apex05_lasso_flight_signature.py`) selects a stability-selected
  sparse flight signature per tissue and predicts FL vs GC at 100% leave-one-out
  accuracy. The signature is strongly enriched for the independent DESeq DEGs:
  **root** 94 genes, 78 also DESeq DEGs (hypergeometric *p* = 3×10⁻⁷⁸);
  **shoot** 70 genes, 16 DESeq (p = 1×10⁻¹¹) — orthogonal ML support for the DE
  calls. Auto-extends to cax2-2 / rbohD when their CPM matrices are added to
  `data/expression/counts_cpm/`. Outputs:
  `results/tables/apex05_lasso_col0_{root,shoot}_signature.csv`,
  `results/ml/figE1_lasso_flight_signature.png`, `lasso_flight_summary.json`.

## Doable immediately (no new data, no decision)

- Stage 2 LASSO on **Col-0** root & shoot (Python sklearn).
- Stage 8 **ggKEGG + Reactome** pathway synthesis from the existing 3-genotype DEG
  lists.
- Stage 6 **PhysioSpace-style** projection once a plant stress reference is chosen.
- Stage 9/10 manuscript + website scaffolding.
