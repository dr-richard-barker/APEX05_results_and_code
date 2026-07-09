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
| 1 | Re-run DESeq2/edgeR FL-vs-GC on the 3 genotypes (or reuse released DEGs) | DESeq2/edgeR (R) or pydeseq2 | counts (stage 0) | pending 0 |
| 2 | **LASSO flight-signature** to support DESeq (sparse, cross-validated; overlap with DEGs) | glmnet / sklearn `LogisticRegressionCV(penalty=l1)` | per-sample counts | **Col-0 doable now; mutants pending 0** |
| 3 | **Cell-type resolution of the bulk signal** (roots & shoots separately) via a published *A. thaliana* single-cell atlas: deconvolution + autoencoder latent projection of flight-DEGs onto cell types | scanpy / an autoencoder (torch) + reference atlas | external atlas + counts | **BLOCKED — decision D1** |
| 4 | **ggPlantmap** anatomical visualisation of the tissue/cell-type-resolved response | ggPlantmap (R) | stage 3 output | pending 3 |
| 5 | **GO** of clusters/DEGs | g:Profiler (done) + per-cluster | gene lists | partly done |
| 6 | **PhysioSpace** stress-resemblance of clusters/DEGs | PhysioSpaceMethods (R) or Python projection + a plant abiotic-stress reference (e.g. AtGenExpress) | DEG signatures + reference | needs reference build |
| 7 | **Cell–cell communication** (ligand–receptor) | PlantCellChat / CellChat | **single-cell + cell types** | **BLOCKED — decision D1** (only exploratory bulk L-R possible) |
| 8 | **ggKEGG + Reactome** systems-biology pathway grouping of loci | ggkegg (installed) + Reactome (gProfiler REAC / ReactomePA) | DEG lists (have) | **doable now** |
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

## Doable immediately (no new data, no decision)

- Stage 2 LASSO on **Col-0** root & shoot (Python sklearn).
- Stage 8 **ggKEGG + Reactome** pathway synthesis from the existing 3-genotype DEG
  lists.
- Stage 6 **PhysioSpace-style** projection once a plant stress reference is chosen.
- Stage 9/10 manuscript + website scaffolding.
