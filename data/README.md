# APEX-05 — `data/` dictionary

Primary inputs for the APEX-05 transcriptomics and root-architecture analyses.
Design factors throughout: **genotype** (Col-0, cax22, cax23, rbohD) ×
**tissue** (Root/Shoot) × **treatment** (Space Flight `FL` vs Ground Control
`GC`) × **replicate**, keyed on **plate-well ID**.

> **Genotype codes.** `cax22` = **cax2-2** and `cax23` = **cax2-3** are **two
> independent mutant alleles of the same gene, *CAX2*** (not two different
> genes). `rbohD` mutates *RBOHD*. A QC comparison found the two *CAX2* alleles
> strongly discordant — see [`../docs/cax2_allele_concordance.md`](../docs/cax2_allele_concordance.md).

## `metadata/`
| File | Description |
| :-- | :-- |
| `apex05_sample_metadata.csv` | Authoritative 60-sample table. Columns: `sample_id` (well-keyed, e.g. `A1_FL_Root`), `genotype`, `tissue`, `treatment`, `replicate`, `raw_file_path`, `notes`. |
| `apex05_hisat_targets_master.txt` | Master targets list mapping sample labels → group / condition / genotype for HISAT2 alignment. |
| `apex05_miseq_bam_name_map.txt` | MiSeq BAM-filename ↔ sample map. |
| `targets_by_group/` | Per genotype × tissue targets subsets (e.g. `apex5_col0_root_hisat_targets.txt`). |

> The final master **omits cax22 well A7** and the anomalous cax23 GC replicate.
> See [`../docs/PROVENANCE_cax23_mislabelling.md`](../docs/PROVENANCE_cax23_mislabelling.md).

## `expression/`
| Subfolder / file | Description |
| :-- | :-- |
| `counts_cpm/apex05_col0_root_cpm.csv`, `…_shoot_cpm.csv` | Per-sample edgeR CPM matrices (Col-0), `Entrez ID` × 8 libraries (4 FL + 4 GC). The only per-replicate count matrices in the package. |
| `transcript_tables/apex05_<geno>_<tissue>_transcripts.txt` | Per-genotype/tissue transcript tables: locus/transcript/Entrez IDs, group baseMeans (`…A_FL`, `…B_GC`), log2 fold change, p/adj-p, gene symbol. |
| `contrasts_full/apex05_<geno>_<tissue>_fl-vs-gc_full.csv` | Full DESeq2 FL-vs-GC contrast tables (`ID, baseMean, baseMeanA, baseMeanB, foldChange, log2FoldChange, pval, padj`). |
| `contrasts_full/apex05_gc-root_3factor_logfc.csv` | 3-factor model log-fold-change table (ground-control root). |
| `contrasts_gc/apex05_col-vs-rbohd_<tissue>_gc.txt` | Col-0 vs rbohD contrasts under ground control. |

## `morphometrics/`
| File | Description |
| :-- | :-- |
| `apex05_rsml_day4_morphometrics.csv` | Day-4 RSML primary-root measurements (469 roots). Columns: `Location` (plate well), `Treatment`, `Genotype`, `Age`, `image`, plus morphometric traits `length, vector_length, surface, volume, direction, diameter` (per-root; higher-order-root traits such as `n_child`/`insertion_angle` are constant on this primary-root export). |
| `apex05_wt-rbohd_rsml_roots.csv` | WT vs rbohD RSML export (same trait schema). |
| `apex05_rsml_source_col-cax22.zip` | Source RSML tracings (Col/cax22). |

## `genesets/`
Pathway/enrichment inputs: `apex05_idep_gmt.txt` (iDEP GMT),
`apex05_metascape_jvenn_genes.txt`, `apex05_rbohd_aracyc_cellwall.txt`
(curated cell-wall AraCyc list), `apex05_cax23_root_metascape.{csv,xlsx}`,
`apex05_transcription-factor_summary.xlsx`.

## `summaries/`
Tabulated up-/down-regulated gene summaries for root and shoot
(`apex5_{root,shoot}_{up,down}-regulated_summary.txt`) and a shoot AMIGO GO
annotation (`apex5_shoot_up-regulated_amigo-analysis.txt`).

---

### Conventions
- Gene identifiers: AGI locus (`AT#G#####`), Entrez, or UniProt as noted per file.
- Condition codes: `FL` = Space Flight, `GC` = Ground Control.
- Encoding: UTF-8, LF line endings (enforced by `.gitattributes`).
