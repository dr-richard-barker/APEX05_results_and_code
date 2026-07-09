# APEX-05 — Supplementary information index

This index maps every supplementary table and dataset released with the
manuscript to its file in the repository. Numbering is provisional and may be
renumbered to match the final manuscript. All paths are relative to the
repository root.

## Supplementary datasets & tables

| ID | Title | File | Format |
| :-- | :-- | :-- | :-- |
| S1 | Sample metadata — 60 libraries (genotype × tissue × flight/ground × replicate), keyed on plate-well ID | `data/metadata/apex05_sample_metadata.csv` | CSV |
| S2 | HISAT alignment targets — master | `data/metadata/apex05_hisat_targets_master.txt` | TXT |
| S3 | Per-group HISAT targets (genotype × tissue) | `data/metadata/targets_by_group/` | TXT |
| S4 | All-sample edgeR differential-expression table | `results/tables/apex5_edger_deg_all-samples.csv` | CSV |
| S5 | Col-0 **root** DEGs — up-regulated (531) | `results/tables/apex5_col_root_edger_up-regulated-531.xlsx` | XLSX |
| S6 | Col-0 **root** DEGs — down-regulated (268) | `results/tables/apex5_col_root_edger_down-regulated-268.xlsx` | XLSX |
| S7 | Col-0 **shoot** DEGs — up-regulated (403 / 559 variants) | `results/tables/apex5_col_shoot_edger_up-regulated-403.xlsx`, `…-559.xlsx` | XLSX |
| S8 | Col-0 **shoot** DEGs — down-regulated (201) | `results/tables/apex5_col_shoot_edger_down-regulated-201.xlsx` | XLSX |
| S9 | *rbohD* **root** DEGs — up (393) / down (233) | `results/tables/apex5_rbohd_root_up-regulated-393.xlsx`, `…_down-regulated-233.xlsx`, `apex5_rbohd_root_deg.xlsx` | XLSX |
| S10 | *rbohD* **shoot** DEGs — up (170) / down (286) | `results/tables/apex5_rbohd_shoot_up-regulated-170.xlsx`, `…_down-regulated-286.xlsx`, `apex5_rbohd_shoot_deg.xlsx` | XLSX |
| S11 | *cax22* **root** DEGs — up (100) / down (223) | `results/tables/apex5_cax22_root_up-regulated-100.xlsx`, `…_down-regulated-223.xlsx` | XLSX |
| S12 | *cax22* **shoot** DEGs — up (45) / down (185) | `results/tables/apex5_cax22_shoot_up-regulated-45.xlsx`, `…_down-regulated-185.xlsx`, `apex5_cax22_shoot_deg.xlsx` | XLSX |
| S13 | Col-0 vs *rbohD* ground-control contrasts (root, shoot) | `results/tables/apex5_col-vs-rbohd_root_gc-gc.xlsx`, `…_shoot_gc-gc.xlsx` | XLSX |
| S14 | Regulated-gene summaries (root/shoot × up/down) | `results/tables/apex5_{root,shoot}_{up,down}-regulated_summary.csv` | CSV |
| S15 | GO enrichment (all-roots; core-shoot bio/molecular function) | `results/tables/apex5_all-roots_2018-11-16_go-enrichment.tsv`, `apex5_core-shoot_2018-11-16_go-enrichment_*.tsv` | TSV |
| S16 | Root–shoot edgeR enrichment (GO CC/MF, pathway, protein domain) | `results/tables/apex5_root-shoot_edger_enrichment_*.tsv` | TSV |
| S17 | Shoot core enrichment | `results/tables/apex5_shoots_core-enrichment.csv` | CSV |
| S18 | Metascape gene lists (Col/cax2; cax23 root) | `results/tables/apex5_col-cax22-cax23_2022_metascape_*.csv`, `apex5_col_cax2_metascape_*.csv`, `data/genesets/apex05_cax23_root_metascape.*` | CSV/XLSX |
| S19 | jVenn transcript membership: roots vs shoots; WT vs *rbohD* (root, leaf) | `results/tables/apex5_roots-vs-shoots_jvenn_transcripts.csv`, `apex5_wt-vs-rbohd_root_jvenn.csv`, `apex5_wt-vs-rbohd_leaf_jvenn.csv` | CSV |
| S20 | Col-0 root Trichoderma-response gene overlap | `results/tables/apex5_col_root_trichoderma-response-genes.xls` | XLS |
| S21 | Transcription-factor summary (grouped) | `data/genesets/apex05_transcription-factor_summary.xlsx` | XLSX |
| S22 | Gene sets: iDEP GMT, Metascape/jVenn genes, *rbohD* AraCyc cell-wall list | `data/genesets/apex05_idep_gmt.txt`, `apex05_metascape_jvenn_genes.txt`, `apex05_rbohd_aracyc_cellwall.txt` | TXT |
| S23 | Root system architecture — day-4 primary-root RSML morphometrics | `data/morphometrics/apex05_rsml_day4_morphometrics.csv` | CSV |
| S24 | Root system architecture — WT vs *rbohD* RSML export | `data/morphometrics/apex05_wt-rbohd_rsml_roots.csv` | CSV |
| S25 | Machine-learning QC — per-well outlier & CV-mismatch scores | `results/ml/tableB_well_outlier_scores.csv` | CSV |
| S26 | Machine-learning QC — headline metrics | `results/ml/ml_metrics.json` | JSON |

## Supplementary figures
See [`../figure_legends.md`](../figure_legends.md) (Figures S1–S4).

## Provenance & data integrity
- Sample-labelling anomaly and its resolution: [`../../docs/PROVENANCE_cax23_mislabelling.md`](../../docs/PROVENANCE_cax23_mislabelling.md)
- Historical Targets-File revisions (V3–V13): `archive/targets_file_versions/`
- Superseded / duplicate artefacts retained for provenance: `archive/superseded/`

## Notes
- Some DEG workbooks contain multiple sheets (raw list, Entrez, UniProt);
  `_list-entrez` / `_list-uniprot` suffixes denote ID-mapped exports.
- `[TO CONFIRM]` in the manuscript indicates a value not yet verified against
  these tables; reconcile each against the file cited above before submission.
