# APEX-05 — Figure legends

Legends for the main-text and machine-learning QC figures. Each entry names the
source file(s) in this repository so figures can be regenerated or traced.
Values marked `[TO CONFIRM]` must be verified against the source data before
submission.

---

## Main figures

**Figure 1 | Spaceflight reduces wild-type leaf area and the ROS/Ca²⁺ mutants
respond differently.**
Violin plots of rosette leaf area (cm²) at [TO CONFIRM: 8 days] for Col-0,
*rbohD*, *cax22* (cax2.2) and *cax23* (cax2.3) under ground control (GC) and
spaceflight (FL). Horizontal bars, group means; dashed line, [TO CONFIRM:
grand mean / wild-type GC reference]. Wild-type leaf area is reduced under
flight (~4.0 → ~2.7 cm² [TO CONFIRM]); the mutants show an altered
genotype × environment response. *n* = [TO CONFIRM] plants per group.
*Source:* `reports/apex05_storyboard.png` (regenerate from the underlying
leaf-area table once added to `data/`).

**Figure 2 | Flight-responsive differential expression across genotypes and
tissues.**
[TO CONFIRM: chosen visualisation — e.g. bar plot of up/down DEG counts per
genotype × tissue, or volcano panels.] DEG counts (FL vs GC; [TO CONFIRM:
FDR/|log2FC| cut-offs]): Col-0 root 531↑/268↓, Col-0 shoot 403↑/201↓, *rbohD*
root 393↑/233↓, *rbohD* shoot 170↑/286↓, *cax22* root 100↑/223↓, *cax22* shoot
45↑/185↓.
*Source:* `results/tables/apex5_*_edger_*regulated-*.xlsx`,
`results/tables/apex5_edger_deg_all-samples.csv`.

**Figure 3 | Root and shoot deploy largely distinct flight-responsive
programmes.**
UpSet / Venn comparison of up- and down-regulated gene sets between root and
shoot (and across genotypes). [TO CONFIRM: describe the largest intersections
and the organ-specific fractions.]
*Source:* `results/plots/apex05_col-cax22-cax23_upset.svg`,
`results/plots/apex05_roots-vs-shoots_jvenn_transcripts.png`,
`results/tables/apex5_roots-vs-shoots_jvenn_transcripts.csv`.

**Figure 4 | Cell-wall and ROS pathways in the *rbohD* flight response.**
AraCyc/KEGG pathway projection of the *rbohD* root flight response, highlighting
cell-wall biosynthesis and [TO CONFIRM] pathways. [TO CONFIRM: legend detail on
colour scale = log2 fold change.]
*Source:* `results/plots/apex05_rbohd_roots_aracyc.png`,
`data/genesets/apex05_rbohd_aracyc_cellwall.txt`.

**Figure 5 | Machine-learning quality control detects mislabelled samples and
validates the corrected design.**
Produced by `analysis/ml/apex05_ml_anomaly_detection.py` (scikit-learn; seed 42).
**(a)** PCA of Col-0 transcriptomes (log-CPM, 8,068 genes) coloured by tissue;
root and shoot separate completely (leave-one-out classifier accuracy 100%).
**(b)** Distribution of the per-sample mislabel score (1 − P[stated label]) over
200 controlled label-swap injections; deliberately swapped samples (mean 0.98)
separate cleanly from correctly-labelled samples (mean 0.09), recovered at ~97%
recall. **(c)** Leave-one-well-out genotype confusion matrix from aggregated
day-4 primary-root RSML morphometrics (48 well × treatment profiles); overall
accuracy ~44% (chance 25%), i.e. genotypes overlap in morphometric space.
**(d)** PCA of the aggregated morphometric space by genotype, showing the same
overlap. Panels (c–d) establish that labels cannot be rescued from phenotype,
motivating the provenance-based (well-ID) correction described in the text.
*Source panels:* `results/ml/figA1_tissue_expression_pca.png`,
`figA2_expression_recovery_scores.png`, `figB1_genotype_confusion_matrix.png`,
`figB2_morphometric_pca.png`; metrics in `results/ml/ml_metrics.json`.

**Figure 6 | The two *CAX2* alleles are transcriptionally discordant.**
Produced by `analysis/ml/apex05_cax2_allele_concordance.py`. *cax2-2* (`cax22`)
and *cax2-3* (`cax23`) are two independent mutant alleles of the same gene,
*CAX2*, and should respond alike to spaceflight. **(a)** Flight-responsive DEG
set size per genotype (root and shoot): *cax2-3* is inflated 9.6× (root) / 17.1×
(shoot) relative to *cax2-2* and exceeds all other genotypes. **(b)** Pairwise
DEG-set Jaccard heatmaps (root, shoot); *cax2-2* overlaps *cax2-3* no more than
it overlaps the unrelated Col-0 and *rbohD*. Of genes DE in both alleles,
fold-change direction agrees 97%, indicating a small genuine shared core beneath
a large *cax2-3*-specific excess. Interpreted as technical/background variance in
the *cax2-3* libraries rather than a real allelic divergence.
*Source panels:* `results/ml/figC1_cax2_deg_set_sizes.png`,
`figC2_cax2_jaccard.png`; metrics in `results/ml/cax2_concordance_metrics.json`;
full write-up in `docs/cax2_allele_concordance.md`.

---

## Supplementary figures

**Figure S1 | CAX23oddity diagnostic that first flagged the labelling anomaly.**
jVenn/UpSet DEG set-membership across the four root genotypes; *cax23*'s overlap
pattern is inconsistent with a clean genotype cluster.
*Source:* `archive/diagnostics/apex05_roots_v11_cax23-oddity.png`,
`results/plots/apex05_cax23_shoot_v11_cax23-oddity.png`.

**Figure S2 | WT-vs-*rbohD* differential expression (jVenn).**
Root and leaf/shoot overlaps between wild type and *rbohD*.
*Source:* `results/plots/apex05_root_wt-vs-rbohd_jvenn.png`,
`results/plots/apex05_leaf_wt-vs-rbohd_jvenn.png`.

**Figure S3 | Per-genotype/pathway summary panels.**
[TO CONFIRM: curate from `results/plots/apex5_col0.png`, `apex5_cipk20.png`,
`apex5_genotype-plot.png`, `apex5_upsetr-plot.png`, and the DESeq/edgeR summary
PDFs in `archive/superseded/`.]

**Figure S4 | Root system architecture example and time-lapse.**
Representative Col-0 GC/FL root imaging.
*Source:* `media/apex05_col0_gc-fl_timelapse_1080p.mov`,
`data/morphometrics/apex05_rsml_source_col-cax22.zip`.
