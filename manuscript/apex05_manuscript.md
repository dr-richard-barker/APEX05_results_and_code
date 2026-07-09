---
title: "Reactive-oxygen and calcium signalling shape the *Arabidopsis* transcriptomic and root-architectural response to spaceflight (APEX-05)"
running_title: "ROS and Ca2+ signalling in the APEX-05 spaceflight response"
target_journal: "npj Microgravity (Article)"
manuscript_status: "DRAFT SCAFFOLD — assembled from released data, storyboard, and reanalysis outputs. Passages marked [TO CONFIRM] require the authors to insert or verify exact values before submission; no quantitative claim here should be treated as final until checked against the source tables."
authors:
  - name: "Richard Barker"
    affiliation: 1
    corresponding: true
    email: "admin@cosecloud.com"
  - name: "[TO CONFIRM: co-authors, ORCID iDs, affiliations]"
affiliations:
  - id: 1
    name: "[TO CONFIRM: institution, city, country]"
keywords: [spaceflight, Arabidopsis thaliana, transcriptomics, root system architecture, reactive oxygen species, RBOHD, calcium, CAX, FAIR data]
---

> **Editorial note (remove before submission).** This file is a *scaffold*. Its
> structure and section flow follow npj Microgravity's Article format. Numerical
> results are drawn directly from the released tables in this repository and are
> cited by filename; where a number could not be verified from the released
> files it is written as `[TO CONFIRM]`. The machine-learning quality-control
> results (Fig. 5) were produced by `analysis/ml/apex05_ml_anomaly_detection.py`
> in this repository and are reproducible from `results/ml/ml_metrics.json`.

---

## Abstract

Spaceflight imposes a unique combination of microgravity and space-radiation
stress on plants, yet the signalling pathways that translate the orbital
environment into altered growth remain incompletely mapped. The Advanced Plant
Experiment-05 (APEX-05) flew four *Arabidopsis thaliana* genotypes — wild-type
Col-0, a mutant in reactive-oxygen (ROS) production (*rbohD*), and **two
independent alleles of the calcium/H⁺ exchanger *CAX2* (*cax2-2* and *cax2-3*)**
— aboard the International Space Station, alongside
matched ground controls, and profiled both the shoot and root transcriptome
(RNA-seq) and root system architecture (RSML morphometrics). A QC concordance
test between the two *CAX2* alleles found *cax2-3* strongly discordant with its
sibling *cax2-2* (DEG set ~10–17× inflated and no more similar to cax2-2 than to
unrelated genotypes); *cax2-3* was therefore excluded, leaving a primary set of
three genotypes (Col-0, *cax2-2*, *rbohD*). Wild-type rosette
leaf area was reduced under spaceflight [TO CONFIRM: exact means/CI from the
leaf-area analysis; storyboard indicates ~4.0 cm² ground control vs ~2.7 cm²
flight], and this response was altered in the ROS- and calcium-signalling
mutants, implicating both pathways in the spaceflight growth response.
Differential-expression analysis identified hundreds of flight-responsive genes
in each genotype and tissue (e.g. Col-0 root: 531 up / 268 down; Col-0 shoot:
403 up / 201 down; see Table 1), enriched for [TO CONFIRM: top GO/KEGG terms from
`results/tables/apex5_*enrichment*`]. Beyond the biology, we release APEX-05 as a
FAIR, reproducible data package and demonstrate a machine-learning
quality-control workflow that (i) recovers tissue identity from expression with
100% leave-one-out accuracy and detects deliberately mislabelled samples at ~97%
recall, and (ii) shows that day-4 primary-root morphology only weakly
distinguishes these subtle mutants — the finding that motivated our provenance-
based correction of a historical *cax23* sample-labelling anomaly. Together the
data and tooling provide a reusable resource for the plant space-biology
community.

---

## Introduction

[Scaffold — 3–4 paragraphs. Suggested content, to be written/verified by authors:]

- **The problem.** Plants are candidate life-support organisms for long-duration
  spaceflight, but microgravity perturbs gravitropic, hydrotropic, oxidative and
  cell-wall processes. Prior spaceflight transcriptomics (e.g. the GeneLab /
  Open Science Data Repository corpus) established broad reprogramming but the
  causal signalling nodes are debated. [TO CONFIRM: citations.]

- **The candidate pathways.** Reactive oxygen species produced by the NADPH
  oxidase RBOHD, and calcium fluxes mediated by CAX-family exchangers, are
  central to plant environmental sensing and have been proposed as early relays
  in the spaceflight response. APEX-05 was designed to test their contribution
  directly by flying *rbohD* and **two independent alleles of the *same* gene
  *CAX2* — *cax2-2* and *cax2-3*** — against Col-0. Including two alleles of one
  gene provides a built-in reproducibility control: concordant responses
  corroborate a genuine *CAX2* phenotype, whereas divergence flags an allele- or
  sample-specific artefact. [TO CONFIRM: precise allele identifiers / T-DNA
  insertion lines and prior hypotheses.]

- **This study.** We report the paired shoot/root transcriptome and root-
  architecture response, and — reflecting community priorities around
  reproducibility — package the analysis to FAIR standards and add an
  independent machine-learning QC layer. We use that layer to document and
  resolve a sample-labelling anomaly that arose because *cax22* and *cax23* were
  grown in physically adjacent plate wells.

---

## Results

### 1. Spaceflight reduces wild-type leaf area and the mutants respond differently

Rosette leaf area measured at [TO CONFIRM: 8 days] showed a reduction in
wild-type Col-0 under spaceflight relative to ground control (Fig. 1). The
ROS-signalling mutant *rbohD* and the calcium-transport mutants *cax22*/*cax23*
displayed [TO CONFIRM: describe the direction and magnitude of the genotype ×
environment interaction from the leaf-area dataset]. This phenotype frames the
transcriptomic analysis that follows: if ROS and Ca²⁺ signalling gate the
growth response, the flight-responsive transcriptomes of these mutants should
diverge from wild type.

*Source:* `reports/apex05_storyboard.png`; leaf-area measurements [TO CONFIRM:
add the underlying leaf-area table to `data/` and cite it here].

### 2. Hundreds of genes respond to spaceflight in each genotype and tissue

Flight-versus-ground-control differential expression (edgeR; [TO CONFIRM:
thresholds, e.g. FDR < 0.05, |log2FC| > 1]) identified robust, tissue-specific
responses (Table 1, Fig. 2). In wild-type Col-0 we detected 531 up- and 268
down-regulated genes in root and 403 up- and 201 down-regulated genes in shoot.
The mutants showed responses of comparable scale but distinct composition:
*rbohD* (root 393 up / 233 down; shoot 170 up / 286 down) and *cax22* (root 100
up / 223 down; shoot 45 up / 185 down). [TO CONFIRM: cax23 counts once the
corrected contrast is finalised — see §5 and the provenance document.]

*Source tables:* `results/tables/apex5_col_root_edger_*`,
`apex5_col_shoot_edger_*`, `apex5_rbohd_root_*`, `apex5_rbohd_shoot_*`,
`apex5_cax22_root_*`, `apex5_cax22_shoot_*`, and the combined
`apex5_edger_deg_all-samples.csv`.

### 3. Root and shoot deploy largely distinct flight-responsive programmes

Comparison of root versus shoot responses (Fig. 3; jVenn/UpSet in
`results/plots/apex05_col-cax22-cax23_upset.svg` and
`results/tables/apex5_roots-vs-shoots_jvenn_transcripts.csv`) showed [TO CONFIRM:
the overlap statistics — number of shared vs organ-specific DEGs]. Functional
enrichment (Table 2) implicated [TO CONFIRM: top terms from
`apex5_root-shoot_edger_enrichment_*.tsv`, `apex5_shoots_core-enrichment.csv`],
consistent with organ-specialised remodelling of [TO CONFIRM: e.g. cell-wall,
photosynthesis, defence] processes.

### 4. Cell-wall and ROS pathways are enriched in the *rbohD* response

Mapping the *rbohD* flight response onto AraCyc/KEGG pathways
(`results/plots/apex05_rbohd_roots_aracyc.png`,
`data/genesets/apex05_rbohd_aracyc_cellwall.txt`) highlighted cell-wall
biosynthesis and [TO CONFIRM] pathways, linking altered ROS production to
structural remodelling under spaceflight. [TO CONFIRM: quantitative pathway
enrichment statistics.]

### 5. Machine-learning quality control detects mislabelled samples and validates the corrected design

Because *cax2-2* and *cax2-3* were grown in physically adjacent plate wells, an
early QC screen flagged a *cax2-3* labelling anomaly (the "CAX23oddity"; full
history in `docs/PROVENANCE_cax23_mislabelling.md`). We built an independent
machine-learning check (`analysis/ml/apex05_ml_anomaly_detection.py`, Fig. 5):

- **Expression carries an unambiguous tissue signal.** A leave-one-out
  classifier separated Col-0 root from shoot transcriptomes with **100%**
  accuracy (16 samples, 8,068 shared genes). Injecting known label swaps and
  asking the detector to recover them yielded **~97%** recall (top-2 of 200
  trials) with near-complete score separation between swapped (mean 0.98) and
  correctly-labelled (mean 0.09) samples (Fig. 5a,b). This is the modality in
  which a mislabelled sample is detectable — the same principle that surfaced
  the original oddity.

- **Root morphology alone cannot re-derive genotype.** A leave-one-well-out
  classifier on aggregated RSML primary-root features reached only **~44%**
  genotype accuracy (chance 25%; Fig. 5c,d), with the genotypes overlapping in
  morphometric space. This negative result is informative: because these subtle
  ROS/Ca²⁺ mutants are not phenotypically separable at day 4, labels cannot be
  rescued from phenotype, and the correct remedy is **provenance-based** — we
  re-keyed every sample to its plate-well ID, removing the ambiguous *cax22*
  well A7 and the anomalous *cax23* ground-control replicate to yield the clean
  60-sample master design.

All metrics are reproducible from `results/ml/ml_metrics.json` (seed 42).

### 6. The two *CAX2* alleles are transcriptionally discordant

Because *cax2-2* and *cax2-3* disrupt the **same gene**, their spaceflight
responses should agree. Instead, a concordance analysis of the released DEG sets
(`analysis/ml/apex05_cax2_allele_concordance.py`, Fig. 6) found *cax2-3* with a
grossly inflated flight-response — **4,539 root / 2,980 shoot** DEGs versus
**473 / 174** for *cax2-2*, i.e. **9.6× / 17.1×** larger and exceeding every
other genotype including wild type (Fig. 6a). Critically, *cax2-2* overlapped its
own sibling allele **no more than it overlapped the unrelated genotypes** (DEG-set
Jaccard 0.069 root / 0.045 shoot vs 0.072–0.118 for Col-0 and *rbohD*; Fig. 6b).
The genuine shared core is small but real: of genes DE in both alleles (216 root,
151 shoot), fold-change direction agreed **97%**. This pattern — a modest
direction-concordant core beneath a large, non-preferential *cax2-3*-exclusive
excess — indicates the *cax2-3* libraries carry substantial technical variance
rather than a real allelic divergence, consistent with the adjacent-well anomaly.
**We therefore excluded *cax2-3* from the primary analysis and report *cax2-2* as
the *CAX2* representative** (retained cax2-3 result tables:
`archive/excluded_cax2-3/`; decision record: `docs/cax2_allele_concordance.md`).
*cax2-3* is retained only within the QC analyses (this concordance test and the
morphometric confusion of Fig. 5) that justify its exclusion.

---

## Discussion

[Scaffold — to be written by authors. Key points supported by this repository:]

1. **Biology.** ROS (RBOHD) and calcium (CAX) signalling both modulate the
   *Arabidopsis* spaceflight response, evidenced by genotype-specific leaf-area
   phenotypes and divergent flight-responsive transcriptomes. [TO CONFIRM:
   integrate with prior spaceflight ROS/Ca²⁺ literature and OSDR datasets.]
2. **Organ specialisation.** Root and shoot mount largely separate programmes,
   arguing against a single systemic flight signal. [TO CONFIRM.]
3. **Reproducibility as a result.** Adjacent-well genotype confusion is a
   generic risk in multi-genotype spaceflight hardware; keying samples on
   physical position and validating with an ML QC layer is a transferable
   practice for the field.
4. **Limitations.** Per-sample expression matrices are released here for Col-0
   only; the mutant contrasts are provided as summary/DEG tables. [TO CONFIRM:
   note replicate numbers, power, and any batch structure.]

---

## Methods

[Scaffold — expand/verify. Anchor each subsection to the released files.]

- **Plant material and spaceflight hardware.** Four genotypes (Col-0, *rbohD*,
  *cax22*, *cax23*); [TO CONFIRM: growth hardware, light, media, flight profile,
  fixation]. Sample design in
  `data/metadata/apex05_sample_metadata.csv` (60 samples; genotype × tissue ×
  flight/ground × replicate, keyed on plate-well ID).
- **RNA-seq and read processing.** HiSeq sequencing; HISAT2 alignment;
  targets/BAM maps in `data/metadata/` (see also archived Targets File revisions
  in `archive/targets_file_versions/`). [TO CONFIRM: genome build, aligner
  version, counting.]
- **Differential expression.** edgeR and DESeq2 (`analysis/R/`), FL vs GC per
  genotype × tissue; [TO CONFIRM: thresholds]. Outlier-handling variant in
  `analysis/R/apex5_edger_outlier-removed-main-analysis.R`.
- **Functional enrichment.** GO/KEGG/AraCyc via [TO CONFIRM: tools — iDEP,
  Metascape, pathview]; inputs in `data/genesets/`, outputs in
  `results/tables/apex5_*enrichment*`.
- **Root system architecture.** RSML exported from Fiji/archidart
  (`analysis/R/apex5_drb_rsml-analysis.R`,
  `data/morphometrics/apex05_rsml_day4_morphometrics.csv`). [TO CONFIRM:
  imaging, day, traits.]
- **Machine-learning QC.** `analysis/ml/apex05_ml_anomaly_detection.py`
  (scikit-learn; leave-one-out / leave-one-well-out CV; IsolationForest; seed
  42). See `analysis/ml/README.md`.
- **Sample-label provenance.** The *cax23* anomaly and its resolution are
  documented in `docs/PROVENANCE_cax23_mislabelling.md`.

---

## Data availability

The complete data package — metadata, expression tables, root morphometrics,
gene sets, results and figures — is in this repository and archived at Zenodo
(DOI: [TO CONFIRM after Zenodo deposit]). Raw sequencing reads are available
from NASA OSDR / GeneLab under accession [TO CONFIRM: OSD-###].

## Code availability

All analysis code (R differential-expression/pathway pipelines and the Python ML
QC pipeline) is in `analysis/`, released under the MIT License. The ML results
are fully reproducible via `analysis/ml/apex05_ml_anomaly_detection.py`.

## Acknowledgements / Author contributions / Competing interests

[TO CONFIRM.]

## References

[TO CONFIRM — populate from the Introduction/Discussion citations.]
