# APEX-05 — Concordance of the two CAX2 alleles (cax2-2 vs cax2-3)

**Analysis:** [`analysis/ml/apex05_cax2_allele_concordance.py`](../analysis/ml/apex05_cax2_allele_concordance.py)
**Outputs:** [`results/ml/figC1_cax2_deg_set_sizes.png`](../results/ml/figC1_cax2_deg_set_sizes.png),
[`results/ml/figC2_cax2_jaccard.png`](../results/ml/figC2_cax2_jaccard.png),
[`results/ml/cax2_concordance_metrics.json`](../results/ml/cax2_concordance_metrics.json)

---

## Why this analysis exists

The repo codes **`cax22`** and **`cax23`** are shorthand for **`cax2-2`** and
**`cax2-3`** — **two independent mutant alleles of the *same* gene, `CAX2`**
(a vacuolar cation/H⁺ exchanger). They are *not* mutations in two different
genes (a natural but incorrect reading of the shorthand as "CAX2" vs "CAX3").

Because they disrupt the same gene, the two alleles should produce **broadly
concordant** spaceflight (FL vs GC) transcriptome responses: comparable numbers
of differentially expressed genes (DEGs), a DEG set that overlaps each other
more than it overlaps unrelated genotypes, and agreeing fold-change directions.
The PI's observation that "the RNA-seq comparing cax2-2 and cax2-3 is very
different" is therefore a red flag worth quantifying before deciding whether to
retain cax2-3.

## Data & method

Released tables only (all in this repo):

- **DEG set overlap** — the jVenn/UpSet "CAX23oddity" membership exports
  (`archive/diagnostics/apex05_roots_v11_cax23-oddity.csv`; shoot twin in
  `results/tables/…`). Each genotype's full DEG set is reconstructed as the union
  of the exclusive-region columns naming it, then pairwise **Jaccard** overlap is
  computed at the locus level.
- **DEG count cross-check** — the cax2-3 full FL-vs-GC contrast tables
  (`archive/excluded_cax2-3/apex05_cax23_{root,shoot}_fl-vs-gc_full.csv`)
  thresholded at `padj < 0.05 & |log2FC| > 1`.
- **Direction concordance** — cax2-2 DEG identities/directions from the up/down
  workbooks (`results/tables/apex5_cax22_{root,shoot}_{up,down}-regulated-*.xlsx`)
  intersected with the thresholded cax2-3 DEG set; agreement of fold-change sign.

> **Caveat.** These tables are from the **V11 era — before** the final
> 60-sample correction — so they describe cax2-3 *as flagged*. That is exactly
> the state under evaluation. Two independent thresholds (jVenn export vs
> `padj/log2FC`) give the same qualitative picture.

## Results

### 1. cax2-3 has a grossly inflated DEG set

| Genotype | Root DEGs | Shoot DEGs |
| :-- | --: | --: |
| Col-0 | 767 | 650 |
| **cax2-2** | **473** | **174** |
| **cax2-3** | **4 539** | **2 980** |
| rbohD | 619 | 376 |

cax2-3's flight-responsive DEG set is **9.6× (root) / 17.1× (shoot)** the size of
its sibling allele cax2-2 — and larger than *every* other genotype, including
wild type. Independently thresholding the cax2-3 full contrast tables gives
**1 280 root / 1 670 shoot** DEGs (still 4–7× cax2-2's 324 / 230). See
`figC1_cax2_deg_set_sizes.png`.

### 2. The two alleles are NOT each other's closest match

Pairwise DEG-set Jaccard (`figC2_cax2_jaccard.png`):

| Pair | Root J | Shoot J |
| :-- | --: | --: |
| **cax2-2 ↔ cax2-3** (same gene!) | **0.069** | **0.045** |
| cax2-2 ↔ Col-0 | 0.082 | 0.072 |
| cax2-2 ↔ rbohD | 0.072 | 0.118 |
| Col-0 ↔ rbohD | 0.331 | 0.236 |

cax2-2 overlaps its supposed sibling allele **no better than — in shoot, less
than half as well as — it overlaps the unrelated genotypes**. (Jaccard is
deflated by the size asymmetry; the size-robust overlap coefficient
|cax2-2 ∩ cax2-3| / |cax2-2| is ≈ 0.67 root / 0.66 shoot — see below.)

### 3. …but the small shared core is genuine

Of the genes DE in **both** alleles (216 root, 151 shoot — i.e. ~2/3 of cax2-2's
DEGs), fold-change direction agrees **97.2% (root) / 96.7% (shoot)**. So a real,
directionally consistent CAX2 signal exists — it is simply **buried under a
~10–17× excess of cax2-3-unique DEGs** that no other genotype shares.

## Interpretation

The pattern — a small, high-confidence, direction-concordant shared core plus a
massive, non-preferential, cax2-3-exclusive DEG excess — is the signature of
**one allele's libraries carrying large technical / background variance on top
of the true genotype signal**, not of a genuine biological divergence between
two alleles of one gene. It is fully consistent with the independently
documented adjacent-well labelling anomaly
([`docs/PROVENANCE_cax23_mislabelling.md`](PROVENANCE_cax23_mislabelling.md)):
cax2-3 sat beside cax2-2 on the plate, and its DEG set-membership is what the
original "CAX23oddity" QC screen flagged.

## Re-verification on the CORRECTED matrix (2026-07-09)

When the fixed all-genotype count matrix (`run1 v2.2 fix2`) became available, the
concordance was **re-tested with fresh DESeq2** on the corrected data
(`analysis/ml/apex05_deseq2_flight.py` → `apex05_cax2_concordance_fixed.py`,
Fig. F1) rather than relying on the pre-fix tables. The result is unchanged and
in fact stronger:

| Tissue | Col-0 DEG | cax2-2 DEG | cax2-3 DEG | cax2-3 / cax2-2 |
| :-- | --: | --: | --: | --: |
| Root | 284 | 5 | 518 | **104×** |
| Shoot | 168 | 3 | 265 | **88×** |

cax2-3 still has a grossly inflated DEG set and near-zero preferential overlap
with cax2-2 (Jaccard ≤ 0.002), and a QC PCA shows a cax2-3 outlier library. **The
exclusion stands on corrected data.**

> **New finding (corrected data): cax2-2 has a strongly attenuated flight
> response.** On the fixed matrix cax2-2 shows only **5 root / 3 shoot** DEGs
> (15/4 even at a relaxed 1.5-fold, padj<0.1 threshold) vs Col-0's 284/168, and
> its FL-vs-GC PCA centroid separation is ~5× smaller than Col-0's (7.9 vs 39.9
> root). This is a genuine biological signal (clean tissue structure, no cax2-2
> outliers), **not** the technical inflation seen in cax2-3 — consistent with
> *CAX2* being required for the normal spaceflight transcriptional response.
> Note this differs sharply from the legacy edgeR workbook counts (cax2-2 ~323
> DEGs); the corrected DESeq2 on the fixed matrix supersedes those. See
> `results/ml/qc_summary.json`, `results/tables/deseq2/`.

## Decision (recorded 2026-07-09)

**cax2-3 is excluded from the APEX-05 primary analysis; cax2-2 is reported as the
*CAX2* representative.** This concordance analysis is retained as the QC
justification.

Implementation:

- cax2-3's derived primary-analysis result tables were relocated to
  [`../archive/excluded_cax2-3/`](../archive/excluded_cax2-3/) (retained, not
  deleted, for auditability — see that folder's README).
- The primary genotype set throughout the manuscript and README is now **Col-0,
  cax2-2, rbohD**.
- cax2-3 is **deliberately retained inside the two QC analyses that justify the
  exclusion** — this concordance analysis and the morphometric genotype-confusion
  QC (`apex05_ml_anomaly_detection.py`, Part B) — because removing it there would
  erase the evidence.
- The raw cax2-3 measurements remain in `data/` (shared per-genotype targets and
  morphometrics); only cax2-3's *derived result tables* were moved.

> **If the decision is revisited:** option (2) — restricting cax2-3 to its
> cax2-2-concordant core (~216 root / 151 shoot genes, 97% direction-agreeing) —
> remains available and can be reinstated from the retained files.

## Concordant-core variant (the high-confidence *CAX2* signature)

As a conservative alternative to full exclusion, we also extracted the **CAX2
concordant core**: genes differentially expressed under spaceflight in **both**
alleles **with agreeing fold-change direction**. This is the part of the cax2-3
signal that cax2-2 corroborates — the defensible *CAX2* flight signature.

**Analysis:** [`analysis/ml/apex05_cax2_concordant_core.py`](../analysis/ml/apex05_cax2_concordant_core.py)
**Outputs:** `results/tables/apex05_cax2_concordant-core_{root,shoot,combined}.csv`,
`results/ml/figC3_cax2_concordant_core.png`, `results/ml/cax2_concordant_core_summary.json`

Direction handling is explicit, not assumed: cax2-2's flight fold-change comes
from its workbook's **labelled** `baseMeanA_FL` / `baseMeanB_GC` columns
(log2 FL/GC), and cax2-3's orientation is resolved **empirically** against
cax2-2 — it came out **flipped** (cax2-3's raw `log2FoldChange` is log2 GC/FL).

| Tissue | shared DEGs | **concordant core** | up in flight | down | direction agreement |
| :-- | --: | --: | --: | --: | --: |
| Root | 216 | **210** | 203 | 7 | 97% |
| Shoot | 151 | **146** | 144 | 2 | 97% |

Two features stand out: the core is **overwhelmingly flight-*induced*** (≈97% up)
and **entirely tissue-partitioned** (0 loci shared between the root and shoot
cores). cax2-3's fold-changes are systematically larger than cax2-2's (points
above the identity line in Fig. C3) — the same inflation seen at the set level —
but their **direction** agrees, which is why this core is trustworthy. Example
core genes include *LHB1B1* (light harvesting), *WRKY51*, *AP2*, *MC3*
(metacaspase) and an invertase.

This core is released as a supplementary table and can serve as the *CAX2*
flight signature in place of the discarded genotype-level cax2-3 analysis.

### Functional enrichment of the core

Over-representation analysis
([`analysis/ml/apex05_cax2_core_enrichment.py`](../analysis/ml/apex05_cax2_core_enrichment.py);
g:Profiler, g:SCS-corrected *p* < 0.05, background = tissue-specific tested genes;
Fig. C4) shows the tissues respond through distinct programmes:

| Core | Top enriched biology | Example term (adj *p*) |
| :-- | :-- | :-- |
| **Root** (92 terms) | photosynthesis / thylakoid / chloroplast | GO:CC thylakoid (9×10⁻⁵⁸); photosynthesis; photosystem I |
| **Shoot** (32 terms) | cell-wall organisation; phenylpropanoid biosynthesis; peroxidase / H₂O₂ catabolism | plant-type cell wall organisation (1×10⁻¹⁰); KEGG phenylpropanoid biosynthesis; lactoperoxidase activity |

The shoot core's peroxidase / hydrogen-peroxide terms connect the *CAX2* flight
response to the same ROS axis probed by *rbohD*. Full term lists:
`results/tables/apex05_cax2_core_enrichment_{root,shoot,all}.csv`.
