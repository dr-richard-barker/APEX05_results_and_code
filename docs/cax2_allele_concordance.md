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
  (`data/expression/contrasts_full/apex05_cax23_{root,shoot}_fl-vs-gc_full.csv`)
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

## Recommendation (decision is the PI's)

The data support **treating cax2-3 as unreliable in its current form**. Three
defensible options, in order of increasing conservatism:

1. **Report cax2-2 as the CAX2 representative; drop cax2-3** from the primary
   flight-response analysis, and present this concordance analysis as the QC
   justification. *(Simplest; matches the PI's inclination.)*
2. **Restrict cax2-3 to its cax2-2-concordant core** (the ~216 root / 151 shoot
   shared, 97%-direction-agreeing DEGs) and analyse that intersection only.
3. **Retain cax2-3 but flag it**, adding this analysis as an explicit caveat.

If you choose option 1 or 2, I can regenerate the downstream DEG/enrichment/ML
outputs on the reduced genotype set (Col-0, cax2-2, rbohD [+ optional cax2-3
core]) and update the manuscript accordingly. **No genotype has been removed
yet** — this document only quantifies the case.
