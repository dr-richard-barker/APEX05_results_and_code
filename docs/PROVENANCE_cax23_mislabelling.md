# APEX-05 — Provenance of the *cax23* sample-labelling anomaly

This document reconstructs, from the archived analysis files, the sample
mislabelling that affected the APEX-05 RNA-seq dataset and how it was detected
and corrected. It is the authoritative record for reviewers and re-users of the
data package. All paths are relative to the repository root; the historical
files themselves are preserved under
[`archive/targets_file_versions/`](../archive/targets_file_versions) and
[`archive/diagnostics/`](../archive/diagnostics).

---

## 1. Executive summary

The APEX-05 growth plate carried the two Ca²⁺/H⁺-exchanger mutants **cax22** and
**cax23** in **physically adjacent wells** (well pairs A3↔A4, B4↔B3, B8↔B7,
A7↔A8). A quality-control screen of differentially-expressed-gene (DEG) overlap
— archived as the **"CAX23oddity"** diagnostic — showed *cax23* behaving
anomalously: its DEG set did not partition cleanly against the other genotypes,
the classic signature of an adjacent-well sample swap / cross-labelling between
cax22 and cax23.

Two corrective actions are visible in the file trail:

1. **During diagnosis (V12–V13):** the single odd replicate — **cax23
   Ground-Control replicate 1** (`GCRoot_CAX23_1` + `GCShoot_CAX23_1`) — was
   dropped, and a dedicated **cax22-vs-cax23 contrast** was built to test
   whether the two lines were separable at all.
2. **In the final published master:** the entire **cax22 well A7** (4 libraries:
   `A7_FL_Root/Shoot`, `A7_GC_Root/Shoot`) was removed, taking the study from
   **64 → 60 samples**, and the master was re-keyed to **plate-well sample IDs**
   so that any future genotype↔well mismatch is auditable.

---

## 2. Version-by-version trail

| File (in `archive/…` unless noted) | Samples | Genotypes | Notes / cax23-relevant change |
| :--- | :--: | :--- | :--- |
| `targets_file_versions/HiSat_APEX5_Targets_File_V3.txt` | 64 | all four (16 ea.) | Baseline; MiSeq-era `counts_out_V3`; **retains a `Location`/well column** — the adjacency evidence |
| `…_V4.txt` | 64 | all four | Column trim (Location dropped) |
| `…_V5.txt` | 64 | all four | Dot-delimited labels; Location restored |
| `APEX5_HiSat_Targets_File_V6.txt` / `…_V6.csv` | 64 | all four | Identical design to V5 |
| `APEX5_Hiseq_Hisat_Targets_V11.txt` | 64 | all four | **Switched to deeper HiSeq `counts_out_V8`**; re-indexed `FL/GC_Root/Shoot_<geno>_1..4` labels — the re-processed data in which the oddity surfaced |
| `diagnostics/apex05_roots_v11_cax23-oddity.csv` / `.png` (+ shoot twin in `results/`) | — | Root{CAX22,CAX23,COL00,RBOHD} + all intersections | **The diagnostic that named the anomaly**: jVenn/UpSet DEG set-membership showing cax23's odd overlap |
| `…_V11_cax22_v_cax23.txt` (+ transposed) | 48 | COL00, CAX22, CAX23 | Purpose-built **cax22-vs-cax23 contrast** (rbohD excluded) to test separability; cax23 still full 16 |
| `…_V12_cax23_test.txt` | 16 | CAX23 only | Isolates cax23 for standalone inspection |
| `…_V12_cax23_removed.txt` | 61 | all four; **cax23 = 14** | Drops the odd `GCRoot_CAX23_1` + `GCShoot_CAX23_1` pair (name is misleading — only the GC rep-1 pair is removed) |
| `…_V12_WTvRBOHD.txt` | 32 | COL00, RBOHD | Splits off the cax-independent WT-vs-rbohd analysis so it proceeds unaffected |
| `…_V13_cax22_v_cax23_2023.csv` (+ transposed variants) | 46 | COL00 (16), CAX22 (16), **CAX23 (14)** | **Formalised fix**: cax22-vs-cax23 contrast with cax23 GC rep-1 permanently excluded; adds explicit `tissue`/`genotype` columns |
| `…_V13_just_cax23.csv` / `…_JUST_cax23_transposed.csv` | 14 | CAX23 only | cax23-only, GC rep-1 already excluded |
| **`data/metadata/apex05_hisat_targets_master.txt`** + **`data/metadata/apex05_sample_metadata.csv`** | **60** | COL00 (16), **CAX22 (12)**, CAX23 (16), RBOHD (16) | **Published master**: `counts_out` final; **well-ID labels** (`A1_FL_Root`…); **cax22 well A7 removed** |

> **Filename caveat.** The `cax23_removed` names are inconsistent between epochs:
> `…_V12_cax23_removed.txt` keeps 14 cax23 libraries, but a same-named `.csv`
> that once sat in `data/` was actually a **COL00-only** subset, and
> `…_V12_NOcax23_JustGC.csv` is **COL00 ground-control only**. Do not treat the
> `.csv` and `.txt` "cax23_removed" files as equivalent.

---

## 3. The anomaly, in plain English

The MiSeq-era design (V3–V6) was a clean 4×4 layout — four genotypes × four
biological replicates × two organs × two conditions = 64 libraries. The
`Location` column of `HiSat_APEX5_Targets_File_V3.txt` records the source well,
and it shows that **cax22 and cax23 were pipetted from interleaved, adjacent
wells**:

```
cax22  →  B8, B4, A7, A3
cax23  →  B7, B3, A8, A4        # each cax22 well sits beside a cax23 well
```

This is exactly the plate geometry in which a hand-off swap or index cross-talk
between the two mutants is most likely. When the libraries were re-processed
against the deeper HiSeq counts (`counts_out_V8`, from V11 on), a DEG-overlap QC
export flagged the cax23 "oddity" (`archive/diagnostics/apex05_roots_v11_cax23-oddity.*`):
cax23's DEG membership did not separate cleanly from cax22/COL00/rbohD,
indicating at least one "cax23" library was not genotypically pure cax23.

---

## 4. Implicated samples

**(a) Odd replicate dropped during diagnosis (V12–V13):**
`GCRoot_CAX23_1` and `GCShoot_CAX23_1` — present in V11 (and in
`…_V11_cax22_v_cax23.txt`) but **absent from every V12/V13 cax23 file**, leaving
cax23 with 14 libraries in those revisions.

**(b) Well removed in the published master:**
`A7_FL_Root`, `A7_FL_Shoot`, `A7_GC_Root`, `A7_GC_Shoot` — the whole **cax22 well
A7**, physically adjacent to cax23 well A8. `A7` appears in the V3/V6 `Location`
columns but is **absent** from both `apex05_sample_metadata.csv` and
`apex05_hisat_targets_master.txt`. Its removal is why the master has **cax22 = 12
samples** (versus 16 for each other genotype) and why the final design is
**60 samples**.

> These are two different corrective actions at two stages, not a contradiction:
> the cax22↔cax23 adjacency investigation traced the "cax23 oddity" back to the
> neighbouring cax22 well (A7).

---

## 5. Resolution

1. **Detect** — HiSeq re-alignment (V11) + the CAX23oddity DEG-overlap diagnostic
   flag cax23 as misbehaving.
2. **Isolate & test** — build a dedicated cax22-vs-cax23 contrast
   (`…_V11_cax22_v_cax23`, then the V13 family) and a cax23-only view
   (`…_V12_cax23_test`); split off the cax-independent WT-vs-rbohd comparison so
   it is unaffected.
3. **Trim** — exclude the suspect cax23 GC replicate 1 from V12 onward; V13 bakes
   this in (cax23 = 14) and adds explicit `tissue`/`genotype` columns.
4. **Finalise** — re-key the master to **plate-well sample IDs**, drop the
   problematic cax22 well A7, and publish the clean **60-sample** design. Keying
   samples on well ID is the durable fix: it makes any genotype↔well mismatch
   auditable.

---

## 6. How the machine-learning package relates to this

`analysis/ml/apex05_ml_anomaly_detection.py` provides an independent,
data-driven check on the class of error described here:

- **Expression (Part A)** — Col-0 root vs shoot transcriptomes are almost
  perfectly separable (leave-one-out accuracy ≈ 1.0), so a swapped-label sample
  is *detectable* in this modality. A controlled injection test quantifies how
  reliably the detector recovers known label swaps — the same principle by which
  the original cax23 oddity was caught in transcriptomic space.
- **Morphometrics (Part B)** — day-4 primary-root architecture only weakly
  separates these subtle mutants (genotype accuracy well below what would be
  needed to re-derive labels), with cax22/cax23 among the most confused. This is
  the QC conclusion that **motivates provenance-based correction (well-ID
  keying) rather than post-hoc morphological rescue.**

See [`../results/ml/ml_metrics.json`](../results/ml/ml_metrics.json) for the
exact figures produced by the current data.
