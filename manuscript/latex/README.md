# LaTeX manuscript (npj Microgravity / Springer Nature style)

Binds the APEX-05 manuscript parts (`apex05_FINAL_manuscript.md`,
`figure_legends.md`) into the official **Springer Nature LaTeX template**
(`sn-jnl` class) — the format npj Microgravity accepts and typesets from.

```
latex/
├── main.tex          # assembled manuscript (sn-jnl class)
├── references.bib    # EMPTY stub (the source has no citations yet)
├── figures/          # the 19 mapped figures (main + supplementary, PNG)
└── README.md         # this file
```

## How to compile

`sn-jnl.cls` / `sn-nature.bst` ship with Springer Nature's official template
(not vendored here).

- **Overleaf (recommended):** new project from the **"Springer Nature Article
  Template (sn-jnl)"** → replace its `main.tex` with this one, upload
  `references.bib` and `figures/`, compile with **pdfLaTeX**.
- **Local:** place `sn-jnl.cls` + `sn-nature.bst` here, then
  `pdflatex main` → `pdflatex main` (bibtex only once you add references).

## Status / TODO before submission

This manuscript is a **scaffold** in places — those markers are carried over
faithfully from the source, not invented:

- [x] **Figure 2** — now rendered as a native `pgfplots` diverging bar chart
      straight from `results/tables/deseq2/apex05_deseq2_DEG_counts.csv` (vector, no
      external image). The same counts are in Table 1.
- [x] **Tool/database citations** (Methods) — DESeq2, PyDESeq2, LASSO, PCMDB,
      ggPlantmap, ggkegg, KEGG, g:Profiler, HISAT, Plant Reactome — added to
      `references.bib` (verified) and cited; `\bibliography` is now enabled.
- [x] **Background/scientific citations** — verified domain references added and
      cited in the Introduction: spaceflight transcriptomics (Paul et al. 2013),
      CAX2 calcium signalling (Shigaki 2003; Pittman 2005), RBOHD/ROS (Torres 2002;
      Miller 2009). Further references may be added as the scaffold prose is expanded.
- [ ] **Author block, Acknowledgements, and many Methods fields** are `[TO CONFIRM]`
      in the source (hardware, media, allele identifiers, OSDR accession, DOI).
- [ ] **Introduction & Discussion** are bullet scaffolds marked `[Scaffold — expand]`.
- [ ] **Not compile-tested** — authored without a local TeX install; build once on
      Overleaf and fix any stragglers.
- [ ] **Figures** — repo PNGs; npj prefers vector/≥300 dpi for final submission.

## Figure mapping

Main Figs 1–8 and Supplementary Figs S1–S5 are mapped 1:1 to `results/ml/fig*.png`
per `../figure_legends.md`. Supplementary figures use an `S`-prefixed counter.

## Source

Ported from `../apex05_FINAL_manuscript.md` and `../figure_legends.md`. All body
text, the DEG-count table, and figures are the author's own content — nothing was
invented.
