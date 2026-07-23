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

- [ ] **References** — the source References section is `[TO CONFIRM]` (no
      citations yet). `references.bib` is intentionally empty. Add entries, insert
      `\cite{}` at the `[TO CONFIRM: citations]` points in Introduction / Methods /
      Discussion, then uncomment `\bibliography{references}`.
- [ ] **Author block, Acknowledgements, and many Methods fields** are `[TO CONFIRM]`
      in the source (hardware, media, allele identifiers, OSDR accession, DOI).
- [ ] **Introduction & Discussion** are bullet scaffolds marked `[Scaffold — expand]`.
- [ ] **Figure 2** has no rendered image in the repo (its source is a CSV). It is a
      clearly-marked placeholder box; the same DEG-count data are tabulated in
      Table 1. Render the panel from `results/tables/deseq2/apex05_deseq2_DEG_counts.csv`
      and replace the placeholder.
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
