# APEX-05 project website

A self-contained static site (GitHub Pages) presenting the manuscript, figures,
and CAX2 QC story. **Live site:** https://dr-richard-barker.github.io/APEX05_results_and_code/

## How it works

- [`build_site.py`](build_site.py) renders the Markdown sources
  (`manuscript/apex05_FINAL_manuscript.md`, `manuscript/apex05_manuscript.md`,
  `manuscript/figure_legends.md`, `docs/cax2_allele_concordance.md`,
  `docs/PROVENANCE_cax23_mislabelling.md`) and the main figures into styled HTML
  under [`../docs/`](../docs).
- **Automatic deploy.** A GitHub Actions workflow
  ([`.github/workflows/pages.yml`](../.github/workflows/pages.yml)) rebuilds the
  site with `build_site.py` and deploys it to Pages on every push to `main` that
  touches the manuscript, figures, or site code. (Pages source =
  *GitHub Actions*.) No manual rebuild/commit of `docs/` is needed.

## Editing the manuscript or figures

Just edit the source and push to `main`:

```bash
# edit manuscript/apex05_FINAL_manuscript.md (or figures under results/ml/) ...
git commit -am "manuscript: ..." && git push      # Actions rebuilds + deploys
```

The live site updates a minute or two later. To preview the exact output locally
before pushing, run `python website/build_site.py` and open `docs/`.

## Preview locally

```bash
PORT=4173 python website/_serve.py      # serves docs/ at http://localhost:4173
```

## Editing the design

All styling lives in the `CSS` string in `build_site.py` (light/dark aware, no
external dependencies). Add figures to the `GALLERY` list there; navigation is the
`NAV` list.
