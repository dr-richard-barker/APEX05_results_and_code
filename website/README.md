# APEX-05 project website

A self-contained static site (GitHub Pages) presenting the manuscript, figures,
and CAX2 QC story. **Live site:** https://dr-richard-barker.github.io/APEX05_results_and_code/

## How it works

- [`build_site.py`](build_site.py) renders the Markdown sources
  (`manuscript/apex05_manuscript.md`, `manuscript/figure_legends.md`,
  `docs/cax2_allele_concordance.md`, `docs/PROVENANCE_cax23_mislabelling.md`) and
  the main figures into styled HTML under [`../docs/`](../docs).
- GitHub Pages serves `docs/` directly (Settings → Pages → *Deploy from a branch*
  → `main` / `/docs`). `docs/.nojekyll` disables Jekyll so the raw HTML/assets are
  served unchanged.
- No external build service is required — the generated site is committed.

## Rebuild after editing the manuscript or figures

```bash
python -m pip install markdown          # one-time
python website/build_site.py            # regenerates docs/*.html + assets
git add docs && git commit -m "site: rebuild" && git push
```

The site updates within ~1 minute of the push.

## Preview locally

```bash
PORT=4173 python website/_serve.py      # serves docs/ at http://localhost:4173
```

## Editing the design

All styling lives in the `CSS` string in `build_site.py` (light/dark aware, no
external dependencies). Add figures to the `GALLERY` list there; navigation is the
`NAV` list.
