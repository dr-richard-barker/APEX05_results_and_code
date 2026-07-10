#!/usr/bin/env python3
# =============================================================================
#  APEX-05 | Static website generator (GitHub Pages)
# =============================================================================
#
#  Renders the manuscript, figure legends and key provenance/QC documents into a
#  self-contained static site under docs/ (served by GitHub Pages, /docs source).
#  Re-run this whenever the manuscript or figures change:
#
#      python -m pip install markdown
#      python website/build_site.py
#      git add docs && git commit -m "site: rebuild" && git push
#
#  No external build service is required — the generated HTML/CSS/images are
#  committed and served directly (docs/.nojekyll disables Jekyll processing).
# =============================================================================

from __future__ import annotations
import re, shutil
from pathlib import Path

import markdown

REPO = Path(__file__).resolve().parents[1]
DOCS = REPO / "docs"                     # GitHub Pages source
ASSETS = DOCS / "assets"
FIGDIR = ASSETS / "figures"
SITE_TITLE = "APEX-05"
REPO_URL = "https://github.com/dr-richard-barker/APEX05_results_and_code"
MD_EXT = ["tables", "fenced_code", "toc", "attr_list", "sane_lists"]

NAV = [("index.html", "Home"), ("final-manuscript.html", "Final manuscript"),
       ("manuscript.html", "Scaffold"), ("figures.html", "Figures"),
       ("cax2-story.html", "CAX2 QC story"), (REPO_URL, "GitHub ↗")]

# Final-manuscript figures (corrected 64-sample, 3-genotype analysis).
FINAL_FIGS = [
    ("figG1_qc_pca.png", "Fig 1 — Corrected 64-sample design; PC1 (94%) = tissue, no outliers."),
    ("figE1_lasso_flight_signature.png", "Fig 3 — LASSO flight signature corroborates DESeq2."),
    ("figH1_celltype_deg_enrichment.png", "Fig 4 — Flight response localises to epidermis/columella/cortex; cax2-2 flat."),
    ("figM1_root_anatomy_flight.png", "Fig 5 — ggPlantmap: outer-root localisation (Col-0/rbohD), cax2-2 blank."),
    ("figI1_morphometric_flight_effects.png", "Fig 6a — Spaceflight shortens roots in all genotypes."),
    ("figL1_stress_resemblance.png", "Fig 7 — Resembles hypoxia / oxidative / defence stress."),
    ("figK1_kegg_pathway_enrichment.png", "Fig 8a — KEGG: glutathione + phenylpropanoid."),
    ("figJ_ath00940_col0_shoot.png", "Fig 8b — ggKEGG phenylpropanoid map, Col-0 shoot flight overlay."),
]

# Figures to surface in the gallery: (filename, caption).
GALLERY = [
    ("figA1_tissue_expression_pca.png", "Fig 5a — Col-0 transcriptomes separate cleanly by tissue (ML QC)."),
    ("figA2_expression_recovery_scores.png", "Fig 5b — Mislabel detector separates injected label-swaps (~97% recall)."),
    ("figB1_genotype_confusion_matrix.png", "Fig 5c — Genotype confusion from root morphometrics (~44%; overlap)."),
    ("figB2_morphometric_pca.png", "Fig 5d — Aggregated root-morphometric space by genotype."),
    ("figC1_cax2_deg_set_sizes.png", "Fig 6a — cax2-3's flight-DEG set is 10–17× inflated vs its sibling allele cax2-2."),
    ("figC2_cax2_jaccard.png", "Fig 6b — The two CAX2 alleles are not each other's closest match."),
    ("figC3_cax2_concordant_core.png", "Fig 7 — CAX2 concordant core: genes DE in both alleles, same direction."),
    ("figC4_cax2_core_enrichment.png", "Fig 8 — Core enrichment: root = photosynthesis; shoot = cell wall + peroxidase."),
    ("figD1_primary_genotype_enrichment.png", "Fig 9 — Genotype-resolved flight-DEG enrichment (Col-0, cax2-2, rbohD)."),
]

CSS = """
:root{
  --bg:#ffffff;--fg:#1a1a1a;--muted:#5a5a5a;--line:#e5e5e5;--card:#f7f7f8;
  --accent:#0072B2;--accent2:#D55E00;--code:#f2f2f4;--maxw:900px;
}
@media (prefers-color-scheme:dark){
  :root{--bg:#0f1115;--fg:#e8e8ea;--muted:#a0a0a8;--line:#2a2d34;--card:#171a20;
        --accent:#4aa3df;--accent2:#ff8a4c;--code:#12141a;}
}
*{box-sizing:border-box}
html{scroll-behavior:smooth}
body{margin:0;background:var(--bg);color:var(--fg);
  font:16px/1.65 -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,Helvetica,Arial,sans-serif;}
a{color:var(--accent);text-decoration:none}
a:hover{text-decoration:underline}
header.site{position:sticky;top:0;z-index:10;background:color-mix(in srgb,var(--bg) 88%,transparent);
  backdrop-filter:blur(8px);border-bottom:1px solid var(--line)}
nav{max-width:var(--maxw);margin:0 auto;display:flex;gap:1.1rem;flex-wrap:wrap;
  align-items:center;padding:.7rem 1.2rem}
nav .brand{font-weight:700;letter-spacing:.02em;margin-right:auto}
nav a{color:var(--fg);font-size:.93rem}
main{max-width:var(--maxw);margin:0 auto;padding:1.5rem 1.2rem 4rem}
.hero{padding:2.4rem 0 1rem;border-bottom:1px solid var(--line);margin-bottom:1.6rem}
.hero h1{font-size:2.05rem;line-height:1.2;margin:.2rem 0 .6rem}
.hero p.lede{font-size:1.12rem;color:var(--muted);margin:.3rem 0 1rem}
.badges{display:flex;gap:.5rem;flex-wrap:wrap;margin:.6rem 0}
.badge{font-size:.78rem;padding:.2rem .6rem;border:1px solid var(--line);border-radius:999px;
  background:var(--card);color:var(--muted)}
.btnrow{display:flex;gap:.6rem;flex-wrap:wrap;margin:1.1rem 0}
.btn{display:inline-block;padding:.5rem .95rem;border-radius:8px;border:1px solid var(--line);
  background:var(--card);color:var(--fg);font-size:.92rem;font-weight:600}
.btn.primary{background:var(--accent);color:#fff;border-color:var(--accent)}
.cards{display:grid;grid-template-columns:repeat(auto-fit,minmax(215px,1fr));gap:1rem;margin:1.4rem 0}
.card{background:var(--card);border:1px solid var(--line);border-radius:12px;padding:1rem 1.1rem}
.card h3{margin:.1rem 0 .35rem;font-size:1.02rem}
.card p{margin:0;color:var(--muted);font-size:.92rem}
.k{color:var(--accent2);font-weight:700}
figure{margin:1.6rem 0;text-align:center}
figure img{max-width:100%;height:auto;border:1px solid var(--line);border-radius:10px;background:#fff}
figure figcaption{color:var(--muted);font-size:.9rem;margin-top:.5rem;text-align:left}
.content h2{margin-top:2.2rem;padding-top:.4rem;border-top:1px solid var(--line)}
.content h3{margin-top:1.6rem}
.content table{border-collapse:collapse;width:100%;font-size:.9rem;margin:1rem 0;display:block;overflow-x:auto}
.content th,.content td{border:1px solid var(--line);padding:.4rem .6rem;text-align:left}
.content th{background:var(--card)}
.content code{background:var(--code);padding:.1rem .35rem;border-radius:4px;font-size:.88em}
.content pre{background:var(--code);padding:1rem;border-radius:8px;overflow-x:auto}
.content pre code{background:none;padding:0}
.content blockquote{margin:1rem 0;padding:.4rem 1rem;border-left:3px solid var(--accent);
  background:var(--card);color:var(--muted)}
.toc{background:var(--card);border:1px solid var(--line);border-radius:10px;padding:.6rem 1rem;font-size:.9rem}
.note{background:var(--card);border:1px solid var(--line);border-left:3px solid var(--accent2);
  border-radius:8px;padding:.7rem 1rem;font-size:.92rem;color:var(--muted);margin:1.2rem 0}
footer.site{border-top:1px solid var(--line);color:var(--muted);font-size:.85rem;
  max-width:var(--maxw);margin:0 auto;padding:1.5rem 1.2rem 3rem}
.gallery{display:grid;grid-template-columns:repeat(auto-fit,minmax(320px,1fr));gap:1.4rem}
.gallery figure{margin:0}
"""


def page(title: str, body: str, active: str) -> str:
    nav = "".join(
        f'<a href="{href}"{" style=\"color:var(--accent)\"" if href==active else ""}>{label}</a>'
        for href, label in NAV)
    return f"""<!doctype html>
<html lang="en"><head>
<meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>{title} · {SITE_TITLE}</title>
<meta name="description" content="APEX-05: Arabidopsis spaceflight transcriptomics, root architecture and ML sample QC — FAIR data and code.">
<link rel="stylesheet" href="assets/style.css">
</head><body>
<header class="site"><nav><span class="brand">🌱 APEX-05</span>{nav}</nav></header>
<main>{body}</main>
<footer class="site">
  APEX-05 — Advanced Plant Experiment-05 · FAIR data &amp; code ·
  <a href="{REPO_URL}">GitHub</a> · Generated by <code>website/build_site.py</code>.
</footer></body></html>"""


def md_section(md_text: str, header: str) -> str:
    """Return the markdown between a '## header' and the next '## '."""
    m = re.search(rf"^##\s+{re.escape(header)}\s*$(.*?)(?=^##\s|\Z)",
                  md_text, re.M | re.S)
    return m.group(1).strip() if m else ""


def render_md(md_text: str) -> str:
    return markdown.markdown(md_text, extensions=MD_EXT)


def fix_doc_links(html: str) -> str:
    """Rewrite in-repo doc links so they resolve on the site or on GitHub."""
    html = html.replace('href="PROVENANCE_cax23_mislabelling.md"', 'href="provenance.html"')
    html = html.replace('href="cax2_allele_concordance.md"', 'href="cax2-story.html"')
    html = re.sub(r'href="(\.\./)?([^"]+\.md)"',
                  rf'href="{REPO_URL}/blob/main/\2"', html)
    html = re.sub(r'href="(\.\./)?((results|data|analysis|archive|manuscript)/[^"]+)"',
                  rf'href="{REPO_URL}/blob/main/\2"', html)
    return html


def build():
    FIGDIR.mkdir(parents=True, exist_ok=True)
    (DOCS / ".nojekyll").write_text("")
    (ASSETS / "style.css").write_text(CSS.strip(), encoding="utf-8")

    # copy gallery + final-manuscript figures
    for fn, _ in GALLERY + FINAL_FIGS:
        src = REPO / "results" / "ml" / fn
        if src.exists():
            shutil.copy2(src, FIGDIR / fn)

    manuscript_md = (REPO / "manuscript/apex05_manuscript.md").read_text(encoding="utf-8")
    legends_md = (REPO / "manuscript/figure_legends.md").read_text(encoding="utf-8")
    abstract = md_section(manuscript_md, "Abstract")

    # ---------- index.html ----------
    hero_figs = "".join(
        f'<figure><a href="figures.html"><img src="assets/figures/{fn}" alt="{cap}"></a>'
        f'<figcaption>{cap}</figcaption></figure>'
        for fn, cap in [GALLERY[4], GALLERY[7], GALLERY[8]])
    index_body = f"""
<section class="hero">
  <h1>APEX-05 — <em>Arabidopsis</em> spaceflight transcriptomics, root architecture &amp; ML sample QC</h1>
  <p class="lede">A FAIR, reproducible data-and-code package comparing four <em>Arabidopsis thaliana</em>
     genotypes flown on the ISS against matched ground controls — paired shoot/root RNA-seq and RSML
     root-architecture morphometrics, with a machine-learning quality-control layer.</p>
  <div class="badges"><span class="badge">MIT licensed</span><span class="badge">FAIR</span>
    <span class="badge">RNA-seq + RSML</span><span class="badge">g:Profiler enrichment</span>
    <span class="badge">scikit-learn QC</span></div>
  <div class="btnrow">
    <a class="btn primary" href="final-manuscript.html">Read the Final manuscript →</a>
    <a class="btn" href="figures.html">Figures</a>
    <a class="btn" href="cax2-story.html">CAX2 QC story</a>
    <a class="btn" href="{REPO_URL}">Code &amp; data (GitHub)</a>
  </div>
  <p class="lede"><b>Headline:</b> wild-type and <em>rbohD</em> mount a strong
  spaceflight response localised to the outer root (epidermis, cortex, gravity-
  sensing columella); <em>cax2-2</em> <b>abolishes the transcriptional response</b>
  (<em>CAX2</em> required) yet its roots still shorten — uncoupling expression from
  growth.</p>
</section>

<h2>Abstract</h2>
<div class="content">{render_md(abstract)}</div>

<h2>Highlights</h2>
<div class="cards">
  <div class="card"><h3>4 genotypes × 2 tissues × 2 conditions</h3>
    <p>Col-0, <em>cax2-2</em>, <em>cax2-3</em>, <em>rbohD</em>; root &amp; shoot; spaceflight vs ground control.</p></div>
  <div class="card"><h3>ML sample QC</h3>
    <p><span class="k">100%</span> leave-one-out tissue recovery; <span class="k">~97%</span> recall detecting injected label-swaps.</p></div>
  <div class="card"><h3>CAX2 allele concordance</h3>
    <p><em>cax2-3</em>'s flight-DEG set is <span class="k">10–17×</span> inflated and discordant with its sibling allele — excluded on QC grounds.</p></div>
  <div class="card"><h3>Functional signatures</h3>
    <p>Root core → photosynthesis; shoot core → cell wall + peroxidase; <em>rbohD</em> → redox/detox.</p></div>
</div>

<h2>Selected figures</h2>
<div class="gallery">{hero_figs}</div>

<h2>Data &amp; code</h2>
<div class="content"><ul>
  <li><b>Manuscript &amp; legends:</b> <a href="manuscript.html">rendered here</a> ·
      source in <a href="{REPO_URL}/tree/main/manuscript"><code>manuscript/</code></a></li>
  <li><b>Data:</b> <a href="{REPO_URL}/tree/main/data"><code>data/</code></a>
      (metadata, expression, morphometrics, gene sets)</li>
  <li><b>Analysis code:</b> <a href="{REPO_URL}/tree/main/analysis"><code>analysis/</code></a>
      (R pipelines + Python ML/enrichment)</li>
  <li><b>Results:</b> <a href="{REPO_URL}/tree/main/results"><code>results/</code></a>
      (tables, plots, ML outputs)</li>
  <li><b>Provenance:</b> <a href="provenance.html">cax2-3 mislabelling record</a></li>
</ul></div>
"""
    (DOCS / "index.html").write_text(page("Home", index_body, "index.html"), encoding="utf-8")

    # ---------- manuscript.html (manuscript + figure legends) ----------
    body_md = manuscript_md + "\n\n---\n\n" + legends_md
    man_html = fix_doc_links(render_md(body_md))
    (DOCS / "manuscript.html").write_text(page(
        "Manuscript",
        f'<div class="note">This is the working manuscript scaffold. Bracketed '
        f'<code>[TO CONFIRM]</code> markers denote fields for the authors to finalise.</div>'
        f'<div class="content">{man_html}</div>', "manuscript.html"), encoding="utf-8")

    # ---------- final-manuscript.html (the 3-genotype Final manuscript) ----------
    final_path = REPO / "manuscript/apex05_FINAL_manuscript.md"
    if final_path.exists():
        final_html = fix_doc_links(render_md(final_path.read_text(encoding="utf-8")))
        gallery = "".join(
            f'<figure><img src="assets/figures/{fn}" alt="{cap}"><figcaption>{cap}</figcaption></figure>'
            for fn, cap in FINAL_FIGS if (FIGDIR / fn).exists())
        (DOCS / "final-manuscript.html").write_text(page(
            "Final manuscript",
            f'<div class="note">Final integrated APEX-05 manuscript (Col-0, '
            f'<em>cax2-2</em>, <em>rbohD</em>) on the corrected 64-sample data. '
            f'<code>[TO CONFIRM]</code> marks fields awaiting author input.</div>'
            f'<div class="content">{final_html}</div>'
            f'<h2>Figures</h2><div class="gallery">{gallery}</div>',
            "final-manuscript.html"), encoding="utf-8")
        print("   wrote docs/final-manuscript.html")

    # ---------- figures.html (gallery) ----------
    figs = "".join(
        f'<figure><img src="assets/figures/{fn}" alt="{cap}"><figcaption>{cap}</figcaption></figure>'
        for fn, cap in GALLERY if (FIGDIR / fn).exists())
    (DOCS / "figures.html").write_text(page(
        "Figures", f'<div class="hero"><h1>Figures</h1>'
        f'<p class="lede">Main machine-learning QC and CAX2 analysis figures. '
        f'Full legends are in the <a href="manuscript.html">manuscript</a>.</p></div>'
        f'<div class="gallery">{figs}</div>', "figures.html"), encoding="utf-8")

    # ---------- cax2-story.html & provenance.html (rendered docs) ----------
    for src, out, title in [
        ("docs/cax2_allele_concordance.md", "cax2-story.html", "CAX2 QC story"),
        ("docs/PROVENANCE_cax23_mislabelling.md", "provenance.html", "cax2-3 provenance"),
    ]:
        txt = (REPO / src).read_text(encoding="utf-8")
        html = fix_doc_links(render_md(txt))
        (DOCS / out).write_text(page(
            title, f'<div class="content">{html}</div>', "cax2-story.html"), encoding="utf-8")

    print("Built site into docs/:")
    for f in sorted(DOCS.glob("*.html")):
        print("  ", f.relative_to(REPO))
    print(f"   assets/style.css + {len(list(FIGDIR.glob('*.png')))} figures")


if __name__ == "__main__":
    build()
