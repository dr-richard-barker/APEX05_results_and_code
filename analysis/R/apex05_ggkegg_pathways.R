#!/usr/bin/env Rscript
# =============================================================================
#  APEX-05 | ggKEGG systems-biology pathway maps with spaceflight DE overlay
# =============================================================================
#
#  Renders KEGG pathway diagrams (ggkegg) with GENE/enzyme nodes coloured by the
#  DESeq2 spaceflight log2 fold-change (FL vs GC) for a chosen genotype x tissue,
#  AND the pathway's METABOLITE / COFACTOR nodes (KEGG compound nodes, e.g. NADPH,
#  SAM, coenzyme A, glutathione) drawn as labelled circles with their compound
#  names fetched from KEGG — so each map reads as a full systems-biology diagram
#  (enzymes acting on substrates/products with their cofactors), not just enzymes.
#  AGI gene IDs from the DESeq2 tables are matched to the 'ath:ATxxxxxxx' nodes.
#
#  Pathways x genotype are chosen to match the enrichment results:
#    ath00940 phenylpropanoid biosynthesis  <- Col-0 shoot
#    ath00480 glutathione metabolism        <- rbohD shoot (redox/detox)
#    ath04075 plant hormone signal transduction <- Col-0 root
#
#  INPUT   results/tables/deseq2/apex05_deseq2_<geno>_<tissue>.csv
#  OUTPUT  results/ml/figJ_<pathway>_<geno>_<tissue>.png
#  RUN  Rscript analysis/R/apex05_ggkegg_pathways.R      (needs internet: KEGG REST)
#  DEPS ggkegg, tidygraph, ggraph, igraph, dplyr, ggplot2
# =============================================================================

suppressWarnings(suppressMessages({
  library(ggkegg); library(tidygraph); library(ggraph)
  library(dplyr); library(ggplot2)
}))

# Resolve repo root from this script's path (analysis/R/ -> repo), else cwd.
.script <- sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))
repo <- if (length(.script))
  normalizePath(file.path(dirname(.script), "..", "..")) else normalizePath(getwd())
outdir <- file.path(repo, "results", "ml")

# AGI -> log2FC lookup for a genotype x tissue
lfc_map <- function(geno, tissue) {
  slug <- tolower(gsub("-", "", geno))
  f <- file.path(repo, "results/tables/deseq2",
                 sprintf("apex05_deseq2_%s_%s.csv", slug, tissue))
  d <- read.csv(f)
  setNames(d$log2FoldChange, d$gene)
}

# KEGG compound C-numbers -> short metabolite/cofactor names (batched REST calls).
cpd_names <- function(cids) {
  cids <- unique(cids); out <- setNames(cids, cids)
  for (chunk in split(cids, ceiling(seq_along(cids) / 10))) {
    url <- paste0("https://rest.kegg.jp/list/", paste(chunk, collapse = "+"))
    lines <- tryCatch(readLines(url, warn = FALSE), error = function(e) character(0))
    for (ln in lines) {
      p <- strsplit(ln, "\t")[[1]]
      if (length(p) == 2) {
        id <- sub("^cpd:", "", p[1])
        nm <- trimws(strsplit(p[2], ";")[[1]][1])       # first synonym
        out[id] <- nm
      }
    }
  }
  out
}

render_pathway <- function(pid, geno, tissue) {
  message(sprintf("  %s  (%s %s)", pid, geno, tissue))
  lfc <- lfc_map(geno, tissue)
  g <- pathway(pid)
  # metabolite / cofactor names for the compound nodes
  cids <- unique(sub("^cpd:", "", unlist(strsplit(
    g |> activate(nodes) |> filter(type == "compound") |> pull(name), " "))))
  cmap <- if (length(cids)) cpd_names(cids) else character(0)
  # assign each gene node the mean log2FC of its AGI members; label compounds by name
  g <- g |> activate(nodes) |> mutate(
    lfc = vapply(strsplit(gsub("ath:", "", name), " "), function(ids) {
      v <- lfc[ids]; v <- v[!is.na(v)]
      if (length(v)) mean(v) else NA_real_
    }, numeric(1)),
    cname = ifelse(type == "compound",
      vapply(strsplit(gsub("cpd:", "", name), " "), function(z) {
        nm <- cmap[z[1]]; nm <- if (is.na(nm)) z[1] else nm
        if (nchar(nm) > 20) paste0(substr(nm, 1, 18), "…") else nm
      }, character(1)), NA_character_))
  lim <- max(abs(range(g |> activate(nodes) |> pull(lfc), na.rm = TRUE)), na.rm = TRUE)
  p <- ggraph(g, layout = "manual", x = x, y = y) +
    geom_edge_link(alpha = 0.25, colour = "grey55",
                   arrow = arrow(length = unit(1.2, "mm")), end_cap = circle(1, "mm")) +
    # metabolites / cofactors (KEGG compound nodes)
    geom_node_point(aes(filter = type == "compound"), shape = 21, size = 2.3,
                    fill = "#4C9F70", colour = "grey25", stroke = 0.3) +
    geom_node_text(aes(label = cname, filter = type == "compound"), size = 1.45,
                   fontface = "italic", colour = "#1B5E20", vjust = -1.1) +
    # enzymes / genes coloured by flight log2FC
    geom_node_rect(aes(fill = lfc, filter = type == "gene"), colour = "grey30", linewidth = 0.2) +
    geom_node_text(aes(label = graphics_name, filter = type == "gene"),
                   size = 1.7, repel = FALSE) +
    scale_fill_gradient2(low = "#0072B2", mid = "grey92", high = "#D55E00",
                         midpoint = 0, limits = c(-lim, lim), na.value = "grey85",
                         name = "flight\nlog2FC") +
    theme_void() +
    ggtitle(sprintf("%s — %s %s spaceflight response  (○ = metabolite/cofactor)",
                    pid, geno, tissue)) +
    theme(plot.title = element_text(size = 11))
  out <- file.path(outdir, sprintf("figJ_%s_%s_%s.png", pid, tolower(gsub("-", "", geno)), tissue))
  ggsave(out, p, width = 11, height = 8, dpi = 200, bg = "white")
  message("    wrote ", sub(paste0(repo, "/"), "", out))
}

jobs <- list(c("ath00940", "Col-0", "shoot"),
             c("ath00480", "rbohD", "shoot"),
             c("ath04075", "Col-0", "root"))
for (j in jobs) tryCatch(render_pathway(j[1], j[2], j[3]),
                         error = function(e) message("    ERROR: ", conditionMessage(e)))
message("done")
