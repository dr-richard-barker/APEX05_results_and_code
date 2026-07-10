#!/usr/bin/env Rscript
# =============================================================================
#  APEX-05 | ggKEGG systems-biology pathway maps with spaceflight DE overlay
# =============================================================================
#
#  Renders KEGG pathway diagrams (ggkegg) with nodes coloured by the DESeq2
#  spaceflight log2 fold-change (FL vs GC) for a chosen genotype x tissue, so the
#  flight response can be read in its pathway context. AGI gene IDs from the
#  DESeq2 tables are matched to the KEGG 'ath:ATxxxxxxx' node identifiers.
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

render_pathway <- function(pid, geno, tissue) {
  message(sprintf("  %s  (%s %s)", pid, geno, tissue))
  lfc <- lfc_map(geno, tissue)
  g <- pathway(pid)
  # assign each gene node the mean log2FC of its AGI members
  g <- g |> activate(nodes) |> mutate(
    lfc = vapply(strsplit(gsub("ath:", "", name), " "), function(ids) {
      v <- lfc[ids]; v <- v[!is.na(v)]
      if (length(v)) mean(v) else NA_real_
    }, numeric(1)))
  lim <- max(abs(range(g |> activate(nodes) |> pull(lfc), na.rm = TRUE)), na.rm = TRUE)
  p <- ggraph(g, layout = "manual", x = x, y = y) +
    geom_edge_link(alpha = 0.25, colour = "grey55",
                   arrow = arrow(length = unit(1.2, "mm")), end_cap = circle(1, "mm")) +
    geom_node_rect(aes(fill = lfc, filter = type == "gene"), colour = "grey30", linewidth = 0.2) +
    geom_node_text(aes(label = graphics_name, filter = type == "gene"),
                   size = 1.7, repel = FALSE) +
    scale_fill_gradient2(low = "#0072B2", mid = "grey92", high = "#D55E00",
                         midpoint = 0, limits = c(-lim, lim), na.value = "grey85",
                         name = "flight\nlog2FC") +
    theme_void() +
    ggtitle(sprintf("%s — %s %s spaceflight response", pid, geno, tissue)) +
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
