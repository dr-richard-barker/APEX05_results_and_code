#!/usr/bin/env Rscript
# =============================================================================
#  APEX-05 | ggPlantmap anatomical view of the cell-type-resolved flight response
# =============================================================================
#
#  Paints the cell-type flight-DEG enrichment (Stage 3, PCMDB projection;
#  results/tables/apex05_celltype_deg_enrichment.csv) onto Arabidopsis root and
#  leaf anatomy using ggPlantmap's built-in maps, per genotype. Shows WHERE in
#  the organ each genotype's spaceflight response localises.
#
#  Value = max -log10(p) over the PCMDB cell types mapping to each anatomical ROI.
#
#  INPUT   results/tables/apex05_celltype_deg_enrichment.csv
#  OUTPUT  results/ml/figM1_root_anatomy_flight.png
#          results/ml/figM2_leaf_anatomy_flight.png
#  RUN  Rscript analysis/R/apex05_ggplantmap_anatomy.R
#  DEPS ggPlantmap, ggplot2, dplyr
# =============================================================================

suppressWarnings(suppressMessages({ library(ggPlantmap); library(ggplot2); library(dplyr) }))
.script <- sub("--file=", "", grep("--file=", commandArgs(FALSE), value = TRUE))
repo <- if (length(.script)) normalizePath(file.path(dirname(.script), "..", "..")) else getwd()
enr <- read.csv(file.path(repo, "results/tables/apex05_celltype_deg_enrichment.csv"))
GENOS <- c("Col-0", "cax2-2", "rbohD")

# PCMDB cell type -> anatomical-map ROI.name
root_roi <- c("columella"="Columella","root cortex"="Cortex","root endodermis"="Endodermis",
  "non-hair root epidermal cell"="Epidermis","root epidermis"="Epidermis",
  "trichoblast"="Epidermis","root hair cell"="Epidermis","pericycle"="Pericycle",
  "phloem"="Phloem","root procambium"="Procambium","xylem"="Xylem","protoxylem"="Xylem")
leaf_roi <- list("leaf mesophyll"=c("Parenchima.palisade","Parenchima.sponge"),
  "leaf epidermis"=c("epidermis.abaxial","epidermis.adaxial"),
  "guard cell"="epidermis.stomata","bundle sheath"="vascularbundle.bundlesheet")

# aggregate enrichment to ROI (max -log10p over mapped cell types) for one genotype
roi_values <- function(tissue, geno, roi_map) {
  d <- enr[enr$tissue == tissue & enr$genotype == geno, ]
  out <- list()
  if (is.list(roi_map)) {                          # leaf: one cell type -> many ROIs
    for (ct in names(roi_map)) for (roi in roi_map[[ct]]) {
      v <- d$neg_log10_p[d$cell_type == ct]; if (length(v)) out[[roi]] <- max(out[[roi]] %||% 0, v)
    }
  } else {                                         # root: many cell types -> one ROI
    for (ct in names(roi_map)) {
      roi <- roi_map[[ct]]; v <- d$neg_log10_p[d$cell_type == ct]
      if (length(v)) out[[roi]] <- max(out[[roi]] %||% 0, max(v))
    }
  }
  data.frame(ROI = names(out), value = unlist(out), row.names = NULL)
}
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

plot_anatomy <- function(map, tissue, roi_map, outfile, title) {
  vmax <- 0
  panels <- lapply(GENOS, function(g) { v <- roi_values(tissue, g, roi_map)
    vmax <<- max(vmax, v$value, 0); list(g = g, v = v) })
  ps <- lapply(panels, function(p) {
    m <- map; m$value <- p$v$value[match(m$ROI.name, p$v$ROI)]
    ggplot(m, aes(x, y, group = ROI.id, fill = value)) +
      geom_polygon(colour = "grey40", linewidth = 0.2) +
      scale_fill_gradient(low = "grey92", high = "#D55E00", na.value = "grey96",
                          limits = c(0, vmax), name = expression(-log[10]*p)) +
      coord_equal() + theme_void() + ggtitle(p$g) +
      theme(plot.title = element_text(hjust = 0.5, size = 12))
  })
  # combine side by side (patchwork-free: use gridExtra if present, else cowplot-like)
  if (requireNamespace("patchwork", quietly = TRUE)) {
    library(patchwork)
    fig <- (ps[[1]] | ps[[2]] | ps[[3]]) + plot_layout(guides = "collect") +
      plot_annotation(title = title)
    ggsave(outfile, fig, width = 12, height = 4.6, dpi = 200, bg = "white")
  } else {
    g <- do.call(gridExtra::grid.arrange, c(ps, ncol = 3, top = title))
    ggsave(outfile, g, width = 12, height = 4.6, dpi = 200, bg = "white")
  }
  message("wrote ", sub(paste0(repo, "/"), "", outfile))
}

plot_anatomy(ggPm.At.roottip.crosssection, "root", root_roi,
             file.path(repo, "results/ml/figM1_root_anatomy_flight.png"),
             "Root cross-section: where the spaceflight response localises (cell-type DEG enrichment)")
plot_anatomy(ggPm.At.leaf.crosssection, "shoot", leaf_roi,
             file.path(repo, "results/ml/figM2_leaf_anatomy_flight.png"),
             "Leaf cross-section: where the spaceflight response localises (cell-type DEG enrichment)")
message("done")
