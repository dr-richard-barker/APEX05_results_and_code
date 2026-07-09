---
  title: "UpsetR_Tools"
author: "Jonathan Lombardino"
date: "July 24, 2020"
output: html_document
---
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(tidyverse)
library(UpSetR)
#library(RColorBrewer)
library(ggplot2)
#library(forcats) # if you want svgs
```
### RUN IF YOU WANT TO KEEP THE GENE IN THE UPSET BINARY TABLE FOR REFERENCE WARNING: OVERRIDES UPSETR fromList command ###
```{r modified fromList command}
#Obtained from user docmanny https://github.com/hms-dbmi/UpSetR/issues/85
fromList <- function (input) {
  # Same as original fromList()...
  elements <- unique(unlist(input))
  data <- unlist(lapply(input, function(x) {
    x <- as.vector(match(elements, x))
  }))
  data[is.na(data)] <- as.integer(0)
  data[data != 0] <- as.integer(1)
  data <- data.frame(matrix(data, ncol = length(input), byrow = F))
  data <- data[which(rowSums(data) != 0), ]
  names(data) <- names(input)
  # ... Except now it conserves your original value names!
  row.names(data) <- elements
  return(data)
}
```
## Wrangle Data ##
```{r make_table, include=True}
#### Read in input csv file that is a dataframe of named lists ####
input_filepath = "UPSETR_WT_V_RBOHD.csv" #####
input_csv <- read.csv(input_filepath)
colnames(input_csv)
fixed_df <- input_csv
### Touch up csv if needed ###
#fixed_df <- as.data.frame(input_csv %>% select(-X, -X.1))
### Use the built in fromList command to turn the dataframe into a binary table for upset to use, NOTE IDs are lost using this ###
df <- fixed_df
upset_table = fromList(df)
col_names = colnames(upset_table)
col_names
### Rename col names if desired ###
#new_col_names <- c("Col Roots Down", "Col Roots Up", "RBOHD Roots Up", "RBOHD Roots Down",
#                     "Col Shoots Up", "Col Shoots Down", "RBOHD Shoots Up", "RBOHD Shoots Down") 
#new_col_names
### Assign new column names to table ###
#upset_table = upset_table %>% rename_all(funs(c(new_col_names)) )
```
## Make a metadata table ##
```{r metadata table}
### Add metadata if desired ###
#genotype = rep(c("col", "col", "rboh", "rboh"), 2) 
#tissue = c(rep(c("root"), 4), rep(c("shoot"),4))
#fold = c("down", "up", "up", "down", "up", "down", "up","down")
#combo = paste(genotype, tissue, sep="_")
### Join column names from upset table to metadata columns to make a metadata dataframe ###
#meta <- data_frame(new_col_names, tissue, genotype, fold, combo)
#meta$combo
#meta
```
## Plot UpsetR
```{r plot, echo=FALSE}
### Make output image file###
fileout="myupset_plot.svg"
svg(filename = fileout, height = 8, width =14, pointsize = 12)
### Example With metadata ###
### NOTE: CHANGE VARIABLE NAMES IN SET.METADATA TO MATCH YOUR OWN: column = "my_meta_col", colors = c(meta1="a", meta2="b") ###
upset(upset_table, order.by = c("freq"), decreasing = TRUE, nsets = 8, nintersects = NA, # plot all 8 sets, all intersects, and order by freq
      mainbar.y.label = "Shared Gene Count", sets.x.label = "Gene Count", # add labels
      mb.ratio = c(.60, .40) , #ratio of plot to bar chart,
      matrix.dot.alpha = 0.5,
      ## Add color coding to matrix plot, pick a column, and what values you want to group
      set.metadata = list(data = meta, 
                          plots = list(list(type = "matrix_rows", column = "combo", colors = c(col_root = "blue", col_shoot = "green",
                                                                                               rboh_root = "red", rboh_shoot = "purple"), alpha = 0.35
                          ))),
      text.scale = c(1.8,1.2,1.5,1.5,1.5,1.3) #intersection size title, intersection ticks, set size title, set size ticks, set names, numbers above bars
)
dev.off()
### No Meta ###
upset(upset_table, order.by = c("freq"), decreasing = TRUE, nsets = 8, nintersects = NA, # plot all 8 sets, all intersects, and order by freq
      mainbar.y.label = "Shared Gene Count", sets.x.label = "Gene Count", # add labels
      mb.ratio = c(.60, .40) , #ratio of plot to bar chart,
      matrix.dot.alpha = 0.5,
      text.scale = c(1.8,1.2,1.5,1.5,1.5,1.3) #intersection size title, intersection ticks, set size title, set size ticks, set names, numbers above bars
)
dev.off()

```
