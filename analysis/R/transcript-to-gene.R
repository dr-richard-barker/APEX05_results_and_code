## Bret Larget
## 2019 January 16

## Code to transform a csv file.
## There is a column named `ID` that has string data such as "AT1G01010.1".
## The goal is to replace this with a column named "Gene_ID"
##   (can replace _ with a space, but it is bad practice to use spaces in variable names)
## where the suffix .1 is removed:  In the example, "AT1G01010"
## In addition, all rows that share the same gene id should have the numerical values of the other columns summed.
## So, the output data frame will have the same number of columns, and one row for each
## unique gene id.
# Package installations (commented out for automated execution/reproducibility)
# if (!requireNamespace("tidyverse", quietly = TRUE)) install.packages("tidyverse")

## Package to read in data in the "tidyverse"
library(readr)
## Package for data manipulation in the "tidyverse"
library(dplyr)
## Package to manipulate strings
library(stringr)

## Create a function that takes the filename as input
## and writes to a CSV file as output, adding _Gene to the body of the file name
transcript_to_gene = function(name)
{
  if (!file.exists(name)) {
    warning("File '", name, "' does not exist. Skipping.")
    return(NULL)
  }
  x = read_csv(name)
  y = x %>%
    mutate(Gene_ID = str_replace(ID,"\\..*","")) %>% ## \\. matches ., .* matches anything else
    select(-ID) %>% ## remove the ID column
    group_by(Gene_ID) %>% ## group rows by Gene_ID
    summarise_all(sum) ## summarize all other columns by summing the values
  
  out_name = str_replace(name,".csv","") %>% ## strip .csv form filename
    str_c("_Gene.csv") ## add _Gene.csv to end of file name
  
  # Note: updated deprecated 'path' parameter to 'file' for newer readr versions
  write_csv(y, file=out_name) ## write output csv file
  return(invisible(y)) ## return new data frame, but invisibly.
}
  
## Example runs (wrapped in if(FALSE) or relative paths to prevent execution crashes)
if (FALSE) {
  # To run, set your working directory to the clutter folder or modify these paths:
  transcript_to_gene("../APEX5_desktop_clutter/full_table_GCRootCAX23-FLRootCAX23.csv")
  transcript_to_gene("../APEX5_desktop_clutter/full_table_GCShootCAX23-FLShootCAX23.csv")
  
  transcript_to_gene("APEX5_HiSeq_Hisat_Counts_V12_RBOHD_Roots.csv")
  transcript_to_gene("APEX5_HiSeq_Hisat_Counts_V12_CAX22_Root.csv")
  transcript_to_gene("APEX5_HiSeq_Hisat_Counts_V12_CAX22_Shoot.csv")
  transcript_to_gene("APEX5_HiSeq_Hisat_Counts_V12_COL00_Root.csv")
  transcript_to_gene("APEX5_HiSeq_Hisat_Counts_V12_COL00_Shoot.csv")
  transcript_to_gene("APEX5_HiSeq_Hisat_Counts_V12_RBOHD_Root.csv")
  transcript_to_gene("APEX5_HiSeq_Hisat_Counts_V12_RBOHD_Shoot.csv")
  
  transcript_to_gene("full_table_GCRootCAX22-FLRootCAX22.csv")
  transcript_to_gene("full_table_GCShootCAX22-FLShootCAX22.csv")
  transcript_to_gene("full_table_GCRootCOL00-FLRootCOL00.csv")
  transcript_to_gene("full_table_GCShootCOL00-FLShootCOL00.csv")
  transcript_to_gene("full_table_GCRootRBOHD-FLRootRBOHD.csv")
  transcript_to_gene("full_table_GCShootRBOHD-FLShootRBOHD.csv")
  
  transcript_to_gene("Col-0_APEX05.csv")
  transcript_to_gene("APEX5_HiSeq_Hisat_Counts_V12.csv")
  transcript_to_gene("APEX5_HiSeq_Hisat_Counts_V12_CAX22.csv")
  transcript_to_gene("APEX5_HiSeq_Hisat_Counts_V12_COL00.csv")
  transcript_to_gene("APEX5_HiSeq_Hisat_Counts_V12_RBOHD.csv")
}


