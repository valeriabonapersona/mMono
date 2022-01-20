'
  Script: Upload data
  Input: processed files of all meta-analyses
  
  Author: v.bonapersona-2@umcutrecht.nl

'

# Data with effect sizes --------------------------------------------------
source("src/removal_outliers.R") # outputs data "dat"

dat_full <- readRDS(paste0(final, "data_for_analysis.RDS"))
mono_full <- readRDS(paste0(temp, "monoamines_complete.RDS"))

turnovers <- c("DOPAC/DA", "HVA/DA", "DOPAC_HVA/DA", "5HIAA/5HT", "VMA/NE")
dat_males <- dat %>% 
  filter(sex == "male",
         at_death == "rest",
         !outcome %in% turnovers)
