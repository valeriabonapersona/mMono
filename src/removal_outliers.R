'
  Script: Removal outliers
  
  Author: Valeria Bonapersona
  
  Input: output of data_cleaning.R 
  Output: analyzable dataset
  
'


# Environment preparation -------------------------------------------------
# upload data
dat_full <- readRDS(paste0(final, "data_for_analysis.RDS"))


# Removal outliers --------------------------------------------------------
all_out <- unique(dat_full$outcome)
res <- data.frame()
row <- 1

for (o in all_out) {
  
  dat_ft <- dat_full[dat_full$outcome == o,]
  
  if(nrow(dat_ft) > 1) {
    
    # model
    mod_out <- rma(yi, vi, method = "REML",
                   data = dat_ft, slab = cite)
    
    # influence analysis
    inf <- influence(mod_out)
    
    inf_cases <- dat_ft$unique_id[which(inf$inf$inf == "*")]
    
    res <- res %>% bind_rows(
      data.frame(
        outcome = rep(o, length(inf_cases)), 
        index = inf_cases
      )
    )
    
    row <- row + 1
    
  }
  
}

inf_df <- dat_full %>% filter(unique_id %in% res$index)

dat_full <- dat_full %>% 
  mutate(
    outlier = ifelse(
      abs(scale(dat_full$yi)) > 3.29, "possible", "no"
    )
  )

out_pos_id <- dat_full %>% filter(outlier == "possible") %>% pull(unique_id) %>% unique()

out_id <- out_pos_id[out_pos_id %in% inf_df$unique_id]

# removal outliers
dat <- dat_full %>% 
  filter(unique_id != "monoamines 319") # outlier and influential



# Clean up ----------------------------------------------------------------

rm(dat_full, out_pos_id, out_id, inf_df,res, row, all_out)
