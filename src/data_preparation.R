'
  Script: Preparation data for analysis
  
  Author: Valeria Bonapersona
  
  Input: output of data_cleaning.R 
  Output: analyzable dataset
  
'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

mono <- readRDS(paste0(temp, "monoamines_complete.RDS"))



# from how researchers normally calculate fold changes and perc changes, 
# we assume equality of variances between the groups
# later 1) check variances, 2) sensitivity analysis


# Calculate effect sizes --------------------------------------------------------
# all exp have been manually checked for merging. 
# calculate effect sizes for each comparison
dat <- escalc("SMDH",
            m1i = mean_e, sd1i = sd_e, n1i = n_e,
            m2i = mean_c, sd2i = sd_c, n2i = n_c,
            data = mono)
# create an aggregate variable to merge effect sizes
dat <- dat %>% 
  mutate(
    aggr = paste(exp_id, outcome, ba_grouped, product_measured)
  )

dat <- aggregate(dat, cluster = aggr, rho = 0.6) # check if this or struct = "ID" is better

# other important vars
dat <- dat %>% 
  
  # unique identified
  mutate(unique_id = paste(out_grouped, 1:nrow(.)), sep ="_") %>% 
  relocate(unique_id, .after = exp_id) %>% 
  
  # var to describe life events
  mutate(
    origin_num = ifelse(origin == "purchased_pregnant_dams", 1, 0),
    behavior_num = ifelse(behavior == "stressful", 1, 0),
    chronic_num = ifelse(major_life_events == "yes", 1, 0), # single housing is together with major life events
    trauma_score = rowSums(across(ends_with("_num"))), 
    trauma_presence = ifelse(trauma_score < 1, "no", "yes")
  )


# Save data for analysis --------------------------------------------------
saveRDS(dat, paste0(final, "data_for_analysis.RDS"))
write.csv(dat, paste0(final, "data_for_analysis.csv"))

