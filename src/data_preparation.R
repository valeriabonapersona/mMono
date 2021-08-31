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
# The following where double, i.e. present both as "total" and as pyramidal and granual layers
double_outcomes <- c("24129488_1_morph_5", "24129488_1_morph_6")
#exp_one_hem <- "22371048_2"

# find exp that have both metabolites and turnovers
double_outcomes <- mono %>% 
  select(exp_id, ba_grouped, brain_area_hemisphere, outcome) %>% 
  unique() %>% 
  mutate(each = 1) %>% 
  pivot_wider(id_cols = c("exp_id", "ba_grouped", "brain_area_hemisphere"), 
              names_from = "outcome", values_from = "each") %>% 
  rowwise() %>%
  mutate(
    dopac_check = sum(DOPAC, `DOPAC/DA`, `DOPAC_HVA/DA`, na.rm = TRUE), 
    hva_check = sum(HVA, `HVA/DA`, `DOPAC_HVA/DA`, na.rm = TRUE), 
    hiaa_check = sum(`5HIAA/5HT`, `5HIAA`, na.rm = TRUE),
    vma_check = sum(VMA, `VMA/NE`, na.rma = TRUE)
  ) %>% 
  filter(any(c(dopac_check > 1, hva_check > 1, hiaa_check > 1, vma_check > 1)))

exp_double_out <- double_outcomes %>% pull(exp_id) %>% unique()

## choose between turnovers or metabolites? 
mono %>% 
  filter(!exp_id %in% exp_double_out, 
         out_grouped %in% c("da_metabolites", "ser_metabolites", "ser_precursors",
                            "ne_metabolites", "ne_precursors")) %>% 
  group_by(outcome) %>% 
  summarize(n_exp = length(unique(exp_id))) -> x


'
The distribution of the ratio between two (normal) variables is a Cauchy distribution, which is an undefined distribution (wiki). 
In special cases, one can consider the Cauchy(µ, sigma) as a non standardised t distribution t(µ, sigma); however, 
the data provided from the papers and means and sem. I will therefore continue considering this ratio as a gaussian distribution. 

The reporting of concentrations vs turnover is half/half across exp. There are also a subset of 
experiments that report both. My aim is to meta-analyze concentrations because they are more interpretable. 

Since it cannot be solved analytically, I will perform a simulation study to estimate the location and scale parameters. 
I will then compare visually the concentrations so derived from those from the papers. 

As exploratory postdoc analysis, I will separately analyze the turnovers. 

'

# calculate missing concentrations / turnovers
da_double <- mono %>% 
  filter(outcome %in% c("DA", "DOPAC", "HVA", 
                        "DOPAC/DA", "HVA/DA", "DOPAC_HVA/DA"), 
         exp_id %in% exp_double_out)

da_validate <- da_double %>% 
  filter(outcome %in% c("DA", "DOPAC", #"HVA", 
                        "DOPAC/DA" 
                     #   "HVA/DA", "DOPAC_HVA/DA")
         )) %>%
  select(exp_id, outcome, brain_area_publication, ends_with("_c"), ends_with("_e")) %>% 
  pivot_longer(cols = c(ends_with("_c"), ends_with("_e"))) %>% 
  separate(col = name, into = c("summary", "group"), sep = "_") %>%
  pivot_wider(id_cols = c("exp_id", "brain_area_publication", "group"), 
              names_from = c("outcome", "summary"), values_from = "value")

da_sampled <- rnorm(10000, da_validate$DA_mean[1], da_validate$DA_sd[2])

# HERBERT EMAILED!

'for now, I will keep only the concentrations and I will not consider the metabolites in the analysis'

# prepare dataset

dat <- mono %>% 
  
  # get variances
  mutate(
    # get variances
    var_c = sd_c^2,
    var_e = sd_e^2
    
  #  across(c("part_cell", "distance_cell"), function(x)str_remove_all(x,"not_applicable"))
  )  %>% 
  
  # sum the volumes per experiment, get the mean for the other outcomes
  group_by_at(vars(one_of(life_vars, "outcome", "out_grouped", "ba_grouped", "ba_main", 
                          "product_measured"))) %>% 
  summarize(
    n_together = length(unique(outcome_id)),
    
    # means
    across(starts_with("mean_"), function(x) mean(x)),
    
    # variances
    across(starts_with("var_"), function(x) sum(x)/(n_together^2)),
    
    # standard deviations
    sd_c = sqrt(var_c),
    sd_e = sqrt(var_e),
    
    # sample sizes
    across(starts_with("n_"), function(x) mean(x)),
    
    # ba_location_layer = paste(paste(ba_location, ba_layer, sep = "-"), collapse = "; "), 
    # about_cell = paste(paste(part_cell, distance_cell, sep = "-"), collapse = "; "),
    .groups = "drop"
  ) %>%
  
  ungroup() %>%
  # unique identified
  mutate(unique_id = paste(out_grouped, 1:nrow(.)), sep ="_") %>% 
  relocate(unique_id, .after = exp_id) %>% 
  
  # var to describe life events
  mutate(
    origin_num = ifelse(origin == "purchased_pregnant_dams", 1, 0),
    behavior_num = ifelse(behavior == "stressful", 1, 0),
  #  housing_num = ifelse(housing_after_weaning == "single", 1, 0),
    chronic_num = ifelse(major_life_events == "yes", 1, 0),
    trauma_score = rowSums(across(ends_with("_num"))), 
    trauma_presence = ifelse(trauma_score < 1, "no", "yes")
  )

# calculate effect size
dat <- escalc("SMDH",
              m1i = mean_e, sd1i = sd_e, n1i = n_e,
              m2i = mean_c, sd2i = sd_c, n2i = n_c, 
              data = dat)



# Save data for analysis --------------------------------------------------
saveRDS(dat, paste0(final, "data_for_analysis.RDS"))
write.csv(dat, paste0(final, "data_for_analysis.csv"))


