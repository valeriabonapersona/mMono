'
  Script: tables for paper
'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/upload_data.R")



# Supplementary Table 2 ---------------------------------------------------
ba_categorization <- mono_full %>% 
  select(ba_grouped, brain_area_publication) %>% 
  unique() %>% 
  arrange(ba_grouped, brain_area_publication) %>% 
  mutate(ba_grouped = str_replace_all(ba_grouped, "_", " "), 
         brain_area_publication = str_replace_all(brain_area_publication, "_", " "))

#write.csv(ba_categorization, paste0("results/","ba_categorization.csv"))


# Turnovers ---------------------------------------------------------------
turnover_order <- c("DOPAC/DA", "(DOPAC+HVA)/DA", "HVA/DA", "5HIAA/5HT", "VMA/NE")
turnovers_df <- mono_full %>% 
  filter(outcome %in% turnovers) %>% 
  select(cite, id, species, sex, model, ba_grouped, outcome, 
         ends_with("_c"), ends_with("_e"), sys_review_sig) %>%
  unique() %>%
  rename(turnover = outcome) %>%
  mutate(
    cite = str_replace_all(cite, "\\(", " \\("),
    model = str_replace_all(model, "_", " "), 
    ba_grouped = str_replace_all(ba_grouped, "_", " "), 
    turnover = ifelse(turnover == "DOPAC_HVA/DA", "(DOPAC+HVA)/DA", turnover), 
    turnover = factor(turnover, turnover_order),
    n_c = round(n_c), 
    n_e = round(n_e), 
    mean_c = round(mean_c, 2),
    mean_e = round(mean_e, 2), 
    sd_c = round(sd_c, 2),
    sd_e = round(sd_e, 2), 
    sys_review_sig = case_when(
      sys_review_sig == "not_significant" | is.na(sys_review_sig) ~ "ns", 
      sys_review_sig == "significant" & mean_e > mean_c ~ "increase",
      sys_review_sig == "significant" & mean_e < mean_c ~ "decrease",
      T ~ sys_review_sig
    )
  ) %>% 
  arrange(turnover, ba_grouped, cite)

write.csv(turnovers_df, paste0("results/", "turnovers.csv"))
