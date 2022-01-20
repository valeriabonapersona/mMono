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
turnovers_df <- mono_full %>% 
  filter(outcome %in% turnovers) %>% 
  select(cite, id, species, sex, model, ba_grouped, ends_with("_c"), ends_with("_e")) %>%
  unique() %>%
  mutate(
    model = str_replace_all(model, "_", " "), 
    ba_grouped = str_replace_all(ba_grouped, "_", " "), 
    n_c = round(n_c), 
    n_e = round(n_e), 
    mean_c = round(mean_c, 2),
    mean_e = round(mean_e, 2), 
    sd_c = round(sd_c, 2),
    sd_e = round(sd_e, 2)
  ) 

write.csv(turnovers_df, paste0("results/", "turnovers.csv"))
