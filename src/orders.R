# must run first g_freq_ba for order_ba
order_out_mono <- c("Dopamine", "Serotonin", "Noradrenaline", 
                    "Enzymes")


ba_order <- mono_full %>% 
  filter(ba_grouped != "other_areas") %>%
  group_by(ba_grouped) %>% 
  count() %>% 
  arrange(n) %>% 
  pull(ba_grouped) %>% 
  rev() %>% 
  c("other_areas") %>%
  str_replace_all("_"," \n")