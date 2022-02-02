# must run first g_freq_ba for order_ba
order_out_mono <- c("Dopamine", "Serotonin", "Noradrenaline", 
                    "Enzymes")

da_out_order <- c("DA", "DOPAC", "HVA", "3MT", "D2R_like", "D1R_like", "DAT")
se_out_order <- c("5HT", "5HIAA", "5HT_1AR", "5HT_2AR", "5HT_2CR","5HT_6R", "SERT")
ne_out_order <- c("NE", "VMA", "MHPG")
enz_out_order <- c("TH", "COMT", "MAO_A", "TPH2")
order_all_out <- c(da_out_order,
                   se_out_order,
                   ne_out_order,
                   enz_out_order)


ba_order <- mono_full %>% 
  filter(ba_grouped != "other_areas") %>%
  group_by(ba_grouped) %>% 
  count() %>% 
  arrange(n) %>% 
  pull(ba_grouped) %>% 
  rev() %>% 
  c("other_areas") %>%
  str_replace_all("_"," \n")