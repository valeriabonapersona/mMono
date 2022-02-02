'
  Script: Visualizations frequency outcomes in brain areas (males)
  Input: dataset after preprocessing 
  Output: Figure 3
  
  Author: v.bonapersona-2@umcutrecht.nl

'
source("src/orders.R")

g_males_ba_out <- dat_males %>%
  mutate(
    out_mono = case_when(
      out_mono == "da_related" ~ "Dopamine",
      out_mono == "ne_related" ~ "Noradrenaline",
      out_mono == "ser_related" ~ "Serotonin", 
      out_mono == "enzymes" ~ "Enzymes",
      T ~ out_mono
    ), 
    ba_grouped = str_replace_all(ba_grouped,"_", " \n"), 
    ba_grouped = factor(ba_grouped, ba_order), # order comes from fig_ba
    outcome = str_replace_all(outcome, "_", " ")
  ) %>%
  group_by(out_mono, ba_grouped, outcome) %>% 
  summarize(n_id = length(unique(id)), .groups = "drop") %>%
  ggplot(aes(ba_grouped, 
           #  outcome,
             factor(outcome, levels = rev(str_replace_all(order_all_out, "_", " "))), 
                    fill = n_id)) + 
  geom_tile() + 
  facet_wrap(~ factor(out_mono, levels = order_out_mono), scales = "free") + 
  my_theme + 
  labs(
    x = "Brain areas", 
    y = "Outcomes", 
    fill = "N publications", 
    title = "Frequency outcomes - Males"
  ) + 
  
  theme(
    legend.position = "bottom"
  ) + 
  scale_fill_viridis()
