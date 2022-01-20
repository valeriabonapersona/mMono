'
  Script: Visualizations frequency outcome by brain area
  Input: full monoamines data
  Output: Figure 2C paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'


ba_order <- mono_full %>% 
                filter(!is.na(ba_grouped), !str_detect(ba_grouped, "other")) %>% 
                group_by(ba_grouped) %>% count() %>% arrange(-n) %>% 
                pull(ba_grouped) %>% str_replace_all("_", " ")

g_out_ba <- mono_full %>% 
  filter(!is.na(ba_grouped), !str_detect(ba_grouped, "other")) %>% 
  mutate(
    ba_grouped = str_replace_all(ba_grouped, "_", " "), 
    ba_grouped = factor(ba_grouped, levels = rev(ba_order))
  ) %>%
  group_by(ba_grouped) %>% 
  summarize(n = length(unique(exp_id)), .groups = "drop")  %>% 
  ggplot(aes(out_mono, ba_grouped, fill = n)) + 
  geom_tile() + 
  my_theme + 
  scale_fill_viridis() + 
  labs(x="Outcomes", y = "Brain areas", fill = expression("N"["exp"]), 
       title = "Frequency outcomes across brain areas") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
