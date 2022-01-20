'
  Script: Visualizations frequency brain areas
  Input: complete dataset before preprocessing (mono_complete in temp)
  Output: Figure 2E
  
  Author: v.bonapersona-2@umcutrecht.nl

'
source("src/orders.R")

g_freq_ba <- mono_full %>%
  
  group_by(sex, ba_grouped) %>% 
  summarize(
    n = length(id),
    .groups = "drop"
  ) %>% 
  mutate(
    tot_n = ifelse(sex == "male", nrow(mono_full[mono_full$sex == "male",]), nrow(mono_full[mono_full$sex == "female",])), 
    unique_id = n/tot_n*100
  ) %>%
  mutate(
    ba_grouped = str_replace_all(ba_grouped, "_", " \n"),
    ba_grouped = factor(ba_grouped, levels = rev(ba_order)), 
    sex = ifelse(sex == "male", "males", "females"), 
    sex = factor(sex, levels = c("males","females"))
    
  ) %>%
  ggplot(aes(ba_grouped, unique_id)) + 
  geom_segment(aes(x=ba_grouped ,xend=ba_grouped, 
                   y=0, yend=unique_id), color="grey") +
  geom_point(size = 2) +
  coord_flip() + 
  facet_wrap(~ sex, scales = "free_x") +
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40), 
                     labels = c("0%", "10%", "20%", "30%", "40%")) + 
  # scale_y_discrete(limits = "rev") + 
  labs(
    y = "% comparisons",
    x = "Brain areas"
  ) + 
  my_theme
