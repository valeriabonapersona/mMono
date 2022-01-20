'
  Script: Visualizations frequency life events
  Input: output of frequency_df.R which merges mab and neurobiology data
  Output: Figure 2 paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'

order_lifeevents <- c("rats", "mice", "males", "females",
                      "naive", "no \nbehav.", "non stressful",
                      "+1 hit", "+2 hits", "+3 hits", 
                      "rest", "aroused", "stressed") %>% 
  rev()

df_life <- mono_full %>% 
  
  mutate(
    origin_num = ifelse(origin == "purchased_pregnant_dams", 1, 0),
    behavior_num = ifelse(behavior == "stressful", 1, 0),
    chronic_num = ifelse(major_life_events == "yes", 1, 0), # single housing is together with major life events
    trauma_score = rowSums(across(ends_with("_num"))), 
    trauma_presence = ifelse(trauma_score < 1, "no", "yes"),
    
    life_events = case_when(
      behavior == "naive" ~ "naive", 
      trauma_score == 1 ~ "+1 hit",
      trauma_score == 2 ~ "+2 hits",
      trauma_score == 3 ~ "+3 hits", 
      behavior == "no_behavior" ~ "no \nbehav.",
      T ~ behavior
    ), 
    sex = str_replace_all(sex, "male", "males"), 
    species = ifelse(species == "rat", "rats", "mice")
    ) %>%
  rename(at_testing = at_death) %>%
  
  select(sex, species, life_events, at_testing) %>%
  unique() %>%
  pivot_longer(cols = everything(), names_to = "vars") %>%
  group_by(vars, value) %>% 
  summarise(
    n = length(vars), 
    perc = n/nrow(mono_full)*100, 
    .groups = "drop"
  ) %>%
  mutate(
    y_pos = n/2,
    lab = str_replace_all(value, "_", " ")
   )

g_life <- df_life %>%
  ggplot(aes(str_replace_all(vars, "_", " "), y = perc, 
             label = factor(lab, levels = order_lifeevents), 
             fill = factor(lab, levels = order_lifeevents))) + 
  geom_bar(stat = "identity", colour = "black") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) + 
  labs(
    y = "% comparisons", 
    x = ""
  ) + 
  my_theme +
  theme(legend.position = "null") + 
  scale_fill_manual(values = rep("white", n_distinct(mono_full$id))) + 
  coord_flip()
