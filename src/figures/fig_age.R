'
  Script: Visualizations frequency age
  Input: output of frequency_df.R which merges mab and neurobiology data
  Output: Figure 3C paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'

g_age <- mono_full %>% 
  ggplot(aes(log(age_testing_weeks), fill = id)) + 
  labs(y = expression("N"["comparisons"]), x = "Weeks (log)", title = "Age") +
  my_hist + my_theme + 
  theme(legend.position = "null") + 
  scale_fill_manual(values = rep("white", n_distinct(mono_full$id))) +
  scale_x_continuous(breaks=c(2.302585,2.995732,
                              3.401197,3.688879,3.912023), labels=c(10, 20, 30, 40, 50))
