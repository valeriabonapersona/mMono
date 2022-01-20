'
  Script: Visualizations frequency strain
  Input: output of frequency_df.R which merges mab and neurobiology data
  Output: Figure 3A paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'

order_strain <- c("sprague_dawley", "wistar", "long_evans", "lister_hooded",
                  "not_specified", "c57bl6", "swiss_webster", "cd1") %>% 
  str_replace_all("_", " \n")

g_strain <- mono_full %>% 
  mutate(strain = str_replace_all(strain, "_", " \n")) %>%
  ggplot(aes(factor(strain,levels = order_strain), fill = id)) +
  labs(y = expression("N"["comparisons"]), 
       x = "Strain", 
       title = "Species"
  ) + 
  my_hist + my_theme + 
  theme(legend.position = "none") + 
  facet_grid(~fct_rev(species), scales = "free_x", space = "free_x") + 
  scale_fill_manual(values = rep("white", n_distinct(mono_full$id))) 

