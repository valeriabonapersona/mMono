'
  Script: Visualizations frequency model
  Input: full monoamines data
  Output: Figure 2B paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'

order_model <- c("maternal_separation","isolation", 
                 "maternal_deprivation", 
                 "limited_nesting") %>% 
  str_replace_all("_", " \n")

g_model <- mono_full %>% 
  mutate(model = str_replace_all(model, "_", " \n")) %>%
  ggplot(aes(factor(model,levels = order_model), fill = id)) +
  labs(y = expression("N"["comparisons"]), 
       x = "Models", 
       title = "ELA model"
  ) + 
  my_hist + my_theme + 
  theme(legend.position = "none") + 
  scale_fill_manual(values = rep("white", n_distinct(mono_full$id))) 

