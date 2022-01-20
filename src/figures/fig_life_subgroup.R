'
  Script: Figure subgroup analysis life events
  Input: rest_stress data from subgroup_life_exp
  
  Author: v.bonapersona-2@umcutrecht.nl

'

g_dat_atdeath <- readRDS(paste0(final, "dat_subgroup_life.RDS"))

g_life_subgroup <- g_dat_atdeath %>%
  ggplot(aes(str_replace_all(at_death,"_", " "), g)) +
  
  geom_hline(yintercept = 0, color = "black") +
  geom_bar(stat = "identity", colour = "black", fill = "white") +
  geom_errorbar(aes(ymin = g-sem, ymax = g+sem), width = 0.2) +
  scale_x_discrete(limits=rev) + 
  
  # beautiful
  my_theme +
  labs(x="", y = expression(italic("g(se)"))
  ) +
  facet_grid(~factor(trauma_presence, levels = c("no other hit", "+ hits"))) + 
  geom_text(aes(label = lab_sig, y = g-sem-0.2)) +
  geom_text(aes(label = paste("study = ", n_id), y = -2.6)) +
  geom_text(aes(label = paste("comp = ", n_comp), y = -2.8)) +
  
  theme(text = element_text(size = 20),
        legend.position = "none")
