'
  Script: Figure subgroup analysis sex differences
  Input: dat_sex_res data from subgroup_life_exp
  
  Author: v.bonapersona-2@umcutrecht.nl

'

dat_sex_res <- readRDS(paste0(final, "dat_sex_res.RDS"))

g_sex_diff <- dat_sex_res %>%
  ggplot(aes(sex, g)) +
  
  geom_hline(yintercept = 0, color = "black") +
  geom_bar(stat = "identity", colour = "black", fill = "white") +
  geom_errorbar(aes(ymin = g-sem, ymax = g+sem), width = 0.2) +
  scale_x_discrete(limits=rev) + 
  
  # beautiful
  my_theme +
  labs(x="", y = expression(italic("g(se)"))
  ) +
  geom_text(aes(label = lab_sig, y = g-sem-0.2)) +
  geom_text(aes(label = paste("study = ", n_id), y = -1)) +
  geom_text(aes(label = paste("comp = ", n_comp), y = -1.1)) +
  
  theme(text = element_text(size = 20),
        legend.position = "none")
