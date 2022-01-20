'
  Script: Figure scatter comparison males and female effect sizes
  Input: sex_df data from subgroup_sex.R
  
  Author: v.bonapersona-2@umcutrecht.nl

'
sex_df <- readRDS(paste0(final, "sex_df.RDS"))

g_sex_scatter <- sex_df %>%
  mutate(
    id = ifelse(exp_id %in% c("21396433_3", "21396433_4"), "21396433_b", id)
  ) %>%
  select(id, outcome, ba_grouped, yi, sex) %>%
  pivot_wider(names_from = "sex", values_from = "yi") %>%
  ggplot(aes(abs(male), abs(female))) + 
  geom_point() + 
  my_theme +
  geom_abline(slope = 1, intercept = 0) + 
  ylim(c(0,5)) + xlim(c(0,5)) + 
  
  labs(y = expression(abs(italic(g))["females"]),
       x = expression(abs(italic(g))["males"])) + 
  
  theme(text = element_text(size = 20),
        legend.position = "none")
