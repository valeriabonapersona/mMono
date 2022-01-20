'
  Script: Visualizations for paper
  Input: All graphs previously generated
  Output: Final figures for the paper
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/upload_data.R")



# Figure 2 ----------------------------------------------------------------
# strain
source("src/figures/fig_strain.R")
g_strain

# model
source("src/figures/fig_model.R")
g_model

# age
source("src/figures/fig_age.R")
g_age

# life events
source("src/figures/fig_life_events.R")
g_life

# brain areas
source("src/figures/fig_ba.R")
g_freq_ba

# put together
ggpubr::ggarrange(
  ggpubr::ggarrange(
    g_strain,
    g_model, 
    g_age, 
    nrow = 1, labels = c("A", "B", "C"), widths = c(2, 1.5, 1)
  ),
  g_life,
  g_freq_ba,
  nrow = 3, #heights = c(2,1), 
  labels = c("", "D", "E")
)

ggsave("figs/unedited/fig_2.svg", width = 10, height = 12)


# Fig 3 -------------------------------------------------------------------
source("src/figures/fig_males_ba_out.R")
g_males_ba_out

ggsave("figs/unedited/fig_3.svg", width = 13, height = 10)

# outcomes by brain areas
source("src/figures/fig_outcomebrainareas.R")
g_out_ba


fig_2 <- ggpubr::ggarrange(
  
  ggpubr::ggarrange(
    g_strain, g_model, g_age, 
    nrow = 1, labels = c("A", "B", "C")
  ), 
  
  ggpubr::ggarrange(
    g_freq_ba, g_out_ba,
    nrow = 1, widths = c(0.7,1), labels = c("D", "E")
  ),
  
  nrow = 2
)


ggsave("figs/unedited/fig_2.svg", width = 10, height = 10)


# Main results ------------------------------------------------------------

source("src/figures/fig_males_main.R")
source("src/orders.R")
fig_mono[[1]]
ggsave("figs/unedited/fig_da_main.svg", width = 12, height = 10)
fig_mono[[2]]
ggsave("figs/unedited/fig_ser_main.svg", width = 12, height = 10)
fig_mono[[4]]
ggsave("figs/unedited/fig_ne_main.svg", width = 12, height = 5)
fig_mono[[3]]
ggsave("figs/unedited/fig_enzymes_main.svg", width = 12, height = 7)


# Figure 9 ----------------------------------------------------------------
source("src/figures/fig_life_subgroup.R")
g_life_subgroup

source("src/figures/fig_sex_diff.R")
g_sex_diff

source("src/figures/fig_sex_scatter.R")
g_sex_scatter

ggpubr::ggarrange(
  g_life_subgroup, 
  g_sex_diff, 
  g_sex_scatter, 
  labels = c("A", "B", "C"), 
  widths = c(1.5, 1, 1),
  nrow = 1
)
ggsave("figs/unedited/fig_9.svg", width = 12, height = 4)
