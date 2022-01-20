'
  Script: Life experience subgroup analysis
  Input: full processed dataset (data_for_analysis.RDS)
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/upload_data.R")

# prepration dataset
rest_stress <- dat_full %>% 
  filter(sex == "male",
         !outcome %in% turnovers) %>% 
  mutate(at_death = ifelse(at_death != "rest", "not_rest", "rest")) %>%
  group_by(outcome, ba_grouped, at_death, trauma_presence) %>% 
  count() %>%
  pivot_wider(names_from = "at_death", values_from = "n") %>% 
  drop_na() %>%
  mutate(keep = "yes") %>%
  select(outcome, ba_grouped, keep) %>% 
  left_join(dat_full %>% filter(sex == "male")
  ) %>% 
  filter(keep == "yes") %>% 
  mutate(at_death = ifelse(at_death == "rest", "rest", "not_rest"))

saveRDS(rest_stress, paste0(final, "rest_stress.RDS"))


# Model -------------------------------------------------------------------
mod <- rma.mv(
  yi, vi, 
  random = ~ 1|id,
  method = "REML",
  data = rest_stress,
  mods = ~ba_grouped:outcome:at_death:trauma_presence -1,
  slab = cite
)

## visualize all and check what's significant
interactions <- names(data.frame(mod$X))
atdeath_modtest <- anova(mod, L = rep(1/length(interactions), length(interactions)))


against_0 <- c("at_deathrest.trauma_presenceno", 
               "at_deathrest.trauma_presenceyes", 
               "at_deathnot_rest.trauma_presenceno", 
               "at_deathnot_rest.trauma_presenceyes"
)

contr <-   cbind(
  ifelse(
    str_detect(interactions, "at_deathnot_rest"), 
    -1 / (sum(str_detect(interactions, "at_deathnot_rest"))),
    1 / (length(interactions) - sum(str_detect(interactions, "at_deathnot_rest")))
  ),
  ifelse(
    str_detect(interactions, "trauma_presenceno"), 
    -1 / (sum(str_detect(interactions, "trauma_presenceno"))),
    1 / (length(interactions) - sum(str_detect(interactions, "trauma_presenceno")))
  ),
  sapply(against_0, function(x) 
    ifelse(str_detect(interactions, x),
           1/sum(str_detect(interactions,x)),
           0
    )
  ) %>% data.frame()
  
)

contr <- contr %>% t() %>% as.matrix()

res_atdeath <- summary(multcomp::glht(mod, linfct = contr, df=df.residual(mod)),
                       test=multcomp::adjusted('none'))
rownames(res_atdeath$linfct) <- c("rest_vs_not_rest", "trauma_yes_vs_no", 
                                  "rest_trauma_no", "rest_trauma_yes",
                                  "not_rest_trauma_no", "not_rest_trauma_yes")


# Create dataset for visualization ----------------------------------------

g_dat_atdeath <- data.frame(
  
  group = rownames(res_atdeath$linfct),
  g = res_atdeath$test$coefficients, 
  sem = res_atdeath$test$sigma, 
  pvalue = res_atdeath$test$pvalues
  
) %>% 
  mutate(lab_sig = case_when(
    pvalue < 0.001 ~ "***",
    pvalue < 0.01 ~ "**", 
    pvalue < 0.05 ~ "#",
    T ~ ""))

rest_n <- rest_stress %>% 
  group_by(at_death, trauma_presence) %>% 
  summarize(n_comp = length(at_death), n_id = length(unique(id)), .groups = "drop") %>% 
  mutate(group = paste0(at_death, "_trauma_", trauma_presence))

g_dat_atdeath <- g_dat_atdeath[(nrow(g_dat_atdeath)-3):nrow(g_dat_atdeath),] %>% 
  left_join(rest_n, by = "group") %>% 
  mutate(
    trauma_presence = ifelse(str_detect(group, "trauma_yes"), "+ hits", "no other hit")
  )

saveRDS(g_dat_atdeath, paste0(final, "dat_subgroup_life.RDS"))
