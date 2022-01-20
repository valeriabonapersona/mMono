'
  Script: sex differences exploratory analysis
  Input: full cleaned dataset data_for_analysis.RDS
  Output: dataset for visualization of sex differences

'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/upload_data.R")


id_sex <- dat_full %>% 
  group_by(id, sex) %>%
  count() %>% 
  pivot_wider(names_from = "sex", values_from = "n") %>%
  drop_na() %>% 
  pull(id)
  
sex_df <- dat_full %>% 
  filter(at_death == "rest",
         !outcome %in% turnovers) %>% 
  group_by(outcome, ba_grouped, sex) %>% 
  summarize(n = length(unique(id))) %>%
  pivot_wider(names_from = "sex", values_from = "n") %>%
  drop_na() %>%
  mutate(keep = "yes") %>%
  select(outcome, ba_grouped, keep) %>% 
  left_join(dat_full %>% filter(at_death == "rest", id %in% id_sex)) %>% 
  filter(keep == "yes") %>%
  filter(!unique_id %in% c("monoamines 210"), # male was removed as outlier
         !exp_id %in% c("16412549_3", "16412549_4"), # too many exp in fem)
         !is.na(id)
  )

saveRDS(sex_df, paste0(final, "sex_df.RDS"))


# Frequencies -------------------------------------------------------------
n_distinct(sex_df$id)
nrow(sex_df)
sex_df %>% group_by(sex) %>% count()


# Model -------------------------------------------------------------------
mod <- rma.mv(
  yi, vi, 
  random = ~ 1|id,
  method = "REML",
  data = sex_df,
  mods = ~ba_grouped:outcome:sex -1,
  slab = cite
)

interactions <- names(data.frame(mod$X))
# main effect
sex_modtest <- anova(mod, L = ifelse(str_detect(interactions, "female"), 
                                     1/(length(interactions)/2), 
                                     -1/(length(interactions)/2)))
against_0 <- c("sexfemale", "sexmale")
contr <-   cbind(
  sapply(against_0, function(x) 
    ifelse(str_detect(interactions, x),
           1/sum(str_detect(interactions,x)),
           0
    )
  ) %>% data.frame()
  
)

contr <- contr %>% t() %>% as.matrix()

res_sex <- summary(multcomp::glht(mod, linfct = contr, df=df.residual(mod)),
                       test=multcomp::adjusted('none'))
rownames(res_sex$linfct) <- str_remove_all(rownames(res_sex$linfct), "sex")

# Create dataset for visualization ----------------------------------------
sex_n <- sex_df %>% 
  group_by(sex) %>% 
  summarize(n_comp = length(at_death), n_id = length(unique(id)), .groups = "drop")

dat_sex_res <- data.frame(
  
  sex = rownames(res_sex$linfct),
  g = res_sex$test$coefficients, 
  sem = res_sex$test$sigma, 
  pvalue = res_sex$test$pvalues
  
) %>% 
  mutate(lab_sig = case_when(
    pvalue < 0.001 ~ "***",
    pvalue < 0.01 ~ "**", 
    pvalue < 0.05 ~ "#",
    T ~ "")) %>%
  left_join(sex_n)


saveRDS(dat_sex_res, paste0(final, "dat_sex_res.RDS"))

