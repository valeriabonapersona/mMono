'
  Script: Analysis males
  Input: males at rest (processed)
  
  Author: v.bonapersona-2@umcutrecht.nl

'



# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/upload_data.R")

out_summary <- dat_males %>% 
  group_by(ba_grouped, out_mono, outcome) %>% 
  summarize(
    across(ends_with("id"), function(x) round(length(unique(x)),2)), 
    .groups = "drop"
  ) %>%
rename(
  n_id = id, 
  n_exp_id = exp_id, 
  n_unique_id = unique_id
) %>% 
  select(-outcome_id)


# Model ----------------------------------------------------------------
mod <- rma.mv(
  yi, vi, 
  random = ~ 1|exp_id,
  
#  random = list(~1 | unique_id, ~1 | exp_id),
  method = "REML",
  #  data = actual_df,
  data = dat_males,
  mods = ~ ba_grouped:outcome -1,
  slab = cite
)

# organize results in df
interactions <- names(data.frame(mod$X))

males_mod <- data.frame(
  inter = names(data.frame(mod$X)),
  g = mod$beta, 
  sem = mod$se,
  pval = mod$pval
) %>% 
  mutate(
    pval = round(pval, 3),
    lab_sig = case_when(
      pval < .001 ~ "***", 
      pval < .01 ~ "**", 
      pval < .05 ~ "#", 
      T ~ ""
    ),
    outcome = str_remove_all(rownames(.), ".*outcome") %>% str_remove_all("\\:.*"),
    ba_grouped = str_remove_all(rownames(.), "ba_grouped") %>% str_remove_all("\\:.*")
  ) %>% 
  left_join(dat_males %>% 
              select(outcome, ba_grouped, out_mono) %>% 
              unique(), by = c("outcome", "ba_grouped")
  ) %>%
  left_join(out_summary %>% 
              select(ba_grouped, out_mono, outcome, n_id, n_unique_id), 
            by = c("outcome", "ba_grouped", "out_mono"))


# Against 0 ---------------------------------------------------------------
## comparisons with sufficient publications (id > 2)
against_0 <- males_mod %>% 
  filter(n_id > 2) %>% 
  pull(inter) %>%
  unique() 

contr <-   cbind(
  sapply(against_0, function(x) 
    ifelse(interactions == x,
           #1/sum(str_detect(interactions,x)),
           1,
           0
    )
  ) %>% data.frame()
  )

contr <- contr %>% t() %>% as.matrix()
res_males_main <- summary(multcomp::glht(mod, linfct = contr, df=df.residual(mod)),
               test=multcomp::adjusted('none'))


# heterogeneity -----------------------------------------------------------
source("src/funs_heterogeneity.R")
get_i2(dat_males, mod)



# Moderator ---------------------------------------------------------------
res1 <- rma.mv(abs(yi), vi,random = ~ 1|exp_id, data=dat_males %>% filter(trauma_presence == "no"), subset=trauma_presence=="no")
res2 <- rma.mv(abs(yi), vi,random = ~ 1|exp_id, data=dat_males %>% filter(trauma_presence == "yes"), subset=trauma_presence=="yes")

dat.comp <- data.frame(estimate = c(coef(res1), coef(res2)), stderror = c(res1$se, res2$se),
                       trauma_presence = c("no","yes"), tau2 = round(c(res1$tau2, res2$tau2),3))

rma(estimate, sei=stderror, mods = ~ trauma_presence, method="FE", data=dat.comp, digits=3)

dat_males %>% 
  group_by(trauma_presence) %>% 
  count()
