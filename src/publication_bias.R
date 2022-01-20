'
  Script: Publication bias assessment
  Input: sex_df data from subgroup_sex.R
  
  Author: v.bonapersona-2@umcutrecht.nl

'


# Environment preparation -------------------------------------------------
source("src/upload_data.R")
sex_df <- readRDS(paste0(final, "sex_df.RDS"))
rest_stress <- readRDS(paste0(final, "rest_stress.RDS"))

# Run univariate models for publication bias ------------------------------
# males
mod_males_publ <- rma(
  yi, vi, 
  method = "REML",
  data = dat_males,
  slab = exp_id
)

# life events
mod_life_publ <- rma(
  yi, vi, 
  method = "REML",
  data = rest_stress,
  slab = exp_id
)

# sex
mod_sex_publ <- rma(
  yi, vi, 
  method = "REML",
  data = sex_df,
  slab = exp_id
)



# Regtests ----------------------------------------------------------------
reg_males <- regtest(mod_males_publ)
reg_life <- regtest(mod_life_publ)
reg_sex <- regtest(mod_sex_publ)


# Visualizations ----------------------------------------------------------

par(mfrow=c(1,3))
  funnel(mod_males_publ, yaxis = "seinv", back = "gray55", hline = "gray55",
         level=c(90, 95, 99), shade=c("white","gray75", "gray65"))
  title("A", adj = 0)
  
  funnel(mod_life_publ, yaxis = "seinv", back = "gray55", hline = "gray55",
         level=c(90, 95, 99), shade=c("white","gray75", "gray65"))
  title("B", adj = 0)
  
  funnel(mod_sex_publ, yaxis = "seinv", back = "gray55", hline = "gray55",
         level=c(90, 95, 99), shade=c("white","gray75", "gray65"))
  title("C", adj = 0)
par(mfrow=c(1,1))
