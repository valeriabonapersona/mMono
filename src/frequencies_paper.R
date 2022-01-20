'
 Script: "Frequencies for publication"

 Author: Valeria Bonapersona
 Contact: v.bonapersona-2 (at) umcutrecht.nl

 Last update:
'

mono_full <- readRDS(paste0(temp, "monoamines_complete.RDS"))

# n publications
n_distinct(mono_full$id)

# year of publishing
min(mono_full$year, na.rm = T)
max(mono_full$year, na.rm = T)

# n experiments
n_distinct(mono_full$exp_id)

# n comparisons
nrow(mono_full)

# species
mono_full %>%
  group_by(species) %>% 
  summarize(per = length(unique(id))/n_distinct(mono_full$id)*100)

# ela model
mono_full %>%
  group_by(model) %>% 
  summarize(per = length(unique(id))/n_distinct(mono_full$id)*100)


# number of animals
mono_full %>% 
  mutate(n_tot = n_c + n_e) %>% 
  select(exp_id, n_tot) %>%
  unique() %>% 
  pull(n_tot) %>% 
  sum()

# percentage males
mono_full %>% 
  filter(sex == "male") %>%
  mutate(n_tot = n_c + n_e) %>% 
  select(exp_id, n_tot) %>%
  unique() %>% 
  pull(n_tot) %>% 
  sum()

# males for analysis
n_distinct(dat_males$id)
n_distinct(dat_males$exp_id)
nrow(dat_males)
