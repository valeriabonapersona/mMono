'
  Script: Cleaning manually generated data
  
  Author: Valeria Bonapersona
  
  Input: excel file with relational structure, manually coded
  Output: structural plasticity data to be analyzed
  
'


# Environment preparation -------------------------------------------------
source("config/utilities.R")
source("src/general_funs.R")

# upload data
temp_path <- "~/surfdrive/Work/PhD/mELA_materials/Eline/all_files_mELA_2020/5. Files/outcomes_mELA_VB_v3.xlsx"
dat_xl <- read_my_sheets(temp_path, c("publications", "experiments", "out_monoamines"))

mESP <- readRDS("~/surfdrive/Work/PhD/mESP/data/temp/structural_plasticity_complete.RDS")

# removed deleted rows
dat_xl$out_monoamines <- dat_xl$out_monoamines[1:791,]

# Select outcomes of interest ---------------------------------------------

outcomes <- dat_xl$publications %>% 
  
  # only included
  filter(outcome_selection_final == "include") %>% 
  
  # get outcomes
  pull(summary_outcomes) %>% 
  unique() %>% 
  paste0(collapse = ", ") %>% 
  strsplit(", ") %>% 
  unlist() %>% 
  unique()


# Data selection ----------------------------------------------------------
# Publications
publ_ft <- dat_xl$publications %>% 
  
  # only included and double checked so far
  filter(outcome_selection_final %in% "include") %>%
  
  # clean up df
  rename(id = PMID) %>% 
  dplyr::select(-c("CHECK WITH EXP", starts_with("..."))) %>% 
  
  # summary outcomes to lower
  mutate(
    summary_outcomes = tolower(summary_outcomes), 
    summary_outcomes = str_replace_all(summary_outcomes, "spines", "morphology") %>% 
      str_replace_all("spine", "morphology") %>% 
      str_replace_all("volume", "morphology"), 
    summary_outcomes = 
      str_replace_all(summary_outcomes, "in_vivo_electrophysiology", "invivo_ephys") %>%
      str_replace_all("invivo_electrophysiology", "invivo_ephys") %>% 
      str_replace_all("in_vivo_ephys", "invivo_ephys") %>% 
      str_replace_all("electrophysiology", "ephys") %>%
      str_replace_all("proteomics", "other") %>% 
      str_replace_all("creb", "other") %>% 
      str_replace_all("nmdr", "other") %>% 
      str_replace_all("epigenetics", "(epi)genetics")
  )


# double check that all publ have exp >> must be TRUE
sum(!publ_ft$id %in% dat_xl$experiments$id) == 0

# Experiments -------------------------------------------------------------
exp_ft <- dat_xl$experiments %>% 
  
  # keep only included publ so far
  filter(id %in% publ_ft$id) %>% 
  
  # keep relevant vars
  dplyr::select(id, exp_id, species, strain, origin, model, sex, 
         housing_after_weaning, other_life_experience, main_outcomes) %>% 
  
  # clean_up vars
  mutate(
    housing_after_weaning = ifelse(is.na(housing_after_weaning), "not_specified", housing_after_weaning)
  ) %>% 
  
  # get at death from mESP
  left_join(mESP %>% select(exp_id, at_death), by = "exp_id")
  

## check if any out in the meta-data is not matching actual data
exp_out <- exp_ft %>% 

  # get outcomes
  filter(str_detect(main_outcomes, "dopamine|serotonin|noradrenaline|norepinephrine")) %>% 
  pull(exp_id) %>% 
  unique()

# must be empty. If not >> manually check
exp_out[!exp_out %in% dat_xl$out_monoamines$exp_id]

# monoamines ---------------------------------------------------
mono <- dat_xl$out_monoamines %>% 
  
  # remove empty rows
  filter(!is.na(subdomain_level1),
         outcome != "E", 
         product_measured != "plasma_protein") %>% 
  
  mutate(
    outcome = str_replace_all(outcome, "tyrosine_hydroxilase", "TH") %>% 
      str_replace_all("HVA_DA", "HVA/DA") %>% 
      str_replace_all("DOPAC_DA", "DOPAC/DA") %>% 
      str_replace_all("VMA_NE", "VMA/NE") %>% 
      str_replace_all("5_H", "5H") %>%
      str_replace_all("serotonin", "5HT") %>% 
      str_replace_all("_5HT", "/5HT") %>% 
      str_replace_all("_R", "R"), 
    outcome = ifelse(outcome %in% c("D1R", "D3R"), "D1Rlike", outcome),
    product_measured = tolower(product_measured) %>%
      str_replace_all("mrna", "rna") %>% 
      str_replace_all("signal_", "") %>% 
      str_replace_all("signal", "density"), 
    technique = case_when(
      str_detect(technique, "in_situ") ~ "in_situ",
      str_detect(technique, "pcr|PCR") ~ "pcr", 
      str_detect(technique, "chromatography") ~ "hplc", 
      str_detect(technique, "autoradiog") ~ "autoradiography"
    )
  ) %>%
  
  # motify out_morphology for consistency with mESP
  rename(sys_review_sig = sys_review_significance) %>%
  
  select(-sample_size_retrieved) %>% 
  
  # get publ id
  left_join(exp_ft, by = "exp_id") %>%
  left_join(publ_ft %>% dplyr::select(id, link, authors, year), by = "id") %>%
  
  # get citation
  mutate(
    authors = str_replace_all(authors, "van Riel", "van_Riel") %>%
      str_replace_all("van der Doelen", "van_der_Doelen"),
    authors = word(authors, 1),
    cite = paste0(authors, "(", year,")")) %>%
  
  # keep only included publ so far
  filter(id %in% publ_ft$id) %>% 
  
  # categorize outcomes
  mutate(
    brain_area_hemisphere = case_when(
      is.na(brain_area_hemisphere) ~ "not_specified", 
      brain_area_hemisphere == "balanced|one" ~ "both", 
      T ~ brain_area_hemisphere
    ),
    
    outcome = ifelse(outcome=="DR2", "D2R", outcome),
    outcome = str_replace_all(outcome, "5HIIA", "5HIAA"),
      
    out_grouped = case_when(
      outcome %in% c("NE", "5HT", "DA") ~ "monoamines",
      outcome %in% c("5HIAA/5HT", "VMA", "VMA/NE", "MHPG", "DOPAC", "DOPAC/DA", "HVA",
                     "5HIAA", "3MT", "HVA/DA", "DOPAC_HVA/DA") ~ "metabolites_and_precursors", 
      outcome %in% c("TH", "COMT", "TPH", "TPH2", "MAO_A") ~ "enzymes", 
      outcome %in% c("5HT_2AR", "5HT_2CR", "D1R", "D2R", "DAT", "5HT_1AR", "SERT", "D3R", 
                     "5HT_6R") ~ "receptors_and_transporters", 
      T ~ outcome
    ),
    
    out_mono = case_when(
      outcome %in% c("DA", "DOPAC", "DOPAC/DA", "HVA", "HVA/DA", "DOPAC_HVA/DA", "3MT",
                     "D1Rlike", "D2R", "DAT") ~ "da_related", 
      outcome %in% c("5HT", "5HIAA", "5HIAA/5HT",
                     "5HT_2AR", "5HT_2CR", "5HT_1AR", "SERT", "5HT_6R") ~ "ser_related", 
      outcome %in% c("NE", "VMA", "VMA/NE", "MHPG") ~ "ne_related", 
      T ~ out_grouped
    ),
  
    ba_grouped = case_when(
      str_detect(brain_area_publication, "striatum|nucleus_accumb|pallid|caud") ~ "striatum_and_pallidum",
      str_detect(brain_area_publication, "nucleus_accumb") ~ "nucleus_accumbens",
      str_detect(brain_area_publication, "caud") ~ "caudate_putamen",


      str_detect(brain_area_publication, "hippocamp|dentate|GZ") ~ "hippocampus",
      str_detect(brain_area_publication, "amygda") ~ "amygdala", 
      str_detect(brain_area_publication, "frontal") ~ "prefrontal_cortex",
      str_detect(brain_area_publication, "hypothal|mammilary|suprachiasmatic|incerta|optic") ~ "hypothalamic_nuclei",
      
      
      str_detect(brain_area_publication, "thalam|habenula") ~ "thalamic_nuclei",
      str_detect(brain_area_publication, "gray|raphe|midbrain|medulla|pons|brainstem|colliculus|substant|tegmental") ~ "brainstem_and_midbrain",
      
   #   str_detect(brain_area_publication, "olfact") ~ "olfactory_areas",

      str_detect(brain_area_publication, "ventricles|edinger|internal_capsule|cortex|endopiri|olfact") ~ "other_areas",
    #  str_detect(brain_area_publication, "cortex") ~ "cortex_other",
   #   str_detect(brain_area_publication, "medulla|pons|brainstem") ~ "brainstem",
    #  str_detect(brain_area_publication, "colliculus") ~ "colliculus",
      
      T ~ brain_area_publication
    )
  ) %>%
  
  # transform other_life_exp in meaningful vars
  mutate(
    behavior =  case_when(
      is.na(other_life_experience) ~ "naive", 
      other_life_experience %in% c("sacrificed by perfusion", "sacrificed by decapitation") ~ "naive",
      str_detect(other_life_experience, "morris water|fear conditioning|behaviour including footshock|MWM|FST|swim test|social defeat") ~ "stressful",
      str_detect(other_life_experience, "non-stressful|non stressful|EPM") ~ "non_stressful",
      str_detect(other_life_experience, "behavioral test|behavior tests 4/8 weeks|vaginal smears, behavior tests|behavior") ~ "non_stressful", ## DOUBLE CHECK TYPE BEHAVIOR
      T~ "no_behavior" ## double check if correct
    #  T~ other_life_experience ## double check if correct
      
    ),
   
   major_life_events = case_when(
     behavior == "naive" ~ "no", 
     str_detect(housing_after_weaning, "single") ~ "yes",
     str_detect(other_life_experience, "7day footshock") ~ "yes",
     str_detect(other_life_experience, "chronic restraint stress|chronic variable stress|unpredictable chronic stress|fox odor|chronic stress immobilization|triple stressor|restraint stress|stress immobilization|chronic unpredictable stress|chronic variable mild stress|chronic mild stress") ~ "yes",
     str_detect(other_life_experience, "single housing| single housed|individual housing") ~ "yes",
     str_detect(other_life_experience, "blood collection tail|blood sampl|blood collection") ~ "yes",
     str_detect(other_life_experience, "anesthe|microdial|surgery") ~ "yes",
     
     T~ "no"
     ## double check if correct
     #   T~ other_life_experience ## double check if correct
     
   ), 
    
    at_death = case_when(
      str_detect(acute_experience_description, "rest|unclear") ~ "rest",
      str_detect(acute_experience_description, "footshock|restraint|odor|FST|immobilization|forced swim|MWM|implantation_3h_prior|social defeat|resident intruder") ~ "stressed",
      str_detect(acute_experience_description, "EPM|injection|fasting|novel environment|single house|behavioral experiments") ~ "aroused",
      T ~ acute_experience_description
    )
  ) %>%
  
  # fix summary stats
  ## remove extremely skewed medians
 # filter(!outcome_id %in% c("26836417_1_morph_2", "26836417_1_morph_3")) %>%
  
  # remove outcomes where data not available yet or not retrievable
  filter(!is.na(estimate_e), 
         estimate_e != "not_specified",
         deviation_e != "not_specified") %>%
  mutate(
    estimate_c = case_when(
      estimate_c == "not_applicable" ~ "1",
      str_detect(estimate_c, "not_specified|undet") ~ "0",
      T ~ estimate_c
    ), 
    estimate_c = as.numeric(estimate_c),
    
    deviation_c = case_when(
      deviation_c == "not_applicable" ~ "0.1",
      str_detect(deviation_c, "not_specified|undet") ~ "0",
      T ~ deviation_c
    ),

    estimate_e = ifelse(str_detect(estimate_e, "undet"), "0", estimate_e),
    estimate_e = as.numeric(estimate_e),
    
    deviation_e = ifelse(str_detect(deviation_e,"undet"), "0", deviation_e)
  ) %>%
  
  rowwise() %>%
  mutate(
    
    ## keep only lower sample size 
    n_c = str_replace_all(n_c, "_.*", "") %>% make_numeric(),
    n_e = str_replace_all(n_e, "_.*", "") %>% make_numeric()) %>% 
  
  ### if n not specified, do mean
  ungroup() %>% 
  mutate(
    n_c = ifelse(is.na(n_c), mean(n_c, na.rm = T), n_c),
    n_e = ifelse(is.na(n_e), mean(n_e, na.rm = T), n_e)) %>% 
  
  rowwise() %>% 
  mutate(
    ## here cut_n_c is only "no", so no correction needed
  
    ## convert median to mean
    mean_c = case_when(
      estimate_c_type == "median" ~ median_to_mean(estimate_c,deviation_c),
      T ~ estimate_c),
    mean_e = case_when(
      estimate_e_type == "median" ~ median_to_mean(estimate_e,deviation_e),
      T ~ estimate_e),
    
    ## convert sem and iqr to sd
    sd_c = case_when(
      deviation_c_type == "iqr" ~ iqr_to_sd(estimate_c, deviation_c), 
      deviation_c_type %in% c("sem", "SEM", "SE") ~ sem_to_sd(deviation_c, n_c), 
      T ~ make_numeric(deviation_c)
      ),
    sd_e = case_when(
      deviation_e_type == "iqr" ~ iqr_to_sd(estimate_e, deviation_e), 
      deviation_e_type %in% c("sem", "SEM", "SE") ~ sem_to_sd(deviation_e, n_e), 
      T ~ make_numeric(deviation_e)
      ),
    
    # correct age_testing
    age_testing = ifelse(age_testing == "not_specified", NA, get_mean_range(age_testing)),
    age_testing_weeks = case_when(
      age_testing_unit == "months" ~ age_testing *4,
      age_testing_unit == "days" ~ age_testing/7,
      T ~ age_testing
    ), 
    
    # harmonize data_unit
    data_unit = ifelse(str_detect(data_unit, "/"), str_replace_all(data_unit, " ", ""), data_unit),
    data_unit = case_when(
      str_detect(data_unit, "turnover") ~ "turnover",
      str_detect(data_unit, "arbitrary|density|% increase|B-actin") ~ "arbitrary",
      str_detect(data_unit, "#|cells") ~ "count",
      str_detect(data_unit, "control|basal|relative|ratio|% of HC|fold change") ~ "relative_control",
      T ~ data_unit
    ), 
    data_unit_check = case_when( # check whether deviations are too large of these! Either exclude or sensitivity
      str_detect(data_unit, "relative_control") ~ "yes", 
      T ~ "no"
    )
    
  ) %>%
  
  ## summary statistics corrections
     # make sd_c = sd_e when missing due to technical problems
  mutate(
    sd_c = ifelse(sd_c == 0, sd_e, sd_c), 
    sd_e = ifelse(sd_e == 0, sd_c, sd_e) # these are values below detection level presented in text
  ) %>% 
  
  # group brain areas
  # rename brain areas
  mutate(
    ba_main = case_when(
      str_detect(brain_area_publication, "striatum") ~ "striatum",
      str_detect(brain_area_publication, "nucleus_accumb") ~ "nucleus_accumbens",
      str_detect(brain_area_publication, "pallid") ~ "pallidum",
      str_detect(brain_area_publication, "caud") ~ "caudate_putamen",
      str_detect(brain_area_publication, "substant") ~ "sub_nigra",
      str_detect(brain_area_publication, "tegment") ~ "vta",
      
      
      str_detect(brain_area_publication, "hippocamp|dentate|GZ") ~ "hippocampus",
      str_detect(brain_area_publication, "amygda") ~ "amygdala", 
      str_detect(brain_area_publication, "frontal") ~ "prefrontal_cortex",
      str_detect(brain_area_publication, "hypothal|mammilary|suprachiasmatic|incerta|optic") ~ "hypothalamic_nuclei",
      
      
      str_detect(brain_area_publication, "thalam|habenula") ~ "thalamic_nuclei",
      str_detect(brain_area_publication, "raphe|midbrain|colliculus") ~ "midbrain",
      str_detect(brain_area_publication, "gray|medulla|ponsbrainstem|") ~ "brainstem_and_hindbrain",
      
      str_detect(brain_area_publication, "olfact") ~ "olfactory_areas",
      
      str_detect(brain_area_publication, "ventricles|edinger|internal_capsule|endopiri") ~ "other_areas",
      str_detect(brain_area_publication, "cortex") ~ "other_cortical",
      #   str_detect(brain_area_publication, "medulla|pons|brainstem") ~ "brainstem",
      #  str_detect(brain_area_publication, "colliculus") ~ "colliculus",
      
      T ~ brain_area_publication
    ),
    ba_location = case_when(
      str_detect(brain_area_publication, "dorsal") ~ "dorsal", 
      str_detect(brain_area_publication, "ventral") ~ "ventral", 
      str_detect(brain_area_publication, "basal") ~ "basal", 
      str_detect(brain_area_publication, "caudal") ~ "caudal", 
      str_detect(brain_area_publication, "apical") ~ "apical", 
      str_detect(brain_area_publication, "rostral") ~ "rostral", 
      str_detect(brain_area_publication, "medial") ~ "medial", 
      
    #  T ~ "not_specified"
    T ~ brain_area_publication
    )
  ) %>%
    
  # select vars of interest
  dplyr::select(
    cite, authors, year, link, id, exp_id, outcome_id,
    sex, species, strain, age_testing_weeks, model, origin, housing_after_weaning,   
    behavior, major_life_events, at_death,
    outcome, out_grouped, out_mono, product_measured, technique,
    brain_area_publication, ba_grouped, brain_area_hemisphere, ba_main, ba_location, 
    data_unit, data_unit_check,
    n_c, n_e, mean_c, mean_e, sd_c, sd_e, sys_review_sig
         ) %>%
  unique()

## check that there are no NAs in the summ stats --> must be 0
mono %>% 
 # filter(!str_detect(data_unit, "perc_|fold")) %>% # percentage and fold change might miss deviation
  dplyr::select(c(ends_with("_c"), ends_with("_e"))) %>% 
  is.na() %>% 
  sum() 


## check all deviations are positive
sum(mono$sd_c <= 0, na.rm = T)
sum(mono$sd_e <= 0, na.rm = T)
mono$outcome_id[duplicated(mono$outcome_id)]


# Save temp data ----------------------------------------------------------
saveRDS(mono, paste0(temp, "monoamines_complete.RDS"))
write.csv(mono, paste0(temp, "monoamines_complete.csv"))

