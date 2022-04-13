### SCRIPT 01

### PRE-PROCESSING AND CLEANING THE DATA FOR THE ANALYSES

library(tidyverse)
library(haven)
library(dplyr)

# Read in the raw data
leger_raw <- read_sas("data/leger1_8.sas7bdat") %>%
  select_all(tolower)

# Create a unique user ID (single digit)
leger_raw <- rowid_to_column(leger_raw)

# Subset the data needed for the analyses
cpa_analysis <- leger_raw %>%
  filter(wave %in% c(2:7)) %>%
  filter(age_yrs <110) %>% # filtering out 3 rows with the values of 205, 333, and 559
  select(prov, sex, age_yrs, hoinc, area, wave, pond, # demographic factors
         impacvd_sq001, impacvd_sq006, impacvd_sq020, # impacts
         contains("hecond")) %>% # self-reported chronic conditions
  dplyr::mutate_at(vars(hecond_sq001, hecond_sq002, hecond_sq003, hecond_sq004,
                        hecond_sq005, hecond_sq006, hecond_sq007, hecond_sq008, 
                        hecond_sq009, hecond_sq010, hecond_sq011), ~ifelse(. == 2, 0, .)) %>% # recode into dummy with 1 and 0 for chronic conditions, easier for counting
  dplyr::mutate_at(vars(impacvd_sq006, impacvd_sq020), ~ifelse(. == 1, 1, 0)) %>% # dichotomized the outcomes for binary logistic regression
  rowwise() %>%
  dplyr::mutate(hecond_immune = coalesce(hecond_sq010, hecond_sq011),
                chronic = sum(c(hecond_sq001, hecond_sq002, hecond_sq003, hecond_sq004, hecond_sq005, hecond_sq006, hecond_sq007, hecond_immune), na.rm = T),
                chronic_dummy = case_when(chronic == 0 ~ 0,
                                          chronic >= 1 ~ 1),
                chronic_multi = case_when(chronic == 0 | chronic == 1 ~ 0, # dummy variables for multiple chronic conditions (2 or more)
                                          chronic > 1 ~ 1),
                chronic_descriptive = case_when(hecond_sq001 == 1 ~ "Heart disease",
                                                hecond_sq002 == 1 ~ "Lung disease",
                                                hecond_sq003 == 1 ~ "Cancer",
                                                hecond_sq004 == 1 ~ "Hypertension",
                                                hecond_sq005 == 1 ~ "Diabetes",
                                                hecond_sq006 == 1 ~ "Obesity",
                                                hecond_sq007 == 1 ~ "Autoimmune",
                                                hecond_immune == 1 ~ "Other"),
                depression = case_when(hecond_sq008 == 2 ~ 0,
                                       hecond_sq008 == 1 ~ 1),
                anxiety = case_when(hecond_sq009 == 2 ~ 0,
                                    hecond_sq009 == 1 ~ 1),
                comorbid_dep_anx = case_when(depression == 0 & anxiety == 0 ~ 0,
                                             depression == 1 & anxiety == 1 ~ 1),
                any_m_health = ifelse(depression == 1 | anxiety == 1, 1, 0)) %>%
  tidyr::replace_na(list(anxiety = 0, depression = 0, comorbid_dep_anx = 0, any_m_health = 0)) %>%
  mutate(sex = factor(sex, levels = c(1:4),
                      labels = c("Male", "Female", "Other", "Prefer not to answer")),
         area = factor(area, levels = c(1:4),
                       labels = c("Rural", "Suburban", "Urban", "Prefer not to answer")),
         hoinc = factor(hoinc, 
                        levels = c(1:4),
                        labels = c("Bottom 3rd", "Middle 3rd", "Top 3rd", "Prefer not to answer")),
         group = case_when(chronic_dummy == 0 & comorbid_dep_anx == 0 ~ 0,
                           chronic_dummy == 0 & comorbid_dep_anx == 1 ~ 1,
                           chronic_dummy == 1 & comorbid_dep_anx == 0 ~ 2,
                           chronic_dummy == 1 & comorbid_dep_anx == 1 ~ 3))

cpa_analysis$impacvd_sq001 <- -cpa_analysis$impacvd_sq001+4 # reverse code the impact so that higher value means

# Save the new data file for the analyses
write_csv(cpa_analysis, "~/Documents/R Codes/CMAJ-leger/CPA_2022/cpa_analyses_april22.csv")
