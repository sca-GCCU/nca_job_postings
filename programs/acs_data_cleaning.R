# ACS Data Restrictions and Cleaning 
# Name: acs_data_cleaning 
# Date: 7-20-26

# ------------------------------- HOUSEKEEPING ---------------------------------
rm(list = ls())
gc()

# local
setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")
# cluster
#setwd("/home/scanast/nca_job_postings") 

library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(janitor)
library(data.table)


# ----------------------------- LOAD DATA --------------------------------------
acs <- read_csv("data/raw-data/usa_00020_1pct.csv") # change in cluster

acs <- acs %>% clean_names()

n_start <- nrow(acs)

acs <- acs %>%
  filter(age >= 16, age <= 65)

n_drop_age <- n_start - nrow(acs)


# ----------------------- MERGE TREATMENT DATA --------------------------------- 
# Import treatment panel 
state_nca_laws <- read_csv("data/raw-data/state_nca_laws.csv")

# Keep only necessary variables  
state_nca_laws <- state_nca_laws %>%
  select(
    starts_with(c("state", "ban", "health", "eff", "inc_t")), # exclude "enact" vars for now
    ind_coverage
  ) %>%
  select(
    -ends_with("month")
  )

# Merge 
acs <- acs %>%
  left_join(state_nca_laws, by = "statefip")

# Create vectors & dataframes for filtering 
max_year <- max(acs$year)

states_hourly <- state_nca_laws %>%
  filter(!is.na(eff_hourly_year), eff_hourly_year <= max_year) %>%
  pull(var = statefip, name = state)

states_other <- state_nca_laws %>%
  filter(!is.na(eff_other_year), eff_other_year <= max_year) %>%
  pull(var = statefip, name = state)

states_full <- state_nca_laws %>%
  filter(!is.na(eff_full_year), eff_full_year <= max_year) %>% 
  pull(var = statefip, name = state)

# NOTE: Work on a mapping from the below occupations/industries to the ACS.
states_ind <- state_nca_laws %>%
  filter(!is.na(eff_ind_year), eff_ind_year <= max_year) %>%
  select(statefip, state, ind_coverage) %>%
  separate_rows(ind_coverage, sep = ";") %>%
  mutate(
    ind_coverage = str_trim(ind_coverage)
  ) %>%
  rename(coverage = ind_coverage)

states_health1 <- state_nca_laws %>%
  filter(!is.na(eff_health1_year), eff_health1_year <= max_year) %>%
  select(statefip, state, health_coverage1) %>%
  separate_rows(health_coverage1, sep = ";") %>%
  mutate(
    health_coverage1 = str_trim(health_coverage1)
  ) %>%
  rename(coverage = health_coverage1) %>%
  filter(!statefip %in% c(1, 5)) # AL & AR bans already accounted for by ind ban

states_health2 <- state_nca_laws %>%
  filter(!is.na(eff_health2_year), eff_health2_year <= max_year) %>%
  select(statefip, state, health_coverage2) %>%
  separate_rows(health_coverage2, sep = ";") %>%
  mutate(
    health_coverage2 = str_trim(health_coverage2)
  ) %>%
  rename(coverage = health_coverage2)

states_health3 <- state_nca_laws %>%
  filter(!is.na(eff_health3_year), eff_health3_year <= max_year) %>%
  select(statefip, state, health_coverage3) %>%
  separate_rows(health_coverage3, sep = ";") %>%
  mutate(
    health_coverage3 = str_trim(health_coverage3)
  ) %>%
  rename(coverage = health_coverage3)

states_ind <- bind_rows(
  states_ind, 
  states_health1, 
  states_health2, 
  states_health3
)

# ind_to_map <- states_ind %>%
#   group_by(coverage) %>%
#   summarise(
#     coverage = first(coverage),
#     .groups = "drop"
#   )

rm(states_health1, states_health2, states_health3, state_nca_laws)
gc()

# NOTE: Need to carefully use OCC and IND to account for the above industry
# and occupation based bans. See the below links to get started: 
# - OCC 2000-2017: https://usa.ipums.org/usa/volii/occ_acs.shtml
# - OCC 2018-ONWARD: https://usa.ipums.org/usa/volii/occ2018.shtml 
# I had attempted to create a mapping using OCC1990 and IND1990: 
# "coverage_occ1990_ind1990_crosswalk.csv." Might abandon this. 


# -------------------- CREATE KEY TREATMENT VARIABLES --------------------------
# Treatment indicator 
acs <- acs %>%
  mutate(
    treated = as.integer(!is.na(eff_inc1_year) & year >= eff_inc1_year),
    cohort = if_else(is.na(eff_inc1_year), 0, eff_inc1_year)
  )

# ---------------------------- SAVE DATA ---------------------------------------
write_csv(
  acs,
  "data/analysis-data/acs_analysis.csv"
)


