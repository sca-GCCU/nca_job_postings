##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "prep_salary_mn" 
# by: Sebastian C. Anastasi
# Date of this version: March 6, 2026
#
# Description: This script prepares the salary analysis data for examining
# Minnesota's full noncompete ban. 
#
# Dependencies: "prep_covariates.R" 
#
# Output: "salary_mn_analysis.csv"
##############################################################################

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

library(tidyverse)
library(lubridate)

# 1. Load data  
salary <- read_csv("data/raw-data/sample_anastasi_salary_v2.csv")

# Starting observations 
n_start_s <- salary %>%
  summarise(n())
n_start_s



# 2. Merge with NCA treatment panel.

# Load treatment panel 
state_nca_laws <- read.csv("data/raw-data/state_nca_laws.csv")

# Convert year and month variables into date variables 
state_nca_laws <- state_nca_laws %>%
  mutate(
    date_enact_full = make_date(enact_full_year, coalesce(enact_full_month, 1), 1),
    date_eff_full = make_date(eff_full_year, coalesce(eff_full_month, 1), 1),
    date_enact_inc1 = make_date(enact_inc1_year, enact_inc1_month, 1),
    date_eff_inc1 = make_date(eff_inc1_year, eff_inc1_month, 1),
    date_enact_inc2 = make_date(enact_inc2_year, enact_inc2_month, 1),
    date_eff_inc2 = make_date(eff_inc2_year, eff_inc2_month, 1),
    date_enact_hourly = make_date(enact_hourly_year, enact_hourly_month, 1),
    date_eff_hourly = make_date(eff_hourly_year, eff_hourly_month, 1),
    date_enact_ind = make_date(enact_ind_year, enact_ind_month, 1),
    date_eff_ind = make_date(eff_ind_year, eff_ind_month, 1),
    date_enact_health1 = make_date(enact_health1_year, enact_health1_month, 1),
    date_eff_health1 = make_date(eff_health1_year, eff_health1_month, 1),
    date_enact_health2 = make_date(enact_health2_year, enact_health2_month, 1),
    date_eff_health2 = make_date(eff_health2_year, eff_health2_month, 1),
    date_enact_health3 = make_date(enact_health3_year, enact_health3_month, 1),
    date_eff_health3 = make_date(eff_health3_year, eff_health3_month, 1),
    date_enact_other = make_date(enact_other_year, enact_other_month, 1),
    date_eff_other = make_date(eff_other_year, eff_other_month, 1)
  )
# NOTE: I will start the analysis focusing on the date_eff.

# Keep only the vars that you need (ban indicators and dates)
state_nca_laws <- state_nca_laws %>%
  select(
    starts_with(c("state", "ban", "date", "health")), ind_coverage
  )
# NOTE: Since we are creating the Minnesota analysis data, we don't need the 
# income thresholds.

# Rename statefip as state to match Lightcast data 
state_nca_laws <- state_nca_laws %>% 
  rename(state_name = state) %>% 
  rename(state = statefip)

# Merge 
salary_mn_treat <- salary %>%
  left_join(state_nca_laws %>% select(-state_name), by = "state")




# 3. Sample restrictions.
# NOTE: We only care if the bans are active Jan 2010 - Jan 2025 (inclusive).
# Would be nice to have a method that's robust to adding more data.

# A. Exclude income, hourly, and other-ban states

# Create date variable 
salary_mn_treat <- salary_mn_treat %>%
  mutate(
    date = make_date(year, month, 1)
  )
# NOTE: keep year and month for baseline covariate merge

# Create indicators for whether an obs is treated by an active (full, inc1, 
# inc2, etc.) ban. We'll use this for the below exclusions. 
salary_mn_treat <- salary_mn_treat %>%
  mutate(
    treated_eff_full = !is.na(date_eff_full) & date >= date_eff_full,
    treated_enact_full = !is.na(date_enact_full) & date >= date_enact_full, # only need for full ban right now
    treated_eff_inc1 = !is.na(date_eff_inc1) & date >= date_eff_inc1, 
    treated_eff_inc2 = !is.na(date_eff_inc2) & date >= date_eff_inc2,
    treated_eff_hourly = !is.na(date_eff_hourly) & date >= date_eff_hourly,
    treated_eff_ind = !is.na(date_eff_ind) & date >= date_eff_ind,
    treated_eff_health1 = !is.na(date_eff_health1) & date >= date_eff_health1,
    treated_eff_health2 = !is.na(date_eff_health2) & date >= date_eff_health2,
    treated_eff_health3 = !is.na(date_eff_health3) & date >= date_eff_health3,
    treated_eff_other = !is.na(date_eff_other) & date >= date_eff_other
  )

# NOTE: The below method of determining states which ever have the various ban
# types is robust to changes in sample alterations (it excludes bans outside of
# whatever the current sample is).

# Determine which states ever have inc1 ban
states_inc1 <- salary_mn_treat %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    ever_inc1 = any(treated_eff_inc1, na.rm = TRUE)
  ) %>%
  filter(ever_inc1) %>%
  pull(var = state, name = state_name)

# Determine which states ever have inc2 ban
#states_inc2 <- salary_mn_treat %>%
#  group_by(state) %>%
#  summarise(
#    state_name = first(state_name),
#    ever_inc2 = any(treated_eff_inc2, na.rm = TRUE)
#  ) %>%
#  filter(ever_inc2) %>%
#  pull(var = state, name = state_name)

# Determine which states ever have hourly ban
states_hourly <- salary_mn_treat %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    ever_hourly = any(treated_eff_hourly, na.rm = TRUE)
  ) %>%
  filter(ever_hourly) %>%
  pull(var = state, name = state_name)

# Determine which states ever have other ban 
states_other <- salary_mn_treat %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    ever_other = any(treated_eff_other, na.rm = TRUE)
  ) %>%
  filter(ever_other) %>%
  pull(var = state, name = state_name)

# Dropping obs from the states with income, hourly, or "other" bans.
salary_mn_treat <- salary_mn_treat %>%
  filter(
    !(state %in% c(states_inc1, states_hourly, states_other))
  )

# Drop vectors used for exclusion 
rm(states_hourly, states_inc1, states_other)


# B. Exclude listings from banned IND/OCC in states w/ IND/OCC bans

# i.a. Determine which states ever have ind ban 
states_ind <- salary_mn_treat %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    ind_coverage = first(ind_coverage),
    ever_ind = any(treated_eff_ind, na.rm = TRUE)
  ) %>%
  filter(ever_ind)

# NOTE: NCAs are universally banned in all legal professions. However, these 
# observations from these occupations are already excluded from the main anal-
# ysis sample, since it is restricted to high incidence of NCA SOC and
# NAICS 2-digit codes, which don't include lawyers (SOC 2-digit = 23).

# i.b. Find corresponding SOC-4 codes 
# NOTE: SOC-4 here appears to be the "broad occupation" group.
ind_crosswalk <- read.csv("data/raw-data/ban_occ_soc_crosswalk.csv") %>%
  mutate(
    ban_occ = str_trim(ban_occ),
  )

states_ind_exp <- states_ind %>%
  separate_rows(ind_coverage, sep = ";") %>%
  mutate(
    ind_coverage = str_trim(ind_coverage)
  )

# Testing that everything will merge properly  
#states_ind_exp %>%
#  anti_join(ind_crosswalk, by = c("ind_coverage" = "ban_occ"))

states_ind_soc <- states_ind_exp %>%
  left_join(
    ind_crosswalk, 
    by = c("ind_coverage" = "ban_occ"),
    relationship = "many-to-many"
  ) %>%
  select(-ever_ind, -note) %>%
  rename(soc_4 = broad_occ_soc)


# ii.a. Determine which states ever have health bans
states_health <- salary_mn_treat %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    ever_health1 = any(treated_eff_health1, na.rm = TRUE),
    health_coverage1 = first(health_coverage1),
    ever_health2 = any(treated_eff_health2, na.rm = TRUE),
    health_coverage2 = first(health_coverage2),
    ever_health3 = any(treated_eff_health3, na.rm = TRUE),
    health_coverage3 = first(health_coverage3)
  ) %>%
  filter(ever_health1)

# Drop Alabama since it's health ban is subsumed by its industry ban
states_health <- states_health %>%
  filter(!(state == 1))


# ii.b. Find corresponding SOC-4 codes 

# Expand coverage cells 
states_health_exp <- states_health %>%
  separate_rows(health_coverage1, sep = ";") %>%
  separate_rows(health_coverage2, sep = ";") %>%
  separate_rows(health_coverage3, sep = ";") %>%
  mutate(
    health_coverage1 = str_trim(health_coverage1),
    health_coverage2 = str_trim(health_coverage2),
    health_coverage3 = str_trim(health_coverage3)
  ) 
# NOTE: The above is robust to eventually have non-missing values in 
# health_coverage3. 

# Clean up some variables I don't need
states_health_exp <- states_health_exp %>%
  select(-starts_with("ever"))

# Merge in SOC codes 
states_health_long <- states_health_exp %>%
  pivot_longer(
    cols = starts_with("health_coverage"),
    names_to  = "coverage_source",   # optional: keeps track of 1/2/3
    values_to = "health_coverage"
  ) %>%
  mutate(health_coverage = str_trim(health_coverage)) %>%
  filter(!is.na(health_coverage), health_coverage != "") %>%
  select(state, state_name, health_coverage) %>% # currently dropping coverage_source
  distinct()

states_health_soc <- states_health_long %>%
  left_join(
    ind_crosswalk, 
    by = c("health_coverage" = "ban_occ"),
    relationship = "many-to-many"
  ) %>%
  select(-note) %>%
  rename(soc_4 = broad_occ_soc)


# iii. Use anti_join to exclude observations from banned occupations by state. 

# Check that all soc_4 codes from states_ind_soc have match.
missing_soc <- states_ind_soc %>%
  anti_join(
    salary_mn_treat %>% distinct(soc_4),
    by = "soc_4"
  ) 
# NOTE: Some not matched, but I think this may because there are no listings
# of that type.

rm(missing_soc)

# Dropping obs in treated occupations in states with ind_bans 
n_drop_ind_ban <- salary_mn_treat %>%
  semi_join(
    states_ind_soc,
    by = c("state", "soc_4")
  ) %>%
  summarise(n())
n_drop_ind_ban

salary_mn_treat <- salary_mn_treat %>%
  anti_join(
    states_ind_soc,
    by = c("state", "soc_4")
  )

# Check that all soc_4 codes from states_health_soc have a match.
missing_soc <- states_health_soc %>%
  anti_join(
    salary_mn_treat %>% distinct(soc_4),
    by = "soc_4"
  ) 
# NOTE: No soc_4 codes missing. Reassuring. 

rm(missing_soc)

# Dropping obs in treated occupations in states with health_bans
n_drop_health_ban <- salary_mn_treat %>%
  semi_join(
    states_health_soc,
    by = c("state", "soc_4")
  ) %>%
  summarise(n())
n_drop_health_ban

salary_mn_treat <- salary_mn_treat %>%
  anti_join(
    states_health_soc,
    by = c("state", "soc_4")
  )


# C. Exclude other full-ban states (CA, ND, OK)

states_full <- salary_mn_treat %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    ever_full = any(treated_eff_full, na.rm = TRUE)
  ) %>%
  filter(ever_full) %>%
  filter(!(state == 27)) %>% # filter out Minnesota 
  pull(state, state_name) # vectorize

n_drop_full <- salary_mn_treat %>%
  filter(state %in% states_full) %>%
  summarise(n())
n_drop_full

salary_mn_treat <- salary_mn_treat %>%
  filter(!(state %in% states_full))


# D. Clean up data frames and variables you don't need anymore 

# Data frames 
rm(salary, ind_crosswalk, state_nca_laws)
rm(list = ls(pattern = "^states"))

# Variables 
# NOTE: Keep in mind, this dataset is for the MN analysis, so we don't really 
# need variables pertaining to other types of bans once exclusions have been
# imposed. 

salary_mn_treat <- salary_mn_treat %>%
  select(
    -starts_with("treated_eff_in"), 
    -starts_with("treated_eff_h"),
    -treated_eff_other,
    -starts_with("health"),
    -ind_coverage, 
    -(ban_inc1:ban_other),
    -(date_enact_inc1:date_eff_other)
  )





# 4. Merge with covariate data

# A. Create covariates ("prep_covariates.R")

# i. Create baseline version from this 
# NOTE: Baseline for the MN samples can be the year before MN's ban: 2022. 
base_year <- 2022 # year before MN ban

covariates <- read_csv("data/clean-data/covariates_a_clean.csv")

covariates_base <- covariates %>%
  filter(
    year == base_year
  )


# B. Merge the covariate data 

salary_mn_analysis <- salary_mn_treat %>%
  left_join(
    covariates_base,
    by = c("state"),
    relationship = "many-to-one"
  ) %>%
  select(
    -state_name.y,
    -year.y
  ) %>%
  rename(
    state_name = state_name.x,
    year = year.x
  )

rm(salary_mn_treat, covariates, covariates_base)



# 5. Convert salary info to a real dollars using CPI. All in 2022 dollars 
# (to match the base period).

cpi <- read_csv("data/clean-data/cpi_clean.csv")

salary_mn_analysis <- salary_mn_analysis %>%
  left_join(
    cpi,
    by = "date",
    relationship = "many-to-one"
  ) %>%
  select(
    -year.y,
    -month.y
  ) %>%
  rename(
    year = "year.x",
    month = "month.x"
  )

salary_mn_analysis <- salary_mn_analysis %>%
  mutate(
    real_salary = salary * cpi_deflator,
    real_salary_from = salary_from * cpi_deflator,
    real_salary_to = salary_to * cpi_deflator
  ) %>%
  select(
    -cpi,
    -cpi_deflator
  )

rm(cpi)

write_csv(salary_mn_analysis, "data/analysis-data/salary_mn_analysis.csv")







