##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "prep_agg1_mn" 
# by: Sebastian C. Anastasi
# Date of this version: February 27, 2026
#
# Description: This script prepares the occupation-state-month level analysis 
# data for analyzing Minnesota's full noncompete ban. 
#
# Dependencies: "prep_ind_ban_codes.R", "prep_covariates.R" 
##############################################################################

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

library(tidyverse)
library(lubridate)

# 1. Restrict to occupation-state-month cells with at 10 total listings. 

# NOTE: Currently using sample data here. 
agg1_mn <- read.csv("data/raw-data/sample_anastasi_agg1_v2.csv")

# Add flag for obs to drop
agg1_mn <- agg1_mn %>%
  mutate(drop_min_ads = total_postings < 10)

# Count obs that will be dropped 
agg1_mn %>% summarise(n_dropped = sum(drop_min_ads, na.rm = TRUE))

# Drop obs 
agg1_mn <- agg1_mn %>% 
  filter(!drop_min_ads) %>%
  select(-drop_min_ads)



# 2. Merge with NCA treatment panel.

# Load treatment panel 
state_nca_laws <- read.csv("data/raw-data/state_nca_laws.csv")

# Convert year and month variables into date variables 
state_nca_laws <- state_nca_laws %>%
  mutate(
    date_enact_full = make_date(enact_full_year, enact_full_month, 1),
    date_eff_full = make_date(eff_full_year, eff_full_month, 1),
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
  select(starts_with(c("state", "ban", "date")))
  # NOTE: Since we are creating the Minnesota analysis data, we don't need the 
  # income thresholds.

# Rename statefip as state to match Lightcast data 
state_nca_laws <- state_nca_laws %>% 
  rename(state_name = state) %>% 
  rename(state = statefip)

# Merge 
agg1_mn_treat <- agg1_mn %>%
  left_join(state_nca_laws %>% select(-state_name), by = "state")



# 3. Sample restrictions.
# NOTE: We only care if the bans are active Jan 2010 - Jan 2025 (inclusive).
# Would be nice to have a method that's robust to adding more data.

# A. Exclude income-ban states (including Nevada's hourly workers ban)

# Create date variable 
agg1_mn_treat <- agg1_mn_treat %>%
  mutate(
    date = make_date(year, month, 1)
  ) %>%
  select(-c(year, month))

# Create indicators for whether an obs is treated by an active (full, inc1, 
# inc2, etc.) ban. We'll use this for the below exclusions. 
agg1_mn_treat <- agg1_mn_treat %>%
  mutate(
    treated_eff_full = !is.na(date_eff_full) & date >= date_eff_full,
    treated_eff_inc1 = !is.na(date_eff_inc1) & date >= date_eff_inc1, 
    treated_eff_inc2 = !is.na(date_eff_inc2) & date >= date_eff_inc2,
    treated_eff_hourly = !is.na(date_eff_hourly) & date >= date_eff_hourly,
    treated_eff_ind = !is.na(date_eff_ind) & date >= date_eff_ind,
    treated_eff_health1 = !is.na(date_eff_health1) & date >= date_eff_health1,
    treated_eff_health2 = !is.na(date_eff_health2) & date >= date_eff_health2,
    treated_eff_health3 = !is.na(date_eff_health3) & date >= date_eff_health3,
    treated_eff_other = !is.na(date_eff_other) & date >= date_eff_other
  )

# Determine which states ever have inc1 ban
states_inc1 <- agg1_mn_treat %>%
  group_by(state) %>%
  summarise(ever_inc1 = any(treated_eff_inc1, na.rm = TRUE)) %>%
  filter(ever_inc1) %>%
  pull(state)


# Determine which states ever have inc2 ban


# Dropping obs from the states indicated above.



# B. Exclude listings from banned IND/OCC in states w/ IND/OCC bans

# i. Identify treated 4-digit SOC codes to exclude for each state. 
# (SEPARATE SCRIPT)

# Match the states to the variable names in the dataframe and exclude based 
# on the values of the respective vector. 


# C. Clean up variables you don't need



# 4. Merge with covariate data

# A. Create baseline covariates (SEPARATE SCRIPT)

# B. Merge the covariate data 

# C. Clean up variables you don't need 


