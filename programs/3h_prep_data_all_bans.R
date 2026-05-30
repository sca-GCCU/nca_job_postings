##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "3h_prep_data_all_bans.R" 
# by: Sebastian C. Anastasi
# Date of this version: May 28, 2026
#
# Description:  
#
# Dependencies:  
#
# Output:
##############################################################################

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)

# -------------------- Import & Preliminary Restrictions -----------------------
# --- Import Lightcast data ---

agg2 <- read_csv("data/raw-data/sample_anastasi_agg2_v2.csv")

n_start <- agg2 %>%
  summarise(N = n()) %>%
  pull(N)
n_start
# NOTE: Ignore writing of output to output files for now.

# --- Restrict to firms with at least 10 total listings --- 
agg2 <- agg2 %>%
  group_by(company) %>%
  mutate(
    total_postings_ever = sum(total_postings, na.rm = TRUE)
  ) %>%
  ungroup()

agg2 <- agg2 %>%
  mutate(
    drop_min_ads = (total_postings_ever < 10)
  )

n_drop_noise <- agg2 %>%
  filter(drop_min_ads) %>%
  nrow()
n_drop_noise

agg2 <- agg2 %>%
  filter(!drop_min_ads) %>%
  select(-drop_min_ads, -total_postings_ever)


# ------------------- Merge Treatment Data ------------------------------------- 
# Import treatment panel 
state_nca_laws <- read_csv("data/raw-data/state_nca_laws.csv")

# NOTE: Don't worry about creating date variables. Probably going to use annual
# data.

# Keep only necessary variables  
state_nca_laws <- state_nca_laws %>%
  select(
    starts_with(c("state", "ban", "health")), 
    ends_with(c("year", "month")), 
    ind_coverage
  )

# Rename state variables to Lightcast
state_nca_laws <- state_nca_laws %>%
  rename(state_name = state) %>%
  rename(state = statefip)

# Merge 
agg2_treat <- agg2 %>%
  left_join(state_nca_laws %>% select(-state_name), by = "state")

rm(agg2, state_nca_laws)


# ----------------- Impose Sample Restrictions ---------------------------------
# NOTE: Use method that is robust to more states appearing as treated when the 
# treatment panel expands.

# Create date variable for the above purpose 
agg2_treat <- agg2_treat %>%
  mutate(
    date = make_date(year, month, 1)
  )


# --- Hourly or other-ban states --- 
# NOTE: Since this is for the staggered analysis of income bans, keep those.

# Identify hourly-ban states
states_hourly <- agg2_treat %>%
  mutate(
    date_eff_hourly = make_date(eff_hourly_year, eff_hourly_month, 1),
    treated_eff_hourly = !is.na(date_eff_hourly) & date >= date_eff_hourly
  ) %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    ever_hourly = any(treated_eff_hourly, na.rm = TRUE)
  ) %>%
  filter(ever_hourly) %>%
  pull(var = state, name = state_name)
states_hourly

# Identify other-ban states  
states_other <- agg2_treat %>%
  mutate(
    date_eff_other = make_date(eff_other_year, eff_other_month, 1),
    treated_eff_other = !is.na(date_eff_other) & date >= date_eff_other 
  ) %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    ever_other = any(treated_eff_other, na.rm = TRUE)
  ) %>%
  filter(ever_other) %>%
  pull(var = state, name = state_name)
states_other

# Count number of obs to be dropped 
n_drop_hourly <- agg2_treat %>%
  filter(state %in% states_hourly) %>%
  nrow()
n_drop_hourly

n_drop_other <- agg2_treat %>%
  filter(state %in% states_other) %>%
  nrow()
n_drop_other

# Drop obs 
agg2_treat <- agg2_treat %>%
  filter(
    !(state %in% c(states_hourly, states_other))
  )

rm(states_hourly, states_other)


# --- Industry or occupation-based bans --- 
# NOTE: Excluding listings from the ind/occ affected by the ban in the state 
# with the ban. 


# A. Lawyers 
# NOTE: NCAs are universally banned for lawyers. Lawyer's SOC codes are 
# 23-1010.

n_drop_lawyers <- agg2_treat %>%
  filter(
    soc_4 == "23-1010"
  ) %>%
  nrow()
n_drop_lawyers


# NOTE: THERE ARE CURRENTLY GAPS IN MY CROSSWALK. WORKING ON FIXING THOSE NOW.


# B. Industry Bans

# Identify the states with industry bans 
states_ind <- agg2_treat %>%
  mutate(
    date_eff_ind = make_date(eff_ind_year, eff_ind_month, 1),
    treated_eff_ind = !is.na(date_eff_ind) & date >= date_eff_ind
  ) %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    ind_coverage = first(ind_coverage),
    ever_ind = any(treated_eff_ind, na.rm = TRUE)
  ) %>%
  filter(ever_ind)


# Find the SOC-4 codes 
# NOTE: SOC-4 here appears to be the "broad occupation" group.
ind_crosswalk <- read_csv("data/raw-data/ban_occ_soc_crosswalk.csv") %>%
  mutate(
    ban_occ = str_trim(ban_occ)
  )

states_ind_expansion <- states_ind %>%
  separate_rows(ind_coverage, sep = ";") %>%
  mutate(
    ind_coverage = str_trim(ind_coverage)
  )

states_ind_soc <- states_ind_expansion %>%
  left_join(
    ind_crosswalk,
    by = c("ind_coverage" = "ban_occ"),
    relationship = "many-to-many"
  )


# C. Health Bans 

# Identify the states with health bans 



# Find the SOC-4 codes 





# --- Full ban states --- 



# --- Clean-up unused variables --- 









