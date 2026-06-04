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

# NOTE: 
# - Don't create date variables. Probably going to use annual data.
# - Don't include year and month of enactment variables for now.
# - Don't include note about income coverage right now (may want in a table).

# Keep only necessary variables  
state_nca_laws <- state_nca_laws %>%
  select(
    starts_with(c("state", "ban", "health", "eff", "inc_t")), # exclude "enact" vars for now
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

agg2_treat <- agg2_treat %>%
  filter(soc_4 != "23-1010")


# B. Industry Bans

    # --- SOC-based restrictions --- 

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

states_ind_expansion <- states_ind %>%
  separate_rows(ind_coverage, sep = ";") %>%
  mutate(
    ind_coverage = str_trim(ind_coverage)
  )

# Find the SOC-4 codes 

# NOTE: Lightcast's "SOC-4" appears to be the "broad occupation" group.

soc_crosswalk <- read_csv("data/raw-data/ban_occ_soc_crosswalk.csv") %>%
  mutate(
    ban_occ = str_trim(ban_occ)
  )

# NOTE: Now that I'm filtering some of these bans using NAICS, there will be NA 
# values that I have to drop. 

states_ind_soc <- states_ind_expansion %>%
  left_join(
    soc_crosswalk,
    by = c("ind_coverage" = "ban_occ"),
    relationship = "many-to-many"
  ) %>%
  select(-ever_ind, -note) %>%
  rename(soc_4 = broad_occ_soc) %>%
  filter(!is.na(soc_4))

# NOTE: Alabama and Arkansas' bans (while not specifically health bans), ban 
# NCAs for some health occupations. Hence I do drop some health occupation 
# listings here already. 

n_drop_ind_soc <- agg2_treat %>%
  semi_join(
    states_ind_soc,
    by = c("state", "soc_4")
  ) %>%
  nrow()

agg2_treat <- agg2_treat %>%
  anti_join(
    states_ind_soc,
    by = c("state", "soc_4")
  )

    # --- NAICS-based restrictions --- 

naics_crosswalk <- read_csv("data/raw-data/ban_ind_naics_crosswalk.csv") %>%
  mutate(
    ban_desc = str_trim(ban_desc)
  )

states_ind_naics <- states_ind_expansion %>%
  left_join(
    naics_crosswalk,
    by = c("ind_coverage" = "ban_desc"),
    relationship = "many-to-many"
  ) %>%
  select(-ever_ind, -note) %>%
  filter(!is.na(naics4))

n_drop_ind_naics <- agg2_treat %>%
  semi_join(
    states_ind_naics,
    by = c("state", "naics4")
  ) %>%
  nrow()

agg2_treat <- agg2_treat %>%
  anti_join(
    states_ind_naics,
    by = c("state", "naics4")
  )

rm(states_ind_soc, states_ind_naics, states_ind, states_ind_expansion)


# C. Health Bans 

# NOTE: All of the health ban restrictions are based on SOC codes, not NAICS. 

# Filter: health1_ban

state_health1 <- agg2_treat %>%
  mutate(
    date_eff_health1 = make_date(eff_health1_year, eff_health1_month, 1),
    treated_eff_health1 = !is.na(date_eff_health1) & date >= date_eff_health1
  ) %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    health_coverage1 = first(health_coverage1),
    ever_health1 = any(treated_eff_health1, na.rm = TRUE)
  ) %>%
  filter(ever_health1)

state_health1_expansion <- state_health1 %>%
  separate_rows(health_coverage1, sep = ";") %>%
  mutate(health_coverage1 = str_trim(health_coverage1)) %>%
  filter(!state %in% c(1, 5)) # drop AL and AR since ind ban filter covers them 

state_health1_soc <- state_health1_expansion %>% 
  left_join(
    soc_crosswalk,
    by = c("health_coverage1" = "ban_occ"),
    relationship = "many-to-many"
  ) %>%
  select(-ever_health1, -note) %>%
  rename(soc_4 = broad_occ_soc)

n_drop_health1 <- agg2_treat %>%
  semi_join(
    state_health1_soc,
    by = c("state", "soc_4")
  ) %>%
  nrow()

agg2_treat <- agg2_treat %>%
  anti_join(
    state_health1_soc,
    by = c("state", "soc_4")
  ) 

rm(state_health1_soc, state_health1_expansion, state_health1)


# Filter: health2_ban

state_health2 <- agg2_treat %>%
  mutate(
    date_eff_health2 = make_date(eff_health2_year, eff_health2_month, 1),
    treated_eff_health2 = !is.na(date_eff_health2) & date >= date_eff_health2
  ) %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    health_coverage2 = first(health_coverage2),
    ever_health2 = any(treated_eff_health2, na.rm = TRUE)
  ) %>%
  filter(ever_health2)

state_health2_expansion <- state_health2 %>%
  separate_rows(health_coverage2, sep = ";") %>%
  mutate(health_coverage2 = str_trim(health_coverage2))

state_health2_soc <- state_health2_expansion %>% 
  left_join(
    soc_crosswalk,
    by = c("health_coverage2" = "ban_occ"),
    relationship = "many-to-many"
  ) %>%
  select(-ever_health2, -note) %>%
  rename(soc_4 = broad_occ_soc)

n_drop_health2 <- agg2_treat %>%
  semi_join(
    state_health2_soc,
    by = c("state", "soc_4")
  ) %>%
  nrow()

agg2_treat <- agg2_treat %>%
  anti_join(
    state_health2_soc,
    by = c("state", "soc_4")
  ) 

rm(state_health2_soc, state_health2_expansion, state_health2)


# Filter: health3_ban

state_health3 <- agg2_treat %>%
  mutate(
    date_eff_health3 = make_date(eff_health3_year, eff_health3_month, 1),
    treated_eff_health3 = !is.na(date_eff_health3) & date >= date_eff_health3
  ) %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    health_coverage3 = first(health_coverage3),
    ever_health3 = any(treated_eff_health3, na.rm = TRUE)
  ) %>%
  filter(ever_health3)

state_health3_expansion <- state_health3 %>%
  separate_rows(health_coverage3, sep = ";") %>%
  mutate(health_coverage3 = str_trim(health_coverage3))

state_health3_soc <- state_health3_expansion %>% 
  left_join(
    soc_crosswalk,
    by = c("health_coverage3" = "ban_occ"),
    relationship = "many-to-many"
  ) %>%
  select(-ever_health3, -note) %>%
  rename(soc_4 = broad_occ_soc)

n_drop_health3 <- agg2_treat %>%
  semi_join(
    state_health3_soc,
    by = c("state", "soc_4")
  ) %>%
  nrow()

agg2_treat <- agg2_treat %>%
  anti_join(
    state_health3_soc,
    by = c("state", "soc_4")
  ) 

rm(state_health3_soc, state_health3_expansion, state_health3)
rm(soc_crosswalk, naics_crosswalk)

# --- Full ban states (CA, MN, ND, OK) --- 

states_full <- agg2_treat %>%
  mutate(
    date_eff_full = make_date(
      eff_full_year,
      coalesce(eff_full_month, 1),
      1
    ),
    treated_eff_full = !is.na(date_eff_full) & date >= date_eff_full
  ) %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    ever_full = any(treated_eff_full, na.rm = TRUE)
  ) %>%
  filter(ever_full) %>%
  pull(var = state, name = state_name)

n_drop_full <- agg2_treat %>%
  filter(state %in% states_full) %>%
  nrow()

agg2_treat <- agg2_treat %>%
  filter(!state %in% states_full)


# --- Clean-up unneeded variables --- 

agg2_treat <- agg2_treat %>%
  select(
    -ban_full, 
    -starts_with("ban_h"),
    -ban_ind,
    -ban_other,
    -starts_with("health"),
    -ind_coverage,
    -starts_with("eff_full"),
    -starts_with("eff_h"),
    -starts_with("eff_ind"),
    -starts_with("eff_other")
  )

# NOTE: I don't think I do actually need to create share variables at this 
# stage. How I construct them may be slightly dependent on the level of 
# aggregation at which I run the analysis.


# -------------------------- Merge Covariate Data ------------------------------

# QUESTION: How to create baseline covariates in a staggered set-up?


# --------------- Convert Nominal Variables to Real Variables ------------------

# NOTE: Income income thresholds should be converted by hand, as they don't vary
# with the date variable in the panel. 



