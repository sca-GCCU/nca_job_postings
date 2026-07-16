##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "3i_prep_data_sab.R" 
# by: Sebastian C. Anastasi
# Date of this version: July 16, 2026
#
# Description: Generates the occ-state-year level analysis data to be used 
# to evaluate states' staggered NCA bans. Also creates a crude state-year level
# dataset for plotting raw means and treatment rollout. 
#
# Dependencies:  
#
# Output:
##############################################################################

# NOTE: Change to actual dataset and not sample version for cluster run.

# ---------------------------- HOUSEKEEPING ------------------------------------
rm(list = ls())
gc()

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")
#setwd("/home/scanast/nca_job_postings") # for cluster run

# --- Load packages ---
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)

# --- Helper to export scalars to LaTex ---
save_stat <- function(x, name, digits = 0, big_mark = ",", dir = "output/other"){
  formatted <- format(round(x, digits), big.mark = big_mark, scientific = FALSE, trim = TRUE)
  cat(formatted, file = file.path(dir, paste0(name, "_sab.tex"))) # sab = soc all bans
}


# ------------------------------ LOAD RAW DATA ---------------------------------
agg2 <- read_csv("data/raw-data/sample_anastasi_agg2_v2.csv") # change in cluster


# ----------------------------- DATA CLEANING ----------------------------------
# --- Drop Obs Outside Sample Window --- 
# NOTE: Dropping 2025 because we don't currently have the whole year.
agg2 <- agg2 %>% filter(year < 2025)


# --- Starting Postings --- 
postings_start <- sum(agg2$total_postings, na.rm = TRUE)
save_stat(postings_start, "postings_start")


# --- Merge with Treatment Data --- 
# NOTE: 
# - Don't create date variables. Probably going to use annual data.
# - Don't include year and month of enactment variables for now.
# - Don't include note about income coverage right now (may want in a table).

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

# Rename state variables to Lightcast
state_nca_laws <- state_nca_laws %>%
  rename(state_name = state) %>%
  rename(state = statefip)

# Merge 
agg2 <- agg2 %>%
  left_join(state_nca_laws %>% select(-state_name), by = "state")

rm(state_nca_laws)
gc()


# --- Exclude Hourly Ban State(s) --- 
# NOTE: The below that is robust to more states appearing as treated when our 
# treatment panel expands.
states_hourly <- agg2 %>%
  mutate(
    treated_eff_hourly = !is.na(eff_hourly_year) & year >= eff_hourly_year
  ) %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    ever_hourly = any(treated_eff_hourly, na.rm = TRUE)
  ) %>%
  filter(ever_hourly) %>%
  pull(var = state, name = state_name)

# Postings to drop 
postings_drop_hourly <- sum(agg2$total_postings[agg2$state %in% states_hourly], 
                            na.rm = TRUE)
save_stat(postings_drop_hourly, "postings_drop_hourly")

# Drop 
agg2 <- agg2 %>%
  filter(
    !(state %in% states_hourly)
  )
rm(states_hourly)
gc()


# --- Exclude "Other" Ban State(s) --- 
states_other <- agg2 %>%
  mutate(
    treated_eff_other = !is.na(eff_other_year) & year >= eff_other_year
  ) %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    ever_other = any(treated_eff_other, na.rm = TRUE)
  ) %>%
  filter(ever_other) %>%
  pull(var = state, name = state_name)

# Postings to drop 
postings_drop_other <- sum(agg2$total_postings[agg2$state %in% states_other], 
                           na.rm = TRUE)
save_stat(postings_drop_other, "postings_drop_other")

# Drop 
agg2 <- agg2 %>%
  filter(
    !(state %in% states_other)
  )
rm(states_other)
gc()


# --- Industry or Occupation-Based Bans --- 
# A. Lawyers 
# NOTE: NCAs are universally banned for lawyers. Lawyer's SOC code: 23-1010.

# Postings to drop
postings_drop_lawyers <- sum(agg2$total_postings[agg2$soc_4 == "23-1010"],
                             na.rm = TRUE)
save_stat(postings_drop_lawyers, "postings_drop_lawyers")

# Drop
agg2 <- agg2 %>%
  filter(soc_4 != "23-1010")


#B. Industry Bans 
# NOTE: Excluding listings from the ind/occ affected by the ban in the state 
# with the ban. 

# 1. SOC-based restrictions --- 
states_ind <- agg2 %>%
  mutate(
    treated_eff_ind = !is.na(eff_ind_year) & year >= eff_ind_year
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

postings_before_ind_soc <- sum(agg2$total_postings, na.rm = TRUE)

agg2 <- agg2 %>%
  anti_join(
    states_ind_soc,
    by = c("state", "soc_4")
  )

# Postings dropped
postings_drop_ind_soc <- postings_before_ind_soc - sum(agg2$total_postings,
                                                       na.rm = TRUE)
save_stat(postings_drop_ind_soc, "postings_drop_ind_soc")

# Drop some saved stats 
rm(list = ls(pattern = "^(n_|postings_)"))
gc()


# 2. NAICS-based restrictions --- 
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

postings_before_ind_naics <- sum(agg2$total_postings, na.rm = TRUE)

agg2 <- agg2 %>%
  anti_join(
    states_ind_naics,
    by = c("state", "naics4")
  )

# Postings to drop
postings_drop_ind_naics <- postings_before_ind_naics - sum(agg2$total_postings,
                                                           na.rm = TRUE)
save_stat(postings_drop_ind_naics, "postings_drop_ind_naics")

rm(list = ls(pattern = "^(states_|n_|postings_)"))
gc()


# C. Health Bans ---
# NOTE: All of the health ban restrictions are based on SOC codes, not NAICS. 

# Filter: health1_ban --- 
state_health1 <- agg2 %>%
  mutate(
    treated_eff_health1 = !is.na(eff_health1_year) & year >= eff_health1_year
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

postings_before_health1 <- sum(agg2$total_postings, na.rm = TRUE)

agg2 <- agg2 %>%
  anti_join(
    state_health1_soc,
    by = c("state", "soc_4")
  ) 

# Postings dropped 
postings_drop_health1 <- postings_before_health1 - sum(agg2$total_postings,
                                                       na.rm = TRUE)
save_stat(postings_drop_health1, "postings_drop_health1")

rm(state_health1_soc, state_health1_expansion, state_health1)
gc()


# Filter: health2_ban ---
state_health2 <- agg2 %>%
  mutate(
    treated_eff_health2 = !is.na(eff_health2_year) & year >= eff_health2_year
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

postings_before_health2 <- sum(agg2$total_postings, na.rm = TRUE)

agg2 <- agg2 %>%
  anti_join(
    state_health2_soc,
    by = c("state", "soc_4")
  ) 

# Postings dropped 
postings_drop_health2 <- postings_before_health2 - sum(agg2$total_postings,
                                                       na.rm = TRUE)
save_stat(postings_drop_health2, "postings_drop_health2")

rm(state_health2_soc, state_health2_expansion, state_health2)
gc()


# Filter: health3_ban ---
state_health3 <- agg2 %>%
  mutate(
    treated_eff_health3 = !is.na(eff_health3_year) & year >= eff_health3_year
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

postings_before_health3 <- sum(agg2$total_postings, na.rm = TRUE)

agg2 <- agg2 %>%
  anti_join(
    state_health3_soc,
    by = c("state", "soc_4")
  ) 

# Postings dropped 
postings_drop_health3 <- postings_before_health3 - sum(agg2$total_postings,
                                                       na.rm = TRUE)
save_stat(postings_drop_health3, "postings_drop_health3")

rm(list = ls(pattern = "^(state_|n_|postings_|soc_|naics_)"))
gc()


# --- Full ban states (CA, MN, ND, OK) --- 
states_full <- agg2 %>%
  mutate(
    treated_eff_full = !is.na(eff_full_year) & year >= eff_full_year
  ) %>%
  group_by(state) %>%
  summarise(
    state_name = first(state_name),
    ever_full = any(treated_eff_full, na.rm = TRUE)
  ) %>%
  filter(ever_full) %>%
  pull(var = state, name = state_name)

n_before_full <- nrow(agg2)
postings_before_full <- sum(agg2$total_postings)

agg2 <- agg2 %>%
  filter(!state %in% states_full)

# Postings dropped 
postings_drop_full <- postings_before_full - sum(agg2$total_postings)
save_stat(postings_drop_full, "postings_drop_full")

rm(list = ls(pattern = "^n_|postings_|states_"))
gc()


# --- Clean-up unneeded variables --- 
agg2 <- agg2 %>%
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
gc()


# --------------------- AGGREGATE TO SOC-STATE-YEAR LEVEL ----------------------
# NOTE: Create panel ID, treatment indicator, and cohort variable when aggregating. 
agg2 <- agg2 %>% # overwriting agg2 to save memory in cluster run
  group_by(soc_4, state, year) %>%
  summarise(
    soc_4_name = first(soc_4_name),
    state_name = first(state_name), 

    any_educ_soc = sum(any_educ, na.rm = TRUE),
    bachelor_soc = sum(bachelor, na.rm = TRUE),
    master_soc = sum(master, na.rm = TRUE),
    doctorate_soc = sum(doctorate, na.rm = TRUE),
    
    any_exp_soc = sum(any_exp, na.rm = TRUE),
    
    ave_exp_num = sum(ave_exp * total_postings, na.rm = TRUE),
    ave_exp_den = sum(total_postings[!is.na(ave_exp)], na.rm = TRUE),
    ave_exp_soc = na_if(ave_exp_num / ave_exp_den, NaN),
    
    fulltime_soc = sum(fulltime, na.rm = TRUE),
    parttime_soc = sum(parttime, na.rm = TRUE),
    flextime_soc = sum(flextime, na.rm = TRUE),
    
    total_postings_soc = sum(total_postings, na.rm = TRUE),
    
    eff_inc1_year = first(eff_inc1_year),
    
    .groups = "drop"
  ) %>% 
  relocate(soc_4_name, .after = soc_4) %>%
  relocate(state_name, .after = state)


# --- Create panel ID, treatment indicator, and cohort variable --- 
agg2 <- agg2 %>%
  mutate(
    panel_id = as.integer(interaction(soc_4, state, drop = TRUE)),
    treated = as.integer(!is.na(eff_inc1_year) & year >= eff_inc1_year), 
    cohort = ifelse(is.na(eff_inc1_year), 0, eff_inc1_year)
  )


# --- Create share variables ---  
agg2 <- agg2 %>%
  mutate(
    share_any_educ = any_educ_soc / total_postings_soc, 
    share_bachelor = bachelor_soc / total_postings_soc,
    share_master = master_soc / total_postings_soc, 
    share_doctorate = doctorate_soc / total_postings_soc, 
    
    share_exp = any_exp_soc / total_postings_soc,
    
    share_fulltime = fulltime_soc / total_postings_soc,
    share_parttime = parttime_soc / total_postings_soc,
    share_flextime = flextime_soc / total_postings_soc
  )

gc()

# ---------------------------- MERGE COVARIATES --------------------------------
covariates <- read_csv("data/clean-data/covariates_a_clean.csv")

agg2 <- agg2 %>%
  left_join(
    covariates,
    by = c("state", "year")
  ) %>%
  select(
    -state_name.y
  ) %>%
  rename(
    state_name = state_name.x
  )

rm(covariates)
gc()

# ----------------- SAVE FIRM-OCC-STATE-YEAR AGGREGATION -----------------------
write_csv(agg2, "data/analysis-data/agg2_sab_analysis.csv") # sab = soc all ban


# NOTE: Income income thresholds should be converted by hand, as they don't vary
# with the date variable in the panel. But the inc_threshold_2024 will match
# the real variables since the base year is 2024. 

# NOTE: (1) Decide whether to balance panel or restrict to SOC-State-Year cells 
# with at least 10 ads later. (2) Can use panelview to see how sparce the cells 
# are. 


  







