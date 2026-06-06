##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "5d_descriptive_graphs_all_bans.R" 
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

library(did)        # Callaway & Sant'Anna estimator 
library(panelView)  # Visualize treatment rollout  
library(fixest)     # High-performance fixed-effects regression (TWFE comparison)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)

df <- read_csv("data/analysis-data/agg2_analysis.csv")

# ------------------------ 1. PREP AGGREGATIONS --------------------------------
# --- FIRM-STATE LEVEL AGGREGATION --- 

# Collapse to firm-by-state-by-year panel 
df_firm <- df %>%
  group_by(company, state, year) %>% 
  summarise(
    state_name = first(state_name),
    
    any_educ_firm = sum(any_educ, na.rm = TRUE),
    bachelor_firm = sum(bachelor, na.rm = TRUE),
    master_firm = sum(master, na.rm = TRUE),
    doctorate_firm = sum(doctorate, na.rm = TRUE),
    
    any_exp_firm = sum(any_exp, na.rm = TRUE),
    ave_exp_firm = weighted.mean(ave_exp, total_postings, na.rm = TRUE),
    
    total_postings_firm = sum(total_postings, na.rm = TRUE),
    
    fulltime_firm = sum(fulltime, na.rm = TRUE),
    parttime_firm = sum(parttime, na.rm = TRUE),
    flextime_firm = sum(flextime, na.rm = TRUE),
    
    ban_inc1 = first(ban_inc1),
    ban_inc2 = first(ban_inc2),
    eff_inc1_year = first(eff_inc1_year),
    eff_inc2_year = first(eff_inc2_year),
    inc_threshold1 = first(inc_threshold1),
    inc_threshold2 = first(inc_threshold2),
    inc_threshold_2024 = first(inc_threshold_2024),
    
    .groups = "drop"
  )

# Checking whether some values of ave_exp SHOULD be NA and why they are NA 
# test <- read_csv("data/analysis-data/agg2_analysis.csv")
# 
# test %>%
#   filter(is.na(ave_exp)) %>%
#   nrow()
# 
# test <- test %>%
#   filter(is.na(ave_exp)) %>%
#   select(
#     company, state, year,
#     ave_exp, any_exp
#   )

# Replace NaN values in ave_exp_firm
df_firm <- df_firm %>%
  mutate(
    ave_exp_firm = ifelse(is.nan(ave_exp_firm), NA, ave_exp_firm)
  )

# Create share variables 
df_firm <- df_firm %>%
  mutate(
    share_any_educ = any_educ_firm / total_postings_firm, 
    share_bachelor = bachelor_firm / total_postings_firm,
    share_master = master_firm / total_postings_firm, 
    share_doctorate = doctorate_firm / total_postings_firm, 
    
    share_exp = any_exp_firm / total_postings_firm,
    
    share_fulltime = fulltime_firm / total_postings_firm,
    share_parttime = parttime_firm / total_postings_firm,
    flextime_firm = flextime_firm / total_postings_firm
  )

# Create treatment indicator 
df_firm <- df_firm %>%
  mutate(
    treated = as.integer(!is.na(eff_inc1_year) & year >= eff_inc1_year)
  )

# Create unique unit (company-by-state) identifier 
df_firm <- df_firm %>% 
  mutate(
    company_state_id = as.integer(interaction(company, state, drop = TRUE))
  )

# Create a cohort variable (year must be 0 for never-treated)
df_firm <- df_firm %>%
  mutate(
    cohort = ifelse(is.na(eff_inc1_year), 0, eff_inc1_year)
  )

# Drop 2025 because not full year 
df_firm <- df_firm %>%
  filter(year != 2025)


# --- STATE LEVEL AGGREGATION --- 

# Create a state-level panel for panelview
df_state <- df %>%
  group_by(state, year) %>%
  summarise(
    state_name = first(state_name),
    
    any_educ_state = sum(any_educ, na.rm = TRUE),
    bachelor_state = sum(bachelor, na.rm = TRUE),
    master_state = sum(master, na.rm = TRUE),
    doctorate_state = sum(doctorate, na.rm = TRUE),
    
    any_exp_state = sum(any_exp, na.rm = TRUE),
    ave_exp_state = weighted.mean(ave_exp, total_postings, na.rm = TRUE),
    
    total_postings_state = sum(total_postings, na.rm = TRUE),
    
    fulltime_state = sum(fulltime, na.rm = TRUE),
    parttime_state = sum(parttime, na.rm = TRUE),
    flextime_state = sum(flextime, na.rm = TRUE),
    
    ban_inc1 = first(ban_inc1),
    ban_inc2 = first(ban_inc2),
    eff_inc1_year = first(eff_inc1_year),
    eff_inc2_year = first(eff_inc2_year),
    inc_threshold1 = first(inc_threshold1),
    inc_threshold2 = first(inc_threshold2),
    inc_threshold_2024 = first(inc_threshold_2024),
    
    .groups = "drop"
  )

# Replace NaN values in ave_exp_firm
df_state <- df_state %>%
  mutate(
    ave_exp_state = ifelse(is.nan(ave_exp_state), NA, ave_exp_state)
  )

# Create share variables 
df_state <- df_state %>%
  mutate(
    share_any_educ = any_educ_state / total_postings_state, 
    share_bachelor = bachelor_state / total_postings_state,
    share_master = master_state / total_postings_state, 
    share_doctorate = doctorate_state / total_postings_state, 
    
    share_exp = any_exp_state / total_postings_state,
    
    share_fulltime = fulltime_state / total_postings_state,
    share_parttime = parttime_state / total_postings_state,
    flextime_state = flextime_state / total_postings_state
  )

# Create treatment indicator 
df_state <- df_state %>%
  mutate(
    treated = as.integer(!is.na(eff_inc1_year) & year >= eff_inc1_year)
  )

df_state <- df_state %>%
  mutate(
    cohort = ifelse(is.na(eff_inc1_year), 0, eff_inc1_year)
  )

# Drop 2025 because not a full year 
df_state <- df_state %>%
  filter(year != 2025)

# ------------------------- 2. VISUALIZE ROLLOUT -------------------------------

# NOTE: Only going to worry about year for the time being.

panelview(
  total_postings_state ~ treated,
  data = df_state,
  index = c("state_name", "year"),
  pre.post = TRUE,
  main = "Treatment Rollout",
  xlab = "Year", ylab = "State"
)

cohort_summary <- df_firm %>%
  distinct(company_state_id, cohort) %>%
  count(cohort) %>%
  mutate(
    cohort_label = case_when(
      cohort == 0 ~ "Never Treated",
      TRUE ~ paste0("Treated in ", cohort)
    )
  )

print(cohort_summary)

# NOTE: Eventually construct histogram of this cohort_summary.


# ----------------------- 3. PLOT RAW MEANS ------------------------------------

# Total Postings
panelview(total_postings_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Total Postings")

panelview(total_postings_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Total Postings",
          by.cohort = TRUE)

# Any Experienced Required
panelview(any_exp_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Experience Required")

panelview(any_exp_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Experience Required",
          by.cohort = TRUE)

# Average Experience Required 
panelview(ave_exp_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Average Experience Required")

panelview(ave_exp_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Average Experience Required",
          by.cohort = TRUE)

# Share Experience Required 
panelview(share_exp ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Experience Required (Share)")

panelview(share_exp ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Experience Required (Share)",
          by.cohort = TRUE)

# Bachelor's 
panelview(bachelor_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Bachelor's Required")

panelview(bachelor_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Bachelor's Required",
          by.cohort = TRUE)

# Share Bachelor's
panelview(share_bachelor ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Bachelor's Required (Share)")

panelview(share_bachelor ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Bachelor's Required (Share)",
          by.cohort = TRUE)


