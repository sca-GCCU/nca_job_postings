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

# Collapse to firm-by-state 

# NOTE: ADD SHARE VARIABLES.

df <- df %>%
  group_by(company, state, year) %>% 
  summarise(
    any_educ = sum(any_educ, na.rm = TRUE),
    bachelor = sum(bachelor, na.rm = TRUE),
    master = sum(master, na.rm = TRUE),
    doctorate = sum(doctorate, na.rm = TRUE),
    
    any_exp = sum(any_exp, na.rm = TRUE),
    ave_exp = weighted.mean(ave_exp, total_postings, na.rm = TRUE),
    
    total_postings = sum(total_postings, na.rm = TRUE),
    
    fulltime = sum(fulltime, na.rm = TRUE),
    parttime = sum(parttime, na.rm = TRUE),
    flextime = sum(flextime, na.rm = TRUE),
    
    ban_inc1 = first(ban_inc1),
    ban_inc2 = first(ban_inc2),
    eff_inc1_year = first(eff_inc1_year),
    eff_inc2_year = first(eff_inc2_year),
    inc_threshold1 = first(inc_threshold1),
    inc_threshold2 = first(inc_threshold2),
    inc_threshold_2024 = first(inc_threshold_2024),
    .groups = "drop"
  )

# Create treatment indicator 
df <- df %>%
  mutate(
    treated = as.integer(!is.na(eff_inc1_year) & year >= eff_inc1_year)
  )

# NOTE: Only going to worry about year for the time being.

panelview(
  total_postings ~ treated,
  data = df,
  index = c("company", "year"),
  pre.post = TRUE,
  main = "Treatment Rollout",
  xlab = "Year", ylab = "Firm"
)


