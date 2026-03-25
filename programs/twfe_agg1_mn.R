##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "twfe_agg1_mn.R" 
# by: Sebastian C. Anastasi
# Date of this version: March 25, 2026
#
# Description: Generates TWFE estimates and tables, as well as corresponding 
# Event Study plots. 
#
# Dependencies: 
#
# Output: 
##############################################################################

# NOTE: When run in the cluster, I will need to update paths and (I believe)
# install the appropriate packages. 

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

library(fixest)
library(tidyverse)

outcome_var <- c("any_educ_share", "bachelor_share", "master_share", 
                 "doctorate_share", "any_exp_share", "ave_exp", "fulltime_share",
                 "internship_share")



# 1. Load the data and prep the data

agg1_mn_analysis <- read_csv("data/analysis-data/agg1_mn_analysis.csv")

# Transform treatment indicator to numeric for tables

agg1_mn_analysis <- agg1_mn_analysis %>%
  mutate(
    treated_eff_full = as.numeric(treated_eff_full),
    treated_enact_full = as.numeric(treated_enact_full)
  )

# Create event time indicator for event studies 
# NOTE: REVISIT. MAY JUST USE THE ACTUAL TIME PERIODS... OR RELABEL/RESCALE YEARS?

agg1_mn_analysis <- agg1_mn_analysis %>%
  mutate(
    event_time = if_else()
  )



# 2. TWFE estimation

# --- A. Any Education Share ---

# --- Table ---  

twfe_any_educ_share <- feols(any_educ_share ~ )


















