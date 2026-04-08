# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "7a_scm_agg1_mn.R" 
# by: Sebastian C. Anastasi
# Date of this version: April 8, 2026
#
# Description: Generates SCM plots and estimates. 
#
# Dependencies: 
#
# Output: 
##############################################################################

# NOTE: When run in the cluster, I will need to update paths and (I believe)
# install the appropriate packages. 

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

#install.packages("Synth")
#install.packages("SCtools")

library(tidyverse)
library(Synth)
library(SCtools)

# Load analysis data 
agg1 <- read_csv("data/analysis-data/agg1_mn_analysis.csv")

# I NEED:
# - numeric predictors 
# - numeric time variable (date version won't work)* 
# - unit variable (statefip), but note that I need to collapse to state-date panel*
# - name variable to go along with unit variable 

outcome_var <- c("total_postings", "any_educ_share", "bachelor_share", "master_share", 
                 "doctorate_share", "any_exp_share", "ave_exp", "fulltime_share",
                 "internship_share")

# Prep data for use with Synth 

agg1_state <- agg1 %>%
  group_by(state, date) %>%
  summarise(
    state_name = first(state_name, na_rm = TRUE),
    date_enact_full = first(date_enact_full, na_rm = TRUE),
    date_eff_full = first(date_eff_full, na_rm = TRUE),
    total_postings = sum(total_postings, na.rm = TRUE),
    any_educ_share = weighted.mean(any_educ_share, w = total_postings, na.rm = TRUE),
    bachelor_share = weighted.mean(bachelor_share, w = total_postings, na.rm = TRUE),
    master_share = weighted.mean(master_share, w = total_postings, na.rm = TRUE),
    doctorate_share = weighted.mean(doctorate_share, w = total_postings, na.rm = TRUE),
    any_exp_share = weighted.mean(any_exp_share, w = total_postings, na.rm = TRUE), 
    ave_exp = weighted.mean(ave_exp, w = total_postings, na.rm = TRUE),
    fulltime_share = weighted.mean(fulltime_share, w = total_postings, na.rm = TRUE),
    internship_share = weighted.mean(internship_share, w = total_postings, na.rm = TRUE),
    unemp_rate = first(unemp_rate, na_rm = TRUE),
    frac_male = first(frac_male, na_rm = TRUE),
    frac_black = first(frac_black, na_rm = TRUE),
    frac_college = first(frac_college, na_rm = TRUE),
    mean_age = first(mean_age, na_rm = TRUE),
    real_income = first(real_income, na_rm = TRUE),
    real_hpi = first(real_hpi, na_rm = TRUE),
    real_ave_salary = first(real_ave_salary, na_rm = TRUE)
  )
  





