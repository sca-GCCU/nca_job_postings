##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "8a_sdid_mn.R" 
# by: Sebastian C. Anastasi
# Date of this version: May 25, 2026
#
# Description:  
#
# Dependencies:  
#
# Output:
##############################################################################

rm(list = ls())

#install.packages("rlang")
#install.packages("devtools")
#devtools::install_github("synth-inference/synthdid")

library(synthdid)
library(ggplot2)
library(data.table)
library(readr)
library(tidyr)
library(dplyr)

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

agg2_mn <- read_csv("data/analysis-data/agg2_mn_analysis.csv")

# ------------------------ PREP DATA -------------------------------------------

# NOTE: I need a balanced panel with simultaneous treatment timing, so I need
# to collapse to the state level. I also need to create Year-Month timesteps
# using as.Date. Finally, I need a treatment indicator.

# --- Create Year-Month Timesteps Using as.Date --- 
agg2_mn <- agg2_mn %>%
  mutate(
    date = as.Date(sprintf('%04d-%02d-%02d', year, month, 1))
  )
# NOTE: This is pretty much exactly how date was already formatted, but let's 
# try making double sure to follow the instructions on the first try.

# --- Convert Treatment Indicator to 1/0 --- 
#agg2_mn <- agg2_mn %>%
#  mutate(
#    treated_eff_full = as.numeric(treated_eff_full)
#  )
# NOTE: I actually believe that it expects the treatment indicator to be logical.

# --- Summarize to State-Level Panel --- 

agg2_mn <- agg2_mn %>%
  filter(year < 2025) %>%
  group_by(state_name, date) %>%
  summarise(
    total_postings_state = sum(total_postings, na.rm = TRUE),
    any_exp_state = sum(any_exp, na.rm = TRUE),
    ave_exp_state = weighted.mean(ave_exp, w = total_postings, na.rm = TRUE),
    share_exp_state = sum(any_exp, na.rm = TRUE)/sum(total_postings, na.rm = TRUE),
    fulltime_state = sum(fulltime, na.rm = TRUE),
    share_fulltime_state = sum(fulltime, na.rm = TRUE)/sum(total_postings, na.rm = TRUE),
    parttime_state = sum(parttime, na.rm = TRUE),
    share_parttime_state = sum(parttime, na.rm = TRUE)/sum(total_postings, na.rm = TRUE),
    treated_eff_full = first(treated_eff_full),
    .groups = "drop"
  ) 


# ----------------------------- TOTAL POSTINGS ---------------------------------

# --- Prep Mini-Panel --- 

agg2_mn_tot_post <- agg2_mn %>%
  select(
    state_name,
    date,
    total_postings_state,
    treated_eff_full
  )
agg2_mn_tot_post <- as.data.frame(agg2_mn_tot_post)

# Checking for states without listings in the sample (they exist)
print(n=37, agg2_mn_tot_post %>%
  group_by(state_name) %>%
  summarise(n_periods = n_distinct(date)) %>%
  arrange(n_periods)
)
bad_states <- c("Wyoming", "Montana", "South Dakota", "Alaska", "Hawaii", "Idaho", 
                "Kansas", "Kentucky", "Vermont")

# Filtering out missing states to create balance for test run
# NOTE: Will need to add them back in once we switch to the full sample.
agg2_mn_tot_post <- agg2_mn_tot_post %>%
  filter(!state_name %in% bad_states)

setup_tot_post = panel.matrices(agg2_mn_tot_post)
# NOTE: Since I have things ordered properly, I technically don't have to 
# specify each argument. 

estimate_tot_post = synthdid_estimate(setup_tot_post$Y, setup_tot_post$N0,
                                      setup_tot_post$T0)

plot(estimate_tot_post)


