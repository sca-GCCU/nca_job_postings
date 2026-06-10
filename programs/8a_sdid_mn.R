##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "8a_sdid_mn.R" 
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

#install.packages("rlang")
#install.packages("devtools")
#devtools::install_github("synth-inference/synthdid")

library(synthdid)
library(ggplot2)
library(data.table)
library(readr)
library(tidyr)
library(dplyr)
library(zoo)

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")
#setwd("/home/scanast/nca_job_postings") # Cluster

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

# --- Create Balanced Panel --- 
# Checking for states without listings in the sample (they exist)
print(n=37, agg2_mn_tot_post %>%
  group_by(state_name) %>%
  summarise(n_periods = n_distinct(date)) %>%
  arrange(n_periods)
)
bad_states_tot_post <- c("Wyoming", "Montana", "South Dakota", "Alaska",   
                         "Hawaii", "Idaho", "Kansas", "Kentucky", "Vermont")

# Filtering out missing states to create balance for test run
# NOTE: Will need to add them back in once we switch to the full sample.
agg2_mn_tot_post <- agg2_mn_tot_post %>%
  filter(!state_name %in% bad_states)

# --- SDID Run --- 
# Create matrices 
setup_tot_post = panel.matrices(agg2_mn_tot_post)
# NOTE: Since I have things ordered properly, I technically don't have to 
# specify each argument. 

# Estimate SDID 
estimate_tot_post = synthdid_estimate(setup_tot_post$Y, setup_tot_post$N0,
                                      setup_tot_post$T0)

# Plot SDID 
sdid_plot_tot_post <- plot(estimate_tot_post, se.method = 'placebo')

# Control Unit Contribution Plot 
synthdid_units_plot(estimate_tot_post, se.method = 'placebo')

rm(agg2_mn_tot_post)

# NOTE: Eventually calulcate and produce the tables of results. 


# ----------------------- ANY EXP LISTINGS -------------------------------------
# --- Prep Mini-Panel --- 
agg2_mn_any_exp <- agg2_mn %>%
  select(
    state_name,
    date,
    any_exp_state,
    treated_eff_full
  )
agg2_mn_any_exp <- as.data.frame(agg2_mn_any_exp)

# --- Create Balanced Panel --- 
# Checking for "bad" states 
print(
  n=37, agg2_mn_any_exp %>%
    group_by(state_name) %>%
    summarise(n_periods = n_distinct(date)) %>%
    arrange(n_periods)
)
# NOTE: Turns out these are the same "bad states" as with total postings. 
bad_states_any_exp <- c("Wyoming", "Montana", "South Dakota", "Alaska",   
                        "Hawaii", "Idaho", "Kansas", "Kentucky", "Vermont")

# Filtering out "bad" states 
agg2_mn_any_exp <- agg2_mn_any_exp %>%
  filter(!state_name %in% bad_states_any_exp)

# --- SDID Run --- 
# Create matrices 
setup_any_exp = panel.matrices(agg2_mn_any_exp)

# Estimate SDID 
estimate_any_exp = synthdid_estimate(setup_any_exp$Y, setup_any_exp$N0,
                                     setup_any_exp$T0)

# Plot SDID 
sdid_plot_any_exp = plot(estimate_any_exp, se.method = 'placebo')
sdid_plot_any_exp

# Control Unit Contribution Plot 
synthdid_units_plot(estimate_any_exp, se.method = 'placebo')

rm(agg2_mn_any_exp)

# ----------------------- ANY EXP SHARE ----------------------------------------
# --- Prep Mini-Panel --- 
agg2_mn_share_exp <- agg2_mn %>%
  select(
    state_name,
    date,
    share_exp_state,
    treated_eff_full
  )
agg2_mn_share_exp <- as.data.frame(agg2_mn_share_exp)

# --- Create Balanced Panel --- 
# Checking for "bad" states 
print(
  n=37, agg2_mn_share_exp %>%
    group_by(state_name) %>%
    summarise(n_periods = n_distinct(date)) %>%
    arrange(n_periods)
)
# NOTE: Turns out these are the same "bad states" as with total postings. 
bad_states_share_exp <- c("Wyoming", "Montana", "South Dakota", "Alaska",   
                        "Hawaii", "Idaho", "Kansas", "Kentucky", "Vermont")

# Filtering out "bad" states 
agg2_mn_share_exp <- agg2_mn_share_exp %>%
  filter(!state_name %in% bad_states_share_exp)

# --- SDID Run --- 
# Create matrices 
setup_share_exp = panel.matrices(agg2_mn_share_exp)

# Estimate SDID 
estimate_share_exp = synthdid_estimate(setup_share_exp$Y, setup_share_exp$N0,
                                     setup_share_exp$T0)

# Plot SDID 
sdid_plot_share_exp = plot(estimate_share_exp, se.method = 'placebo')
sdid_plot_share_exp

# Control Unit Contribution Plot 
synthdid_units_plot(estimate_share_exp, se.method = 'placebo')

rm(agg2_mn_share_exp)

# ----------------------- AVE EXP ----------------------------------------------
# --- Prep Mini-Panel --- 
agg2_mn_ave_exp <- agg2_mn %>%
  select(
    state_name,
    date,
    ave_exp_state,
    treated_eff_full
  )
agg2_mn_ave_exp <- as.data.frame(agg2_mn_ave_exp)

# --- Create Balanced Panel --- 
# Checking for "bad" states 
print(
  n=37, agg2_mn_ave_exp %>%
    group_by(state_name) %>%
    summarise(n_periods = n_distinct(date)) %>%
    arrange(n_periods)
)
# NOTE: Bad states from total postings, plus West Virginia, Arkansas, Delaware,
# and New Mexico have missing values for the share variable. 
bad_states_ave_exp <- c("Wyoming", "Montana", "South Dakota", "Alaska",   
                          "Hawaii", "Idaho", "Kansas", "Kentucky", "Vermont",
                        "Arkansas", "Delaware", "New Mexico", "West Virginia")

# Filtering out "bad" states 
agg2_mn_ave_exp <- agg2_mn_ave_exp %>%
  filter(!state_name %in% bad_states_ave_exp)

# --- SDID Run --- 
# Create matrices 
setup_ave_exp = panel.matrices(agg2_mn_ave_exp)

# Estimate SDID 
estimate_ave_exp = synthdid_estimate(setup_ave_exp$Y, setup_ave_exp$N0,
                                       setup_ave_exp$T0)

# Plot SDID 
sdid_plot_ave_exp = plot(estimate_ave_exp, se.method = 'placebo')
sdid_plot_ave_exp

# Control Unit Contribution Plot 
synthdid_units_plot(estimate_ave_exp, se.method = 'placebo')

rm(agg2_mn_ave_exp)

# ------------------------ FULLTIME LISTINGS -----------------------------------
# --- Prep Mini-Panel --- 
agg2_mn_fulltime <- agg2_mn %>%
  select(
    state_name,
    date,
    fulltime_state,
    treated_eff_full
  )
agg2_mn_fulltime <- as.data.frame(agg2_mn_fulltime)

# --- Create Balanced Panel --- 
# Checking for "bad" states 
print(
  n=37, agg2_mn_fulltime %>%
    group_by(state_name) %>%
    summarise(n_periods = n_distinct(date)) %>%
    arrange(n_periods)
)
# NOTE: Turns out these are the same "bad states" as with total postings. 
bad_states_fulltime <- c("Wyoming", "Montana", "South Dakota", "Alaska",   
                          "Hawaii", "Idaho", "Kansas", "Kentucky", "Vermont")

# Filtering out "bad" states 
agg2_mn_fulltime <- agg2_mn_fulltime %>%
  filter(!state_name %in% bad_states_fulltime)

# --- SDID Run --- 
# Create matrices 
setup_fulltime = panel.matrices(agg2_mn_fulltime)

# Estimate SDID 
estimate_fulltime = synthdid_estimate(setup_fulltime$Y, setup_fulltime$N0,
                                       setup_fulltime$T0)

# Plot SDID 
sdid_plot_fulltime = plot(estimate_fulltime, se.method = 'placebo')
sdid_plot_fulltime

# Control Unit Contribution Plot 
synthdid_units_plot(estimate_fulltime, se.method = 'placebo')

rm(agg2_mn_fulltime)

# Other clean-up
rm(setup_tot_post, setup_any_exp, setup_share_exp, setup_ave_exp, setup_fulltime)


# ------------------------ FULLTIME SHARE --------------------------------------
# --- Prep Mini-Panel --- 
agg2_mn_share_fulltime <- agg2_mn %>%
  select(
    state_name,
    date,
    share_fulltime_state,
    treated_eff_full
  )
agg2_mn_share_fulltime <- as.data.frame(agg2_mn_share_fulltime)

# --- Create Balanced Panel --- 
# Checking for "bad" states 
print(
  n=37, agg2_mn_share_fulltime %>%
    group_by(state_name) %>%
    summarise(n_periods = n_distinct(date)) %>%
    arrange(n_periods)
)
# NOTE: Turns out these are the same "bad states" as with total postings. 
bad_states_share_fulltime <- c("Wyoming", "Montana", "South Dakota", "Alaska",   
                         "Hawaii", "Idaho", "Kansas", "Kentucky", "Vermont")

# Filtering out "bad" states 
agg2_mn_share_fulltime <- agg2_mn_share_fulltime %>%
  filter(!state_name %in% bad_states_share_fulltime)

# --- SDID Run --- 
# Create matrices 
setup_share_fulltime = panel.matrices(agg2_mn_share_fulltime)

# Estimate SDID 
estimate_share_fulltime = synthdid_estimate(setup_share_fulltime$Y, setup_share_fulltime$N0,
                                      setup_share_fulltime$T0)

# Plot SDID 
sdid_plot_share_fulltime = plot(estimate_share_fulltime, se.method = 'placebo')
sdid_plot_share_fulltime

# Control Unit Contribution Plot 
synthdid_units_plot(estimate_share_fulltime, se.method = 'placebo')

rm(agg2_mn_share_fulltime)



# ------------------------ PARTTIME LISTINGS -----------------------------------
# --- Prep Mini-Panel --- 
agg2_mn_parttime <- agg2_mn %>%
  select(
    state_name,
    date,
    parttime_state,
    treated_eff_full
  )
agg2_mn_parttime <- as.data.frame(agg2_mn_parttime)

# --- Create Balanced Panel --- 
# Checking for "bad" states 
print(
  n=37, agg2_mn_parttime %>%
    group_by(state_name) %>%
    summarise(n_periods = n_distinct(date)) %>%
    arrange(n_periods)
)
# NOTE: Turns out these are the same "bad states" as with total postings. 
bad_states_parttime <- c("Wyoming", "Montana", "South Dakota", "Alaska",   
                         "Hawaii", "Idaho", "Kansas", "Kentucky", "Vermont")

# Filtering out "bad" states 
agg2_mn_parttime <- agg2_mn_parttime %>%
  filter(!state_name %in% bad_states_parttime)

# --- SDID Run --- 
# Create matrices 
setup_parttime = panel.matrices(agg2_mn_parttime)

# Estimate SDID 
estimate_parttime = synthdid_estimate(setup_parttime$Y, setup_parttime$N0,
                                      setup_parttime$T0)

# Plot SDID 
sdid_plot_parttime = plot(estimate_parttime, se.method = 'placebo')
sdid_plot_parttime

# Control Unit Contribution Plot 
synthdid_units_plot(estimate_parttime, se.method = 'placebo')

rm(agg2_mn_parttime)


# ------------------------ PARTTIME SHARE --------------------------------------
# --- Prep Mini-Panel --- 
agg2_mn_share_parttime <- agg2_mn %>%
  select(
    state_name,
    date,
    share_parttime_state,
    treated_eff_full
  )
agg2_mn_share_parttime <- as.data.frame(agg2_mn_share_parttime)

# --- Create Balanced Panel --- 
# Checking for "bad" states 
print(
  n=37, agg2_mn_share_parttime %>%
    group_by(state_name) %>%
    summarise(n_periods = n_distinct(date)) %>%
    arrange(n_periods)
)
# NOTE: Turns out these are the same "bad states" as with total postings. 
bad_states_share_parttime <- c("Wyoming", "Montana", "South Dakota", "Alaska",   
                         "Hawaii", "Idaho", "Kansas", "Kentucky", "Vermont")

# Filtering out "bad" states 
agg2_mn_share_parttime <- agg2_mn_share_parttime %>%
  filter(!state_name %in% bad_states_share_parttime)

# --- SDID Run --- 
# Create matrices 
setup_share_parttime = panel.matrices(agg2_mn_share_parttime)

# Estimate SDID 
estimate_share_parttime = synthdid_estimate(setup_share_parttime$Y, setup_share_parttime$N0,
                                      setup_share_parttime$T0)

# Plot SDID 
sdid_plot_share_parttime = plot(estimate_share_parttime, se.method = 'placebo')
sdid_plot_share_parttime

# Control Unit Contribution Plot 
synthdid_units_plot(estimate_share_parttime, se.method = 'placebo')

rm(agg2_mn_share_parttime)




