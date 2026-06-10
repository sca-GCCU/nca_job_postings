##############################################################################
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
#setwd("/home/scanast/nca_job_postings") # Cluster

#install.packages("Synth")
#install.packages("SCtools")

library(dplyr)
library(ggplot2)
library(readr)
library(Synth)
library(SCtools)


# ---- Load analysis data ----  
agg1_mn <- read_csv("data/analysis-data/agg1_mn_analysis.csv")

# I NEED:
# - numeric predictors 
# - numeric time variable (date version won't work)* 
# - unit variable (statefip), but note that I need to collapse to state-date panel*
# - name variable to go along with unit variable 


# ---- Prep data for use with Synth ---- 
# NOTE: Time variable is currently "year" not "date" because I'm worried about 
# matching on noise. 

# Collapsing to state-year panel
agg1_state <- agg1_mn %>%
  group_by(state, year) %>%
  summarise(
    state_name = first(state_name, na_rm = TRUE),
    date_enact_full = first(date_enact_full, na_rm = TRUE),
    date_eff_full = first(date_eff_full, na_rm = TRUE),
    total_postings_state = sum(total_postings, na.rm = TRUE),
    any_exp_postings_state = sum(any_exp, na.rm = TRUE),
    any_educ_share_state = weighted.mean(any_educ_share, w = total_postings, na.rm = TRUE),
    bachelor_share_state = weighted.mean(bachelor_share, w = total_postings, na.rm = TRUE),
    master_share_state = weighted.mean(master_share, w = total_postings, na.rm = TRUE),
    doctorate_share_state = weighted.mean(doctorate_share, w = total_postings, na.rm = TRUE),
    any_exp_share_state = weighted.mean(any_exp_share, w = total_postings, na.rm = TRUE), 
    ave_exp_state = weighted.mean(ave_exp, w = total_postings, na.rm = TRUE),
    fulltime_share_state = weighted.mean(fulltime_share, w = total_postings, na.rm = TRUE),
    internship_share_state = weighted.mean(internship_share, w = total_postings, na.rm = TRUE),
    unemp_rate = first(unemp_rate, na_rm = TRUE), # OK to do this because currently using annual state-level controls 
    frac_male = first(frac_male, na_rm = TRUE),
    frac_black = first(frac_black, na_rm = TRUE),
    frac_college = first(frac_college, na_rm = TRUE),
    mean_age = first(mean_age, na_rm = TRUE),
    real_income = first(real_income, na_rm = TRUE),
    real_hpi = first(real_hpi, na_rm = TRUE),
    real_ave_salary = first(real_ave_salary, na_rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(year != 2025)

agg1_state <- agg1_state %>%
  mutate(
    state = as.numeric(state),
    year = as.numeric(year)
  ) %>%
  as.data.frame() # this step was key for getting dataprep to recognize numeric variables 

# Creating vector of control state fips 
state_fip_vec <- agg1_state %>% 
  group_by(state_name) %>%
  summarise(
    state_fip = first(state, na_rm = TRUE),
    .groups = "drop"
  ) %>%
  pull(state_fip)

state_fip_vec_new <- state_fip_vec[state_fip_vec != 27]


# ----------------------- Synth Control - Spec 1 -------------------------------
# ---- Total Postings ---- 

# Feeding to dataprep  
dataprep_out_tot_post <- dataprep(
  foo = agg1_state, # Dataset to be prepped
  
  # Predictor variables to be used
  predictors = c("unemp_rate", "frac_male", "frac_black", "frac_college", 
                 "mean_age", "real_income", "real_hpi", "real_ave_salary"),
  
  predictors.op = "mean", # operation to apply to predictors 
  
  time.predictors.prior = 2010:2022, # pre-intervention time period 
  
  dependent = "total_postings_state",
  
  unit.variable = "state",
  
  unit.names.variable = "state_name",
  
  time.variable = "year",
  
  treatment.identifier = 27, # Minnesota state FIP
  
  controls.identifier = state_fip_vec_new,
  
  time.optimize.ssr = 2010:2022,
  
  time.plot = 2010:2024
)

synth_out <- synth(data.prep.obj = dataprep_out_tot_post)

path.plot(synth_out, dataprep_out_tot_post)


# ---- Listings with Any Experience Requirement ----

# Feeding to dataprep  
dataprep_out_any_exp <- dataprep(
  foo = agg1_state, # Dataset to be prepped
  
  # Predictor variables to be used
  predictors = c("unemp_rate", "frac_male", "frac_black", "frac_college", 
                 "mean_age", "real_income", "real_hpi", "real_ave_salary"),
  
  predictors.op = "mean", # operation to apply to predictors 
  
  time.predictors.prior = 2010:2022, # pre-intervention time period 
  
  dependent = "any_exp_postings_state",
  
  unit.variable = "state",
  
  unit.names.variable = "state_name",
  
  time.variable = "year",
  
  treatment.identifier = 27, # Minnesota state FIP
  
  controls.identifier = state_fip_vec_new,
  
  time.optimize.ssr = 2010:2022,
  
  time.plot = 2010:2024
)

synth_out <- synth(data.prep.obj = dataprep_out_any_exp)

path.plot(synth_out, dataprep_out_any_exp)


# ---- Any Experience Share ----

# Feeding to dataprep  
dataprep_out_any_exp <- dataprep(
  foo = agg1_state, # Dataset to be prepped
  
  # Predictor variables to be used
  predictors = c("unemp_rate", "frac_male", "frac_black", "frac_college", 
                 "mean_age", "real_income", "real_hpi", "real_ave_salary"),
  
  predictors.op = "mean", # operation to apply to predictors 
  
  time.predictors.prior = 2010:2022, # pre-intervention time period 
  
  dependent = "any_exp_share_state",
  
  unit.variable = "state",
  
  unit.names.variable = "state_name",
  
  time.variable = "year",
  
  treatment.identifier = 27, # Minnesota state FIP
  
  controls.identifier = state_fip_vec_new,
  
  time.optimize.ssr = 2010:2022,
  
  time.plot = 2010:2024
)

synth_out <- synth(data.prep.obj = dataprep_out_any_exp)

path.plot(synth_out, dataprep_out_any_exp)


# ---- Average Experience Requirement ----

# Feeding to dataprep  
dataprep_out_any_exp <- dataprep(
  foo = agg1_state, # Dataset to be prepped
  
  # Predictor variables to be used
  predictors = c("unemp_rate", "frac_male", "frac_black", "frac_college", 
                 "mean_age", "real_income", "real_hpi", "real_ave_salary"),
  
  predictors.op = "mean", # operation to apply to predictors 
  
  time.predictors.prior = 2010:2022, # pre-intervention time period 
  
  dependent = "ave_exp_state",
  
  unit.variable = "state",
  
  unit.names.variable = "state_name",
  
  time.variable = "year",
  
  treatment.identifier = 27, # Minnesota state FIP
  
  controls.identifier = state_fip_vec_new,
  
  time.optimize.ssr = 2010:2022,
  
  time.plot = 2010:2024
)

synth_out <- synth(data.prep.obj = dataprep_out_any_exp)

path.plot(synth_out, dataprep_out_any_exp)


# ------------------------- Synth Control - Spec 2 -----------------------------
# ---- Total Postings ---- 

# Feeding to dataprep  
dataprep_out_tot_post_2 <- dataprep(
  foo = agg1_state, # Dataset to be prepped
  
  predictors = NULL,
  time.predictors.prior = 2010:2022, # pre-intervention time period 
  
  special.predictors = list(
    list("total_postings_state", 2010, "mean"),
    list("total_postings_state", 2011, "mean"),
    list("total_postings_state", 2012, "mean"),
    list("total_postings_state", 2013, "mean"),
    list("total_postings_state", 2014, "mean"),
    list("total_postings_state", 2015, "mean"),
    list("total_postings_state", 2016, "mean"),
    list("total_postings_state", 2017, "mean"),
    list("total_postings_state", 2018, "mean"),
    list("total_postings_state", 2019, "mean"),
    list("total_postings_state", 2020, "mean"),
    list("total_postings_state", 2021, "mean"),
    list("total_postings_state", 2022, "mean")
  ),
  
  dependent = "total_postings_state",
  
  unit.variable = "state",
  
  unit.names.variable = "state_name",
  
  time.variable = "year",
  
  treatment.identifier = 27, # Minnesota state FIP
  
  controls.identifier = state_fip_vec_new,
  
  time.optimize.ssr = 2010:2022,
  
  time.plot = 2010:2024
)

synth_out_tot_post_2 <- synth(data.prep.obj = dataprep_out_tot_post_2)

path.plot(synth_out_tot_post_2, dataprep_out_tot_post_2)



# ---- Listings with Any Experience Requirement ----

dataprep_out_any_exp_post_2 <- dataprep(
  foo = agg1_state, # Dataset to be prepped
  
  predictors = NULL,
  time.predictors.prior = 2010:2022, # pre-intervention time period 
  
  special.predictors = list(
    list("any_exp_postings_state", 2010, "mean"),
    list("any_exp_postings_state", 2011, "mean"),
    list("any_exp_postings_state", 2012, "mean"),
    list("any_exp_postings_state", 2013, "mean"),
    list("any_exp_postings_state", 2014, "mean"),
    list("any_exp_postings_state", 2015, "mean"),
    list("any_exp_postings_state", 2016, "mean"),
    list("any_exp_postings_state", 2017, "mean"),
    list("any_exp_postings_state", 2018, "mean"),
    list("any_exp_postings_state", 2019, "mean"),
    list("any_exp_postings_state", 2020, "mean"),
    list("any_exp_postings_state", 2021, "mean"),
    list("any_exp_postings_state", 2022, "mean")
  ),
  
  dependent = "any_exp_postings_state",
  
  unit.variable = "state",
  
  unit.names.variable = "state_name",
  
  time.variable = "year",
  
  treatment.identifier = 27, # Minnesota state FIP
  
  controls.identifier = state_fip_vec_new,
  
  time.optimize.ssr = 2010:2022,
  
  time.plot = 2010:2024
)

synth_out_any_exp_post_2 <- synth(data.prep.obj = dataprep_out_any_exp_post_2)

path.plot(synth_out_any_exp_post_2, dataprep_out_any_exp_post_2)


# ---- Any Experience Share ----


dataprep_out_any_exp_share_2 <- dataprep(
  foo = agg1_state, # Dataset to be prepped
  
  predictors = NULL,
  time.predictors.prior = 2010:2022, # pre-intervention time period 
  
  special.predictors = list(
    list("any_exp_share_state", 2010, "mean"),
    list("any_exp_share_state", 2011, "mean"),
    list("any_exp_share_state", 2012, "mean"),
    list("any_exp_share_state", 2013, "mean"),
    list("any_exp_share_state", 2014, "mean"),
    list("any_exp_share_state", 2015, "mean"),
    list("any_exp_share_state", 2016, "mean"),
    list("any_exp_share_state", 2017, "mean"),
    list("any_exp_share_state", 2018, "mean"),
    list("any_exp_share_state", 2019, "mean"),
    list("any_exp_share_state", 2020, "mean"),
    list("any_exp_share_state", 2021, "mean"),
    list("any_exp_share_state", 2022, "mean")
  ),
  
  dependent = "any_exp_share_state",
  
  unit.variable = "state",
  
  unit.names.variable = "state_name",
  
  time.variable = "year",
  
  treatment.identifier = 27, # Minnesota state FIP
  
  controls.identifier = state_fip_vec_new,
  
  time.optimize.ssr = 2010:2022,
  
  time.plot = 2010:2024
)

synth_out_any_exp_share_2 <- synth(data.prep.obj = dataprep_out_any_exp_share_2)

path.plot(synth_out_any_exp_share_2, dataprep_out_any_exp_share_2)



# ---- Average Experience Requirement ----

dataprep_out_ave_exp_2 <- dataprep(
  foo = agg1_state, # Dataset to be prepped
  
  predictors = NULL,
  time.predictors.prior = 2010:2022, # pre-intervention time period 
  
  special.predictors = list(
    list("ave_exp_state", 2010, "mean"),
    list("ave_exp_state", 2011, "mean"),
    list("ave_exp_state", 2012, "mean"),
    list("ave_exp_state", 2013, "mean"),
    list("ave_exp_state", 2014, "mean"),
    list("ave_exp_state", 2015, "mean"),
    list("ave_exp_state", 2016, "mean"),
    list("ave_exp_state", 2017, "mean"),
    list("ave_exp_state", 2018, "mean"),
    list("ave_exp_state", 2019, "mean"),
    list("ave_exp_state", 2020, "mean"),
    list("ave_exp_state", 2021, "mean"),
    list("ave_exp_state", 2022, "mean")
  ),
  
  dependent = "ave_exp_state",
  
  unit.variable = "state",
  
  unit.names.variable = "state_name",
  
  time.variable = "year",
  
  treatment.identifier = 27, # Minnesota state FIP
  
  controls.identifier = state_fip_vec_new,
  
  time.optimize.ssr = 2010:2022,
  
  time.plot = 2010:2024
)

synth_out_ave_exp_2 <- synth(data.prep.obj = dataprep_out_ave_exp_2)

path.plot(synth_out_ave_exp_2, dataprep_out_ave_exp_2)


# NOTE: Still need to run all of this in the Cluster before making judgements.
