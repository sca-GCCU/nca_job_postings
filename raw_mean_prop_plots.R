# PREAMBLE ---------------------------------------------------------------------

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/data")

library(tidyverse)
library(haven)
library(panelView)
library(srvyr)

# LOAD DATA --------------------------------------------------------------------

# Main analysis
acs <- read_dta("nca_acs_soc.dta")

# 1% test

acs_1pct <- read_dta("nca_acs_soc_1pct.dta")

# AGGREGATE UP TO THE STATE-LEVEL----------------------------------------------- 

# Cleaning the data up

# Main
acs_clean <- acs %>%
  mutate(across(where(is.labelled), zap_labels)) %>%
  mutate(
    year = as.integer(year),
    statefip = as.integer(statefip)
  )

# 1%
acs_1pct_clean <- acs_1pct %>%
  mutate(across(where(is.labelled), zap_labels)) %>%
  mutate(
    year = as.integer(year),
    statefip = as.integer(statefip)
  )  

# Choose variables to keep

cont_vars <- c("age", "pot_exp", "incwage")

prop_vars <- c(
  "young_adult", "earlyc_adult", "mlc_adult", "older_adult",
  "no_high_school", "high_school", "some_college", "college"
)

treat_vars <- c("treated_eff", "treated_enact", "year_eff_ban", "year_enact_ban", "hw_ban")

# Incorporate survey weights 

# NOTE: This code was for when I was using the svy package approach which takes
# forever and is unnecessary for point estimates. Disregard for the simple 
# weighted mean version. 

# Main
acs_svy <- acs_clean %>%
  select(
    statefip, state, year, perwt,
    all_of(cont_vars), 
    all_of(prop_vars),
    all_of(treat_vars)
  ) %>%
  as_survey(weights = perwt)

# 1%
acs_1pct_svy <- acs_1pct_clean %>%
  select(
    statefip, state, year, perwt,
    age, pot_exp, incwage, 
    young_adult, earlyc_adult, mlc_adult, older_adult,
    no_high_school, high_school, some_college, college,
    treated_eff, year_eff_ban, hw_ban
  ) %>%
  as_survey(weights = perwt)


# Computing the means and proportions 

# This code takes way too long
# Main
#state_year <- acs_svy %>%
#  group_by(statefip, state, year) %>%
#  summarise(
#    across(all_of(cont_vars),
#           ~ survey_mean(.x, na.rm = TRUE, vartype = NULL),
#           .names = "{.col}_mean"),
#    across(all_of(prop_vars),
#           ~survey_mean(.x, na.rm = TRUE, vartype = NULL),
#           .names = "{.col}_prop")
#  ) %>%
#  ungroup()

# 1% 
#state_year_1pct <- acs_1pct_svy %>%
#  group_by(statefip, state, year) %>%
#  summarise(
#    across(all_of(cont_vars),
#           ~ survey_mean(.x, na.rm = TRUE, vartype = NULL),
#           .names = "{.col}_mean"),
#    across(all_of(prop_vars),
#           ~survey_mean(.x, na.rm = TRUE, vartype = NULL),
#           .names = "{.col}_prop")
#  ) %>%
#  ungroup()

