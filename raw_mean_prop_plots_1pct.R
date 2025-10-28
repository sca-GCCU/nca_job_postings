# PREAMBLE ---------------------------------------------------------------------

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/data")

install.packages("forcats")

library(tidyverse)
library(haven)
library(panelView)
library(srvyr)
library(forcats)

# LOAD DATA --------------------------------------------------------------------

# ACS data

acs_1pct <- read_dta("nca_acs_soc_1pct.dta")

# Treatment Panel 

treatment_panel <- read_csv(
  "state_year_R_treatment_panel.csv",
  col_select = -1
)

# AGGREGATE UP TO THE STATE-LEVEL----------------------------------------------- 

# Cleaning the data up

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

# Compute weighted means and proportions by state-year

state_year_1pct <- acs_1pct_clean %>%
  group_by(statefip, state, year) %>%
  summarise(
    across(all_of(cont_vars),
           ~ weighted.mean(.x, perwt, na.rm = TRUE),
           .names = "{.col}_mean"),
    across(all_of(prop_vars),
           ~ weighted.mean(.x, perwt, na.rm = TRUE),
           .names = "{.col}_prop")
  ) %>%
  ungroup()

plot_panel_1pct <- state_year_1pct %>%
  left_join(treatment_panel, by = c("statefip", "state", "year")) %>%
  mutate(
    across(starts_with("eff_"), as.integer),
    across(starts_with("enact_"), as.integer)
  ) 


# PLOTTING MEANS AND PROPORTIONS -----------------------------------------------

# High- or low-wage bans by cohort

# NOTE 1: I've already dropped full_ban states in my construction of the acs data.

# NOTE 2: This data doesn't currently save the plots. I might want to automate 
# that process too. 

# 1) Define a var for treatment status

any_ban_panel_1pct <- plot_panel_1pct %>%
  mutate(
    eff_inc = ifelse(eff_hw == 1 | eff_lw == 1, 1L, 0L)
  ) %>%
  select(statefip:state_abb, eff_inc) # drop unnecessary vars 

# 2) Plot the means and proportions 

# a) Means

panelview(age_mean ~ eff_inc,
          data = any_ban_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Mean Age", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(pot_exp_mean ~ eff_inc,
          data = any_ban_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Mean Potential Experience", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(incwage_mean ~ eff_inc,
          data = any_ban_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Mean Earnings", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

# b) Proportions

panelview(young_adult_prop ~ eff_inc,
          data = any_ban_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Proportion Young-Adult", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(earlyc_adult_prop ~ eff_inc,
          data = any_ban_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Proportion Early-Career", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(mlc_adult_prop ~ eff_inc,
          data = any_ban_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Proportion Mid- to Late-Career", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(older_adult_prop ~ eff_inc,
          data = any_ban_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Proportion Near-Retirement", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(no_high_school_prop ~ eff_inc,
          data = any_ban_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Proportion No High-School", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(high_school_prop ~ eff_inc,
          data = any_ban_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Proportion High-School", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(some_college_prop ~ eff_inc,
          data = any_ban_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Proportion Some College", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(college_prop ~ eff_inc,
          data = any_ban_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Proportion College", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

rm(any_ban_panel_1pct)

# High-wage bans by cohort

# 1) Drop the low-wage ban states

hw_panel_1pct <- plot_panel_1pct %>% 
  filter(is.na(hw_ban) | hw_ban == 1) %>%
  select(statefip:state.abb, eff_hw)

# 2) Plot means and proportions 

# a) Means

panelview(age_mean ~ eff_hw,
          data = hw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Mean Age", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(pot_exp_mean ~ eff_hw,
          data = hw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Mean Potential Experience", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(incwage_mean ~ eff_hw,
          data = hw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Mean Earnings", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

# b) Proportions

panelview(young_adult_prop ~ eff_hw,
          data = hw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Proportion Young-Adult", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(earlyc_adult_prop ~ eff_hw,
          data = hw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Proportion Early-Career", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(mlc_adult_prop ~ eff_hw,
          data = hw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Proportion Mid- to Late-Career", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(older_adult_prop ~ eff_hw,
          data = hw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Proportion Near-Retirement", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(no_high_school_prop ~ eff_hw,
          data = hw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Proportion No High-School", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(high_school_prop ~ eff_hw,
          data = hw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Proportion High-School", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(some_college_prop ~ eff_hw,
          data = hw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Proportion Some College", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(college_prop ~ eff_hw,
          data = hw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Proportion College", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))


rm(hw_panel_1pct)


# Low-wage bans by cohort 

# 1) Drop the high-wage states 

lw_panel_1pct <- plot_panel_1pct %>%
  filter(is.na(hw_ban) | hw_ban == 0) %>%
  select(statefip:state_abb, eff_lw)
  
# 2) Plot means and proportions 

# a) Means

panelview(age_mean ~ eff_lw,
          data = lw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Mean Age", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(pot_exp_mean ~ eff_lw,
          data = lw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Mean Potential Experience", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(incwage_mean ~ eff_lw,
          data = lw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Mean Earnings", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

# b) Proportions

panelview(young_adult_prop ~ eff_lw,
          data = lw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Proportion Young-Adult", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(earlyc_adult_prop ~ eff_lw,
          data = lw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Proportion Early-Career", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(mlc_adult_prop ~ eff_lw,
          data = lw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Proportion Mid- to Late-Career", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(older_adult_prop ~ eff_lw,
          data = lw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Proportion Near-Retirement", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(no_high_school_prop ~ eff_lw,
          data = lw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Proportion No High-School", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(high_school_prop ~ eff_lw,
          data = lw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Proportion High-School", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(some_college_prop ~ eff_lw,
          data = lw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Proportion Some College", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

panelview(college_prop ~ eff_lw,
          data = lw_panel_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Proportion College", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

rm(lw_panel_1pct)


# AGE-STRATIFIED ANALYSIS-------------------------------------------------------

# CREATE STRATIFIED DATASET 

acs_by_age <- acs_1pct_clean %>%
  # collapse the mutually exclusive age dummies into one factor
  mutate(
    age_group = case_when(
      young_adult == 1 ~ "Young adult",
      earlyc_adult == 1 ~ "Early-career",
      mlc_adult == 1 ~ "Mid–late career",
      older_adult == 1 ~ "Older adult",
      TRUE ~ NA_character_
    ),
    age_group = fct_relevel(
      factor(age_group),
      "Young adult","Early-career","Mid–late career","Older adult"
    )
  ) %>%
  filter(!is.na(age_group)) %>%  # drop any rows that didn't match a stratum
  group_by(statefip, state, year, age_group) %>%
  summarise(
    # continuous vars (means)
    across(all_of(cont_vars),
           ~ weighted.mean(.x, perwt, na.rm = TRUE),
           .names = "{.col}_mean"),
    # explicit income aggregates (nice to name them plainly for plotting)
    incwage_mean = weighted.mean(incwage, perwt, na.rm = TRUE),
    inctot_mean  = weighted.mean(inctot,  perwt, na.rm = TRUE),
  ) %>%
  ungroup()

plot_age_strat_1pct <- acs_by_age %>%
  left_join(treatment_panel, by = c("statefip","state","year")) %>%
  mutate(
    across(starts_with("eff_"),   ~ as.integer(replace_na(.x, 0))),
    across(starts_with("enact_"), ~ as.integer(replace_na(.x, 0)))
  )

# YOUNG

plot_young_1pct <- plot_age_strat_1pct %>%
  filter(age_group == "Young adult") %>%
  mutate(
    eff_inc = ifelse(eff_hw == 1 | eff_lw == 1, 1L, 0L)
  ) %>%
  select(statefip:year, incwage_mean, state_abb, eff_inc, eff_hw, eff_lw)
  
# Any income ban by cohort

panelview(incwage_mean ~ eff_inc,
          data = plot_young_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Earnings - Young", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

# High-wage ban by cohort 

panelview(incwage_mean ~ eff_hw,
          data = plot_young_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Earnings - Young", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

# Low-wage ban by cohort 

panelview(incwage_mean ~ eff_lw,
          data = plot_young_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Earnings - Young", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

rm(plot_young_1pct)

# EARLY-CAREER 

plot_early_1pct <- plot_age_strat_1pct %>%
  filter(age_group == "Early-career") %>%
  mutate(
    eff_inc = ifelse(eff_hw == 1 | eff_lw == 1, 1L, 0L)
  ) %>%
  select(statefip:year, incwage_mean, state_abb, eff_inc, eff_hw, eff_lw)

# Any income ban by cohort

panelview(incwage_mean ~ eff_inc,
          data = plot_early_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Earnings - Early Career", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

# High-wage ban by cohort 

panelview(incwage_mean ~ eff_hw,
          data = plot_early_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Earnings - Early Career", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

# Low-wage ban by cohort 

panelview(incwage_mean ~ eff_lw,
          data = plot_early_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Earnings - Early Career", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

rm(plot_early_1pct)

# MID-LATE CAREER

plot_mid_1pct <- plot_age_strat_1pct %>%
  filter(age_group == "Mid–late career") %>%
  mutate(
    eff_inc = ifelse(eff_hw == 1 | eff_lw == 1, 1L, 0L)
  ) %>%
  select(statefip:year, incwage_mean, state_abb, eff_inc, eff_hw, eff_lw)

# Any income ban by cohort

panelview(incwage_mean ~ eff_inc,
          data = plot_mid_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Earnings - Mid-late Career", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

# High-wage ban by cohort 

panelview(incwage_mean ~ eff_hw,
          data = plot_mid_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Earnings - Mid-late Career", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

# Low-wage ban by cohort 

panelview(incwage_mean ~ eff_lw,
          data = plot_mid_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Earnings - Mid-late Career", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

rm(plot_mid_1pct)

# OLDER 

plot_old_1pct <- plot_age_strat_1pct %>%
  filter(age_group == "Older adult") %>%
  mutate(
    eff_inc = ifelse(eff_hw == 1 | eff_lw == 1, 1L, 0L)
  ) %>%
  select(statefip:year, incwage_mean, state_abb, eff_inc, eff_hw, eff_lw)

# Any income ban by cohort

panelview(incwage_mean ~ eff_inc,
          data = plot_old_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Any Income Ban and Earnings - Near Retirement", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

# High-wage ban by cohort 

panelview(incwage_mean ~ eff_hw,
          data = plot_old_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "High-wage Ban and Earnings - Near Retirement", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

# Low-wage ban by cohort 

panelview(incwage_mean ~ eff_lw,
          data = plot_old_1pct, index = c("state_abb", "year"), 
          type = "outcome", main = "Low-wage Ban and Earnings - Near Retirement", 
          by.cohort = TRUE,
          color = c("lightblue", "blue", "#99999950"))

rm(plot_old_1pct)





# Incorporate survey weights 

# NOTE: This code was for when I was using the svy package approach which takes
# forever and is unnecessary for point estimates. Disregard for the simple 
# weighted mean version. 

# Main
#acs_svy <- acs_clean %>%
#  select(
#    statefip, state, year, perwt,
#    all_of(cont_vars), 
#    all_of(prop_vars),
#    all_of(treat_vars)
#  ) %>%
#  as_survey(weights = perwt)

# 1%
#acs_1pct_svy <- acs_1pct_clean %>%
#  select(
#    statefip, state, year, perwt,
#    age, pot_exp, incwage, 
#    young_adult, earlyc_adult, mlc_adult, older_adult,
#    no_high_school, high_school, some_college, college,
#    treated_eff, year_eff_ban, hw_ban
#  ) %>%
#  as_survey(weights = perwt)


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

