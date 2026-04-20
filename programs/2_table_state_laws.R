##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "table_state_laws" 
# by: Sebastian C. Anastasi
# Date of this version: March 6, 2026
#
# Description: This script prepares... 
#
# Dependencies: 
#
# Output: 
##############################################################################

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

#install.packages("kableExtra")

library(dplyr)
library(readr)
library(tibble)
library(knitr)
library(kableExtra)
library(lubridate)
library(scales) 

state_laws <- read_csv("data/raw-data/state_nca_laws.csv")
cpi <- read_csv("data/clean-data/cpi_clean.csv")

# 1. INCOME BANS TABLE 

# A. States that update their wage thresholds 
update_states <- state_laws %>%
  filter(ban_inc2 == 1) %>%
  select(state)
update_states 

# B. Table of income ban states 
state_laws_ib <- state_laws %>%
  filter(ban_inc1 == 1) %>%
  select(
    state, eff_inc1_year, eff_inc1_month, inc_coverage1, inc_threshold1,
    #eff_inc2_year, eff_inc2_month, inc_coverage2, inc_threshold2
  ) %>%
  mutate(
    date = make_date(eff_inc1_year, eff_inc1_month, 1)
  )

# Merge to CPI data and convert to base year dollars 
# NOTE: Base year is currently 2022 to match MN's ban year)
state_laws_ib <- state_laws_ib %>%
  left_join(
    cpi,
    by = "date"
  )

# Convert to base year dollars 
state_laws_ib <- state_laws_ib %>%
  mutate(
    inc_threshold1_real = inc_threshold1 * cpi_deflator
  )

# Only keep the stuff I want in the final table 
state_laws_ib <- state_laws_ib %>%
  select(
    state,
    eff_inc1_year,
    eff_inc1_month,
    inc_threshold1,
    inc_threshold1_real
  )

# Reformat for final table 
month_number <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
month_number 
month.name

months <- tibble(
  month_number = 1:12,
  month_name = month.name,
  month_abb = month.abb
)

state_laws_ib <- state_laws_ib %>%
  left_join(
    months,
    by = c("eff_inc1_month" = "month_number"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    Date = sprintf("%s %04d", month_name, eff_inc1_year),
    inc_threshold1 = format(inc_threshold1, big.mark = ","),
    inc_threshold1_real = format(inc_threshold1_real, big.mark = ",")
  ) 

# Keep only the variables I need and put them in the right order
state_laws_ib <- state_laws_ib %>%
  select(
    state,
    Date,
    inc_threshold1,
    inc_threshold1_real
  ) 

# Create the Latex table 
income_bans_tab <- state_laws_ib %>%
  kable(format = "latex", booktabs = TRUE, 
        linesep = "", align = c("llrr"),
        caption = "Noncompete Income Restrictions", label = "income_restrictions",
        col.names = c("State", "Date", "Threshold (\\$)", "Theshold (Real \\$)"),
        escape = FALSE) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the date that states' noncompete income restrictions become effective. Of these states, Illinois, Maryland, and Oregon passed further laws to raise these thresholds in subsequent years. The third column reports the income threshold at the time the initial law is passed. Column four reports these income thresholds converted into 2022 dollars using the Consumer Price Index (CPI).",
    threeparttable = TRUE,
    escape = FALSE # enables me to force footnotesize using LaTex commands
  ) 

# Write table to a .tex file 
writeLines(income_bans_tab, "output/tables/table_income_bans.tex")

print(income_bans_tab)



# 2. OCCUPATION/INDUSTRY BANS 

# HAVE TO THINK ABOUT THE RIGHT WAY TO HANDLE MULTIPLE POTENTIAL BAN DATES.
# ONLY HAPPENS WHEN STATE HAS IND-BAN AND HEALTH-BAN. 


# MISCELLANEOUS 

# Number of income bans 
n_inc_ban <- state_laws %>%
  filter(ban_inc1 == 1) %>%
  summarise(N = n()) %>%
  pull(N)
n_inc_ban

    # Number of income bans since 2019 
    n_inc_ban_2019 <- state_laws %>%
      filter(ban_inc1 == 1) %>%
      filter(enact_inc1_year >= 2019 | enact_hourly_year >= 2019) %>%
      summarise(N = n()) %>%
      pull(N)
    n_inc_ban_2019

    # Minimum income ban year 
    min_inc_ban_year <- state_laws %>%
      filter(ban_inc1 == 1) %>%
      summarise(
        min_year_eff = min(eff_inc1_year),
        min_year_enact = min(enact_inc1_year)
      ) %>%
      pull(min_year_eff, min_year_enact)
    min_inc_ban_year

# Number of hourly bans 
n_hour_ban <- state_laws %>%
  filter(ban_hourly == 1) %>%
  summarise(
    N = n(),
    eff_hourly_year = eff_hourly_year,
    state = state
  )
n_hour_ban

# Number of other bans 
n_other_ban <- state_laws %>%
  filter(ban_other == 1) %>%
  summarise(
    N = n(),
    eff_other_year = eff_other_year,
    state = state
  )
n_other_ban

# Number of occupation/industry restrictions (including health)
n_ind_ban <- state_laws %>%
  filter(ban_ind == 1 | ban_health1 == 1) %>%
  summarise(
    N = n()
  ) %>%
  pull(N)
n_ind_ban

# Retroactive bans 
retro <- state_laws %>%
  filter(retro_full == 1 | retro_inc1 == 1 | retro_inc2 == 1 | retro_hourly == 1 | 
           retro_ind == 1 | retro_health1 == 1 | retro_health2 == 1 | retro_health3 == 1)

