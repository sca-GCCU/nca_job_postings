##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "state_law_map.R" 
# by: Sebastian C. Anastasi
# Date of this version: March 11, 2026
#
# Description: This script generates a map of the NCA policies of the 50 states
# and D.C.
#
# Dependencies:  
#
# Output: "state_law_map.pdf"
##############################################################################

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

library(tidyverse)
library(usmap)
library(lubridate)

# 1. CREATE MAP OF NONCOMPETE POLICIES ----------------------------------------

# Load policy dataset 
state_nca_laws <- read.csv("data/raw-data/state_nca_laws.csv") 

state_nca_laws <- state_nca_laws %>%
  mutate(
    date_full = make_date(eff_full_year, coalesce(eff_full_month, 1), 1),
    date_inc1 = make_date(eff_inc1_year, coalesce(eff_inc1_month, 1), 1),
    date_health1 = make_date(eff_health1_year, coalesce(eff_health1_month, 1), 1),
    date_ind = make_date(eff_ind_year, coalesce(eff_ind_month, 1), 1),
    date_hourly = make_date(eff_hourly_year, coalesce(eff_hourly_month, 1), 1),
    date_other = make_date(eff_other_year, coalesce(eff_other_month, 1), 1)
  )

# Set bounds of data 
# NOTE: In practice, I will only use the date_ub to determine what states have 
# bans in the map during the analysis period.
date_lb <- make_date(2010, 1, 1)  
date_ub <- make_date(2025, 1, 1)

# New analysis-window-specific treatment variables 
state_nca_laws <- state_nca_laws %>%
  mutate(
    full = !is.na(date_full) & date_full <= date_ub,
    inc = !is.na(date_inc1) & date_inc1 <= date_ub,
    health = !is.na(date_health1) & date_health1 <= date_ub,
    ind = !is.na(date_ind) & date_ind <= date_ub,
    hourly = !is.na(date_hourly) & date_hourly <= date_ub,
    other = !is.na(date_other) & date_other <= date_ub
  )

  # Check WY ban is outside of range
  # NOTE: If in range, I would have to decide how to treat their ban.
state_nca_laws %>% filter(other)

# Code up variables for different policy types
state_nca_laws_map <- state_nca_laws %>%
  mutate(
    policy_type = case_when(
      full ~ "Full",
      ( inc | hourly ) & ( health | ind ) ~ "Income + Occupation/Industry",
      ( inc | hourly ) ~ "Income",
      ( health | ind ) ~ "Occupation/Industry",
      .default = "None"
    )
  ) %>%
  select(
    statefip,
    state,
    starts_with("date_"),
    policy_type
  )

rm(state_nca_laws)

state_nca_laws_map <- state_nca_laws_map %>%
  mutate(
    policy_type = factor(
      policy_type,
      levels = c(
        "None",
        "Occupation/Industry",
        "Income",
        "Income + Occupation/Industry",
        "Full"
      ),
      ordered = TRUE
    )
  )

# Load map data
map_df <- us_map(
  regions = "states",
  exclude = "PR"
)

map_df <- map_df %>%
  mutate(
    fips = as.integer(fips)
  ) %>%
  left_join(
    state_nca_laws_map,
    by = c("fips" = "statefip"),
    relationship = "one-to-one"
  )

# Generate the map
ggplot(map_df) + 
  geom_sf(aes(fill = policy_type)) + 
  theme_minimal() + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank()) +
  scale_fill_brewer(
    name = "",
    palette = "Greys", # Other interesting options: Blues, YlOrBr
    direction = 1
  )

# Save the map
ggsave(
  "output/state_law_map.pdf",
  width = 7,
  height = 4.5,
  units = "in"
)



