# PREAMBLE ---------------------------------------------------------------------

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/data")

#install.packages('panelView')

library(panelView)
library(tidyverse)  # loads dplyr, tidyr, tibble, readr, etc.
library(haven)      # for read_dta()

state_data <- tribble(
  ~state_abb, ~statefip,
  "AL", 1,  "AK", 2,  "AZ", 4,  "AR", 5,  "CA", 6,  "CO", 8,  "CT", 9,  "DE",10, "DC",11,
  "FL",12,  "GA",13, "HI",15, "ID",16, "IL",17, "IN",18, "IA",19, "KS",20, "KY",21,
  "LA",22,  "ME",23, "MD",24, "MA",25, "MI",26, "MN",27, "MS",28, "MO",29, "MT",30,
  "NE",31,  "NV",32, "NH",33, "NJ",34, "NM",35, "NY",36, "NC",37, "ND",38, "OH",39,
  "OK",40,  "OR",41, "PA",42, "RI",44, "SC",45, "SD",46, "TN",47, "TX",48, "UT",49,
  "VT",50,  "VA",51, "WA",53, "WV",54, "WI",55, "WY",56
)

years <- tibble(year = 2001:2023)

state_year_panel <- expand_grid(state_data, years) %>%
  select(statefip, state_abb, year) %>%
  arrange(statefip, year)

nca_laws <- read_dta("nca_laws_gks.dta")

treatment_panel <- state_year_panel %>%
  left_join(nca_laws, by = "statefip")

# Recode Hawaii as untreated

treatment_panel <- treatment_panel %>%
  mutate(
    ban = if_else(state_abb == "HI", 0, ban),
    year_enact_ban = if_else(state_abb == "HI", NA_real_, year_enact_ban),
    month_enact_ban = if_else(state_abb == "HI", NA_real_, month_enact_ban),
    year_eff_ban = if_else(state_abb == "HI", NA_real_, year_eff_ban),
    month_eff_ban = if_else(state_abb == "HI", NA_real_, month_eff_ban),
    hw_ban = if_else(state_abb == "HI", NA_real_, hw_ban)
  )


treatment_panel <- treatment_panel %>%
  mutate(
    eff_full = full_ban == 1 & year >= year_full_ban,
    eff_hw = hw_ban == 1 & year >= year_eff_ban,
    eff_lw = hw_ban == 0 & year >= year_eff_ban,
    
    treated_levels_num = case_when(
      eff_full ~ 3L,
      eff_hw ~ 2L,
      eff_lw ~ 1L,
      TRUE ~ 0L
    ),
    
    enact_full = full_ban == 1 & year >= year_full_ban,
    enact_hw = hw_ban == 1 & year >= year_enact_ban,
    enact_lw = hw_ban == 0 & year >= year_enact_ban,
    
    enacted_levels_num = case_when(
      enact_full ~ 3L,
      enact_hw ~ 2L,
      enact_lw ~ 1L,
      TRUE ~ 0L
    )
  )

panelview(1 ~ treated_levels_num,
          data = treatment_panel,
          index = c("state_abb", "year"),
          type = "treat",
          legend.labs = c("No ban", "Low-wage ban", "High-wage ban", "Full ban"),
          xlab = "Year",
          ylab = "State",
          main = "Noncompete Bans - Rollout by Effective Date",
          color = c("#B3CDE3", "#6497B1", "#005B96", "#03396C"),
          background = "white"
)

ggsave(
  "C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/figures/rollout_effective.png",
  plot = last_plot(),
  width = 8, height = 7
)

panelview(1 ~ enacted_levels_num,
          data = treatment_panel,
          index = c("state_abb", "year"),
          type = "treat",
          legend.labs = c("No ban", "Low-wage ban", "High-wage ban", "Full ban"),
          xlab = "Year",
          ylab = "State",
          main = "Noncompete Bans - Rollout by Enactment Date",
          color = c("#B3CDE3", "#6497B1", "#005B96", "#03396C"),
          background = "white"
)

ggsave(
  "C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/figures/rollout_enacted.png",
  plot = last_plot(),
  width = 8, height = 7
)

