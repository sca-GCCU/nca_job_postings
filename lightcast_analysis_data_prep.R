# Clear everything in the environment 

rm(list = ls())

# Change directory to the directory where my data is stored

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/data")

# Load packages 

library(readr)
library(readxl)
library(tidyverse)
library(haven)
library(lubridate)

# TREATMENT DATA PREP 

state_data <- tribble(
  ~state_abb, ~statefip,
  "AL", 1,  "AK", 2,  "AZ", 4,  "AR", 5,  "CA", 6,  "CO", 8,  "CT", 9,  "DE",10, "DC",11,
  "FL",12,  "GA",13, "HI",15, "ID",16, "IL",17, "IN",18, "IA",19, "KS",20, "KY",21,
  "LA",22,  "ME",23, "MD",24, "MA",25, "MI",26, "MN",27, "MS",28, "MO",29, "MT",30,
  "NE",31,  "NV",32, "NH",33, "NJ",34, "NM",35, "NY",36, "NC",37, "ND",38, "OH",39,
  "OK",40,  "OR",41, "PA",42, "RI",44, "SC",45, "SD",46, "TN",47, "TX",48, "UT",49,
  "VT",50,  "VA",51, "WA",53, "WV",54, "WI",55, "WY",56
)

months_data <- tibble(
  date = seq.Date(from = as.Date("2010-01-01"),
                  to = as.Date("2025-08-01"),
                  by = "month")
) %>% mutate(year = year(date), month = month(date))

state_date_panel <- expand_grid(state_data, months_data) %>%
  select(statefip, state_abb, year, month) %>%
  arrange(statefip, year, month)

nca_laws <- read_dta("nca_laws_gks.dta")

treatment_panel <- state_date_panel %>%
  left_join(nca_laws, by = "statefip")

# Creating treatment dummies (which I may not need)
treatment_panel <- treatment_panel %>%
  mutate(
    panel_date = make_date(year, month, 1L),
    eff_date = make_date(year_eff_ban, month_eff_ban, 1L), 
    enact_date = make_date(year_enact_ban, month_enact_ban, 1L)
  ) %>%
  mutate(
    treated_eff = if_else(!is.na(eff_date) & panel_date >= eff_date, 1L, 0L),
    treated_enact = if_else(!is.na(enact_date) & panel_date >= enact_date, 1L, 0L)
  )

# Creating unified time index (tname) and group variable (gname) for CS DID
treatment_panel <- treatment_panel %>%
  mutate(
    time_id = year*12L + month,
    gvar_eff = if_else(!is.na(year_eff_ban) & !is.na(month_eff_ban),
                       year_eff_ban*12L + month_eff_ban, 0L),
    gvar_enact = if_else(!is.na(year_enact_ban) & !is.na(month_enact_ban),
                         year_enact_ban*12L + month_enact_ban, 0L)
  )

# Creating year-month variable for merging to Lightcast 
treatment_panel <- treatment_panel %>%
  mutate(year_month = sprintf("%04d-%02d", year, month))

# Dropping variables I don't need 
treatment_panel <- treatment_panel %>%
  rename(ever_treated = ban) %>%
  select(statefip, state, year_month, time_id, gvar_eff, ever_treated, 
         full_ban, hw_ban, treated_eff) # Note: Excluding enact var for now. 
  
# Save to CSV
write.csv(treatment_panel, "lightcast_treatment_panel.csv")


# COVARIATE DATA PREP

# 1. BLS Employment data 

bls_emp_2025 <- read_excel("bls_employment_2025.xlsx")

bls_emp_2025 <- bls_emp_2025 %>%
  filter(!(state %in% c("Los Angeles County", "New York city"))) %>%
  filter(year >= 2010) %>%
  mutate(year_month = sprintf("%04d-%02d", year, month))

bls_emp_2025 <- bls_emp_2025 %>%
  select(statefip, state, year_month, employment_sa)

write.csv(bls_emp_2025, "bls_emp_2025.csv")


# 2. FHFA HPI data

# NOTE: Switch to seasonally-adjusted purchase-only HPI. 
# Need to do in Stata too. 

hpi_po_state <- read_excel("hpi_po_state.xlsx")

hpi_po_state <- hpi_po_state %>%
  left_join(state_data, by = c("state" = "state_abb"))

hpi_po_state <- hpi_po_state %>%
  tidyr::uncount(weights = 3, .id = "m_in_qtr") %>%
  mutate(
    month = (qtr - 1)*3 + m_in_qtr,
    date = make_date(yr, month, 1)
  ) %>%
  select(statefip, yr, qtr, month, date, index_sa) %>% glimpse()

hpi_po_state <- hpi_po_state %>%
  filter(yr >= 2010) %>%
  mutate(year_month = sprintf("%04d-%02d", yr, month)) %>%
  select(statefip, year_month, index_sa)

write.csv(hpi_po_state, "hpi_po_state.csv")

# 3. BEA Income Data 










