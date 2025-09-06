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

# 1. Convert NCA laws file and drop variables I don't need

# Load dta files 

#nca_laws_panel <- read_dta("nca_laws_panel.dta")

# Drop variables I don't need 

#nca_laws_panel %>%
#  select(statefip, state, year, full_ban, ban, year_eff_ban, month_eff_ban, hw_ban)

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

# NEXT STEP: Merge nca_laws to state_date_panel. Define treatment variables. 


# Convert to CSV

write.csv(nca_laws_panel, "nca_laws_panel.csv")

# NOTE: Pretty sure these datasets have the wrong date range, since I have to 
# go from 2010 to 2015.

# 2. Convert BLS Employment data 

#bls_emp <- read_dta("bls_emp.dta")

#bls_emp %>%
#  select(-employment_nsa)

#write.csv(bls_emp, "bls_emp.csv")

bls_emp_2025 <- read_excel("bls_employment_2025.xlsx")

write.csv(bls_emp_2025, "bls_emp_2025.csv")


# 3. Convert FHFA HPI data

fhfa_hpi1 <- read_dta("fhfa_hpi1.dta")

fhfa_hpi1 %>% glimpse()









