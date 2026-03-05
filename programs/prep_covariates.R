##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "prep_covariates" 
# by: Sebastian C. Anastasi
# Date of this version: March 4, 2026
#
# Description: This script prepares the monthly covariates. 
##############################################################################

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

library(tidyverse)
library(readxl)
library(lubridate)
library(haven)

# Lightcast data range (used in multiple steps below)
date_lb <- make_date(2010, 1, 1)
date_ub <- make_date(2025, 1, 1)

# 1. CPI data for deflating variables 
# NOTE: The exact dataset ("Historical CPI-U, January 2026") is found here: 
# https://www.bls.gov/cpi/tables/supplemental-files/. CPI-U is the CPI for 
# "all urban consumers."  

cpi <- read_xlsx(
  "data/raw-data/historical-cpi-u-202601.xlsx", 
  range = "B6:N119",
  na = "–",
  col_names = FALSE, # don't use first row as names; get default names
  trim_ws = TRUE
)

cpi <- cpi %>%
  rename_with(
    ~ifelse(
      str_detect(.x, "^\\.\\.\\.[0-9]+"),
      as.character(as.integer(str_remove(.x, "^\\.\\.\\.")) - 1),
      .x
    )
  ) %>%
  rename(year = "0")

cpi_long <- cpi %>%
  pivot_longer(
    cols = matches("^[0-9]+"),
    names_to = "month",
    values_to = "cpi"
  ) %>%
  mutate(
    year = as.numeric(year),
    month = as.numeric(month)
  ) %>%
  mutate(
    date = make_date(year, month, 1)
  )

rm(cpi)

base_period <- 2022

base_cpi <- cpi_long %>%
  filter(year == base_period) %>%
  summarise(base_cpi = mean(cpi, na.rm = TRUE)) %>%
  pull(base_cpi)

cpi_clean <- cpi_long %>%
  mutate(
    cpi_deflator = base_cpi / cpi
  ) %>%
  filter(date >= date_lb) %>%
  filter(date <= date_ub)

rm(cpi_long, base_cpi, base_period)

write_csv(cpi_clean, "data/analysis-data/cpi_clean.csv")

# NOTE: Be aware that October 2025 data is missing (perhaps we could interpolate
# it if necessary in future) and much of the 2026 is missing. 


# 2. Economic controls

# A.1 Unemployment - Monthly 
# NOTE: Using "Employment status of the civilian noninstitutional population, 
# not seasonally adjusted" (https://www.bls.gov/lau/rdscnp16.htm), from the BLS. 

bls <- read_xlsx(
  "data/raw-data/ststdnsadata.xlsx", 
  range = "A9:K31808",
  na = "–",
  col_names = FALSE, 
  trim_ws = TRUE
)

bls <- bls %>%
  select(
    ...1:...4, 
    ...11
  ) %>%
  rename(
    state = "...1",
    state_name = "...2",
    year = "...3",
    month = "...4",
    unemp_rate = "...11"
  ) %>%
  mutate(
    state = as.numeric(state),
    year = as.numeric(year),
    month = as.numeric(month)
  )

bls_clean <- bls %>%
  mutate(
    date = make_date(year, month, 1)
  ) %>%
  filter(date >= date_lb) %>%
  filter(date <= date_ub) %>%
  filter(!(state_name %in% c("Los Angeles County", "New York city"))) # non-state FIPS

rm(bls)


# A.2 Unemployment - Annual (only goes up to 2024)
# NOTE: Using "Employment status of the civilian noninstitutional population, 
# annual averages" (https://www.bls.gov/lau/rdscnp16.htm), from the BLS. 

#bls_annual <- read_xlsx(
#  "data/raw-data/staadata.xlsx",
#  range = "A9:J2605",
#  col_names = FALSE,
#  trim_ws = TRUE
#)



# B.1 Total Personal Income - Monthly
# NOTE: BEA interactive data, nominal/current dollars "SQINC1 State quarterly  
# personal income summary: personal income, population, per capita personal  
# income" (https://www.bea.gov/data/income-saving/personal-income-by-state).
# This is a statewide measure in millions of dollars.

bea <- read.csv(
  "data/raw-data/Table.csv",
  header = TRUE, 
  skip = 3,
  nrows = 51
)

# Cleaning up the data 
bea_long <- bea %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year_quarter",
    values_to = "income"
  ) %>% 
  mutate(
    state = GeoFIPS / 1000,
  ) %>%
  select(-GeoFIPS) %>%
  mutate(
    GeoName = recode(GeoName, "Alaska *" = "Alaska", "Hawaii *" = "Hawaii") 
  ) %>%
  rename(
    state_name = GeoName
  )

rm(bea)

bea_long <- bea_long %>%
  mutate(
    year_quarter = sub("^X", "", year_quarter)
  ) %>%
  separate(
    year_quarter, into = c("year", "quarter"), sep = "\\.Q"
  ) %>%
  mutate(
    year = as.integer(year),
    quarter = as.integer(quarter)
  )

# Quarter to month mapping 
quarter_to_month <- read_xlsx("data/raw-data/quarter_to_month_crosswalk.xlsx")

bea_monthly <- bea_long %>%
  left_join(
    quarter_to_month,
    by = "quarter",
    relationship = "many-to-many"
  )

rm(quarter_to_month, bea_long)

bea_clean <- bea_monthly %>%
  mutate(
    date = make_date(year, month, 1)
  ) %>%
  select(
    -month_name,
    -quarter
  ) %>%
  filter(
    date >= date_lb,
    date <= date_ub
  )

rm(bea_monthly)


# B.2 Per Capita Personal Income - Monthly
# NOTE: BEA interactive data, nominal/current dollars "SQINC1 State quarterly  
# personal income summary: personal income, population, per capita personal  
# income" (https://www.bea.gov/data/income-saving/personal-income-by-state).

bea_per_capita <- read.csv(
  "data/raw-data/Table_per_capita.csv",
  header = TRUE, 
  skip = 3,
  nrows = 51
)

# NOTE: JUST REPLACE B.1 WITH B.2. IDENTICAL CODE!



# B.3 Per Capita Personal Income - Annual (only goes up to 2024)
# NOTE: BEA interactive data, nominal/current dollars "SAINC1 - State annual 
#personal income summary: personal income, population, per capita personal 
#income" (https://www.bea.gov/data/income-saving/personal-income-by-state).




# C. Housing Price Index - Monthly
# NOTE: U.S. Federal Housing Finance Agency (FHFA) House Price Index Datasets, 
# "Master HPI Data" (https://www.fhfa.gov/data/hpi/datasets?tab=master-hpi-data).
# State data is only available on a quarterly basis (not monthly or annual). 

hpi <- read.csv("data/raw-data/hpi_master.csv", header = TRUE)

hpi_state <- hpi %>% 
  filter(
    hpi_type == "traditional",
    hpi_flavor == "purchase-only",
    frequency == "quarterly", # only available frequency for States
    level == "State"
  ) %>%
  rename(
    state_name = "place_name",
    state_abb = "place_id",
    year = "yr",
    quarter = "period"
  )

hpi_state <- hpi_state %>%
  select(
    -index_sa, # only using not seasonally adjusted initially
    -hpi_type, 
    -hpi_flavor,
    -frequency,
    -level
  )  

# Quarter to monthly mapping 
quarter_to_month <- read_xlsx("data/raw-data/quarter_to_month_crosswalk.xlsx")

hpi_state_month <- hpi_state %>%
  left_join(
    quarter_to_month,
    by = "quarter",
    relationship = "many-to-many"
  ) 

rm(quarter_to_month)

hpi_clean <- hpi_state_month %>%
  mutate(
    date = make_date(year, month, 1)
  ) %>%
  filter(
    date >= date_lb,
    date <= date_ub
  ) %>%
  select(
    -state_abb,
    -quarter,
    -month_name
  )

rm(hpi, hpi_state, hpi_state_month)


# 3. Demographic controls - Annual

# Load ACS data 
acs <- read_dta("data/raw-data/usa_00019.dta") 

# Restrict to working age individuals: Age 16-64
n_drop_age <- acs %>%
  mutate(
    age = as.numeric(age)
  ) %>%
  filter(age < 16 | age > 64) %>%
  summarise(n = n())
n_drop_age

acs <- acs %>%
  mutate(
    year = as.numeric(year),
    statefip = as.numeric(statefip)
  ) %>%
  filter(
    age >= 16,
    age <= 64
  )

# Code up indicators 
acs <- acs %>%
  mutate(
    male = (sex == 1), # male indicator
    black = (race == 2), # black indicator
    college = (educ %in% c(10, 11)) # college indicator (4+ years of college)
  ) %>%
  select(
    year,
    statefip,
    perwt,
    age,
    male, 
    black,
    college
  ) 

# Fraction male, black, and college, and mean age, all with ACS weights 
acs_clean <- acs %>%
  group_by(statefip, year) %>%
  summarise(
    frac_male = weighted.mean(male, perwt), 
    frac_black = weighted.mean(black, perwt),
    frac_college = weighted.mean(college, perwt),
    mean_age = weighted.mean(age, perwt),
    .groups = "drop"
  ) 

acs_clean <- acs_clean %>%
  rename(state = "statefip")

rm(acs)



# 4. Merge all covariates together 

# BEA to BLS 
bea_bls <- bea_clean %>%
  full_join(
    bls_clean,
    by = c("state", "date")
  ) %>%
  select(
    -ends_with(".y")
  ) %>%
  rename(
    state_name = "state_name.x",
    year = "year.x",
    month = "month.x"
  )

# BEA + BLS to HPI
bea_bls_hpi <- bea_bls %>%
  full_join(
    hpi_clean,
    by = c("state_name", "date")
  ) %>%
  select(
    -ends_with(".y")
  ) %>%
  rename(
    year = "year.x",
    month = "month.x"
  )

rm(bea_bls)

# BEA + BLS + HPI to ACS
covariates <- bea_bls_hpi %>%
  left_join(
    acs_clean,
    by = c("state", "year"),
    relationship = "many-to-many"
  )


# 5. Convert the above variables to real measures using CPI 
# NOTE: Keep the CPI deflator around to use to convert Lightcast variables.
covariates_cpi <- covariates %>%
  left_join(
    cpi_clean,
    by = "date",
    relationship = "many-to-many"
  ) %>%
  select(
    -ends_with(".y")
  ) %>%
  rename(
    year = "year.x",
    month = "month.x"
  )

rm(covariates)

covariates_clean <- covariates_cpi %>%
  mutate(
    real_income = income * cpi_deflator,
    real_hpi = index_nsa * cpi_deflator
  ) %>%
  select(
    -income,
    -index_nsa,
    -cpi # but keeping the deflator
  )

rm(acs_clean, bea_bls_hpi, bea_clean, bls_clean, covariates_cpi, cpi_clean,
   hpi_clean)



# Baseline version 
base_year <- 2022 # year before MN ban

covariates_base <- covariates_clean %>%
  filter(year == base_year) %>%
  group_by(state) %>%
  summarise(
    unemp_rate_base = mean(unemp_rate, na.rm = TRUE),
    real_income_base = mean(real_income, na.rm = TRUE),
    real_hpi_base = mean(real_hpi, na.rm = TRUE),
    frac_male_base = mean(frac_male, na.rm = TRUE),
    frac_black_base = mean(frac_black, na.rm = TRUE),
    frac_college_base = mean(frac_college, na.rm = TRUE),
    mean_age_base = mean(mean_age, na.rm = TRUE),
    .groups = "drop"
  )

# Merge baseline version back into main version
covariates_clean <- covariates_clean %>%
  left_join(
    covariates_base,
    by = "state",
    relationship = "many-to-one"
  )


# 6. Save csv file "covariates.csv" 
write_csv(covariates_clean, "data/analysis-data/covariates.csv") # time varying 

# NOTE: Make everything baseline in the prep sample files that way I can vary
# the base year depending on what analysis I'm doing. Also, I may want to 
# run robustness checks where I treat these covariates as outcomes.  
