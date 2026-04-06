##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "3a_prep_covariates.R" 
# by: Sebastian C. Anastasi
# Date of this version: April 6, 2026
#
# Description: This script prepares the covariates. 
#
# Outputs: "acs_clean.csv," "bea_a_clean.csv," "bea_m_clean.csv," 
# "bls_a_clean.csv," "bls_m_clean.csv," "covariates_a_clean.csv,"
# "covariates_m_clean.csv," "cpi_clean.csv," and "hpi_clean.csv" 
##############################################################################

# NOTE: When run in the cluster, I will need to update paths and (I believe)
# install the appropriate packages. 

# Clear environment
rm(list = ls())

# Load path helper 
home <- path.expand("~")
proj_root <- file.path(home, "nca_job_postings")
programs_dir <- file.path(proj_root, "programs")
source(file.path(programs_dir, "0c_paths.R"))

# Load packages 
library(tidyverse)
library(readxl)
library(lubridate)
library(haven)


# Lightcast data range (used in multiple steps below)
date_lb <- make_date(2010, 1, 1)
date_ub <- make_date(2025, 1, 1)
year_lb <- 2010
year_ub <- 2025

# 1.A CPI data for deflating variables 
# NOTE: The exact dataset ("Historical CPI-U, January 2026") is found here: 
# https://www.bls.gov/cpi/tables/supplemental-files/. CPI-U is the CPI for 
# "all urban consumers."  

cpi <- read_xlsx(
  file.path(data_raw, "historical-cpi-u-202601.xlsx"), 
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
  filter(date >= 2008) %>% # to convert OR's 2008 inc_threshold to real dollars
  filter(date <= date_ub)

rm(cpi_long, base_cpi, base_period)

write_csv(cpi_clean, file.path(data_clean, "cpi_clean.csv"))

# NOTE: Be aware that October 2025 data is missing (perhaps we could interpolate
# it if necessary in future) and much of the 2026 is missing. 


# 2. Economic controls

# A.1 Unemployment - Monthly 
# NOTE: Using "Employment status of the civilian noninstitutional population, 
# not seasonally adjusted" (https://www.bls.gov/lau/rdscnp16.htm), from the BLS. 

bls_m <- read_xlsx(
  file.path(data_raw, "ststdnsadata.xlsx"), 
  range = "A9:K31808",
  na = "–",
  col_names = FALSE, 
  trim_ws = TRUE
)

bls_m <- bls_m %>%
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

bls_m_clean <- bls_m %>%
  mutate(
    date = make_date(year, month, 1)
  ) %>%
  filter(date >= date_lb) %>%
  filter(date <= date_ub) %>%
  filter(!(state_name %in% c("Los Angeles County", "New York city"))) # non-state FIPS

rm(bls_m)

write_csv(bls_m_clean, file.path(data_clean, "bls_m_clean.csv"))


# A.2 Unemployment - Annual (only goes up to 2024)
# NOTE: Using "Employment status of the civilian noninstitutional population, 
# annual averages" (https://www.bls.gov/lau/rdscnp16.htm), from the BLS. 

bls_a <- read_xlsx(
  file.path(data_raw, "staadata.xlsx"),
  range = "A9:J2605",
  col_names = FALSE,
  trim_ws = TRUE
)

bls_a <- bls_a %>%
  select(
    ...1:...3, 
    ...10
  ) %>%
  rename(
    state = "...1",
    state_name = "...2",
    year = "...3",
    unemp_rate = "...10"
  ) %>%
  mutate(
    state = as.numeric(state),
    year = as.numeric(year)
  )

bls_a_clean <- bls_a %>%
  filter(year >= year_lb) %>%
  filter(year <= year_ub) %>%
  filter(!(state_name %in% c("Los Angeles County", "New York city"))) # non-state FIPS

rm(bls_a)

write_csv(bls_a_clean, file.path(data_clean, "bls_a_clean.csv"))



# B.1 Per Capita Personal Income - Monthly
# NOTE: BEA interactive data, nominal/current dollars "SQINC1 State quarterly  
# personal income summary: personal income, population, per capita personal  
# income" (https://www.bea.gov/data/income-saving/personal-income-by-state).

bea <- read.csv(
  file.path(data_raw, "Table_monthly.csv"),
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
quarter_to_month <- read_xlsx(file.path(data_raw, "quarter_to_month_crosswalk.xlsx"))

bea_m <- bea_long %>%
  left_join(
    quarter_to_month,
    by = "quarter",
    relationship = "many-to-many"
  )

rm(quarter_to_month, bea_long)

bea_m_clean <- bea_m %>%
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

rm(bea_m)

write_csv(bea_m_clean, file.path(data_clean, "bea_m_clean.csv"))


# B.2 Per Capita Personal Income - Annual (only goes up to 2024)
# NOTE: BEA interactive data, nominal/current dollars "SAINC1 - State annual 
#personal income summary: personal income, population, per capita personal 
#income" (https://www.bea.gov/data/income-saving/personal-income-by-state).

bea_a <- read.csv(
  file.path(data_raw, "Table_annual.csv"),
  header = TRUE, 
  skip = 3,
  nrows = 51
)

bea_a_long <- bea_a %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "year",
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

rm(bea_a)

bea_a_clean <- bea_a_long %>%
  mutate(
    year = sub("^X", "", year)
  ) %>%
  mutate(
    year = as.integer(year)
  )

rm(bea_a_long)

write_csv(bea_a_clean, file.path(data_clean, "bea_a_clean.csv"))



# C. Housing Price Index - Monthly
# NOTE: U.S. Federal Housing Finance Agency (FHFA) House Price Index Datasets, 
# "Master HPI Data" (https://www.fhfa.gov/data/hpi/datasets?tab=master-hpi-data).
# State data is only available on a quarterly basis (not monthly or annual). 

hpi <- read.csv(file.path(data_raw, "hpi_master.csv"), header = TRUE)

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
quarter_to_month <- read_xlsx(file.path(data_raw, "quarter_to_month_crosswalk.xlsx"))

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

write_csv(hpi_clean, file.path(data_clean, "hpi_clean.csv"))


# 3. Demographic controls - Annual

# Load ACS data 
acs <- read_dta(file.path(data_raw, "usa_00019.dta")) 

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

write_csv(acs_clean, file.path(data_clean, "acs_clean.csv"))



# 4.1 Merge all covariates together - Monthly 

# BEA to BLS 
bea_bls_m <- bea_m_clean %>%
  full_join(
    bls_m_clean,
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

# (BEA + BLS) to HPI
bea_bls_hpi_m <- bea_bls_m %>%
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

rm(bea_bls_m)

# (BEA + BLS + HPI) to ACS
covariates_m <- bea_bls_hpi_m %>%
  left_join(
    acs_clean,
    by = c("state", "year"),
    relationship = "many-to-one"
  )

rm(bea_bls_hpi_m)

# Convert the above variables to real measures using CPI 
# NOTE: Don't need to keep deflator here because I can load cpi_clean.csv to 
# convert Lightcast variables.
covariates_cpi_m <- covariates_m %>%
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

rm(covariates_m)

covariates_m_clean <- covariates_cpi_m %>%
  mutate(
    real_income = income * cpi_deflator,
    real_hpi = index_nsa * cpi_deflator
  ) %>%
  select(
    -income,
    -index_nsa,
    -starts_with("cpi")
  )

rm(covariates_cpi_m)

write_csv(covariates_m_clean, file.path(data_clean, "covariates_m_clean.csv"))

rm(bea_m_clean, bls_m_clean, covariates_m_clean)


# 4.2 Merge all covariates together - Annual

# i. Transform the HPI measure to annual averages 
hpi_a_clean <- hpi_clean %>%
  select(
    -month,
    -date
  ) %>%
  group_by(state_name, year) %>%
  summarise(
    hpi = mean(index_nsa, na.rm = TRUE),
    .groups = "drop" 
  )

rm(hpi_clean)

# ii. Merge
# BEA to BLS

bea_bls_a <- bea_a_clean %>%
  full_join(
    bls_a_clean,
    by = c("state", "year")
  ) %>%
  select(
    -ends_with(".y")
  ) %>%
  rename(
    state_name = state_name.x
  )

# (BEA + BLS) to HPI
bea_bls_hpi_a <- bea_bls_a %>%
  left_join(
    hpi_a_clean,
    by = c("state_name", "year")
  )

rm(bea_bls_a)

# (BEA + BLS + HPI) to ACS
covariates_a <- bea_bls_hpi_a %>%
  full_join(
    acs_clean,
    by = c("state", "year")
  )

rm(bea_bls_hpi_a)


# iii. Covert to Real
# Create annual version of CPI data
cpi_a_clean <- cpi_clean %>%
  group_by(year) %>%
  summarise(
    mean_cpi = mean(cpi, na.rm = TRUE),
    mean_cpi_deflator = mean(cpi_deflator, na.rm = TRUE),
    .groups = "drop"
  )

covariates_cpi_a <- covariates_a %>%
  left_join(
    cpi_a_clean,
    by = "year",
    relationship = "many-to-one"
  )

rm(covariates_a)

# Perform conversion
covariates_a_clean <- covariates_cpi_a %>%
  mutate(
    real_income = income * mean_cpi_deflator,
    real_hpi = hpi * mean_cpi_deflator
  ) %>%
  select(
    -income,
    -hpi,
    -mean_cpi,
    -mean_cpi_deflator
  )

rm(covariates_cpi_a)

write_csv(covariates_a_clean, file.path(data_clean, "covariates_a_clean.csv")) 



# NOTE: Make everything baseline in the analysis-data prep, NOT HERE.