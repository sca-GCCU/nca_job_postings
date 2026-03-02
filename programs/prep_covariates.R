##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "prep_covariates" 
# by: Sebastian C. Anastasi
# Date of this version: March 2, 2026
#
# Description: This script prepares the monthly covariates. 
##############################################################################

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

library(tidyverse)
library(readxl)
library(lubridate)

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

# NOTE: Be aware that October 2025 data is missing (perhaps we could interpolate
# it if necessary in future) and much of the 2026 is missing. 


# 2. Economic controls

# A. Uemployment 
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


# B. Earnings 
# NOTE: BEA interactive data, nominal/current dollars "SQINC1 State quarterly  
# personal income summary: personal income, population, per capita personal  
# income" (https://www.bea.gov/data/income-saving/personal-income-by-state).

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
  )

rm(bea_monthly)


# C. Housing Price Index


# D. Convert the above variables to real measures using CPI 
# NOTE: Keep the CPI deflator around to use to convert Lightcast variables.


# 3. Demographic controls 
# A. Percent Male 

# B. Percent Black 

# C. Average Age 


# 4. Save csv file "covariates.csv" 


# NOTE: Make everything baseline in the prep sample files that way I can vary
# the base year depending on what analysis I'm doing. Also, I may want to 
# run robustness checks where I treat these covariates as outcomes.  
