# Generate Sample Versions of ACS Data 
# Name: acs_sample_generation.R
# Date: 7-20-26

rm(list = ls())
gc()

# local
setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")
# cluster
#setwd("/home/scanast/nca_job_postings") 

library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(data.table)

# Load new ACS data: "usa_0020.csv" 
acs <- read_csv("data/raw-data/usa_00020.csv") 

set.seed(123)

acs_1pct <- acs %>%
  group_by(STATEFIP, YEAR) %>%
  slice_sample(prop = 0.01) %>%
  ungroup()

write_csv(
  acs_1pct,
  "data/raw-data/usa_00020_1pct.csv"
)

acs_5pct <- acs %>%
  group_by(STATEFIP, YEAR) %>%
  slice_sample(prop = 0.05) %>%
  ungroup()

write_csv(
  acs_5pct,
  "data/raw-data/usa_00020_5pct.csv"
)

