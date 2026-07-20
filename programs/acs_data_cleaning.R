# ACS Data Restrictions and Cleaning 
# Name: acs_data_cleaning 
# Date: 7-20-26

# ------------------------------- HOUSEKEEPING ---------------------------------
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


# ----------------------------- LOAD DATA --------------------------------------
acs_1pct <- read_csv("data/raw-data/usa_00020_1pct.csv")

n_start <- nrow(acs_1pct)

acs_1pct <- acs_1pct %>%
  filter(AGE >= 16, AGE <= 65)

n_drop_age_2 <- n_start - nrow(acs_1pct)



# ---------------------------- SAVE DATA ---------------------------------------
write_csv(
  acs_1pct,
  "data/analysis-data/acs_analysis.csv"
)


