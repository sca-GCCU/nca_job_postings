# PREAMBLE ---------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(did)
library(haven)
library(srvyr)

set.seed(1390)

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/data")

# LOAD DATA --------------------------------------------------------------------

nca_acs_soc_1pct <- read_dta("nca_acs_soc_1pct.dta")

# SUMMARY STATISTICS -----------------------------------------------------------

outcome_vars <- c("age", "pot_exp", "young_adult", "earlyc_adult", "mlc_adult", 
              "older_adult", "yrschool", "no_high_school", "high_school",
              "some_college", "college")

control_vars <- c("employment_sa", "income_pcap", "hpi", "male", "black")

out_con_vars <- c(outcome_vars, control_vars)



# WELCH TESTS ------------------------------------------------------------------




# CS DID -----------------------------------------------------------------------

