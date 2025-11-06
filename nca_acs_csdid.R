# PREAMBLE ---------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(did)
library(haven)
library(srvyr)
library(survey)
library(purrr)
library(readr)
library(tibble)
library(rlang)

set.seed(1390)

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/data")


# LOAD THE DATA ----------------------------------------------------------------

# VECTORS ----------------------------------------------------------------------
outcome_vars <- c("age", "early_career","mid_career", "late_career")
control_vars <- c("employment_sa_l1","inc_pcap_r_l1","hpi_r_l1")
combined_vars <- c(outcome_vars, control_vars)



acs <- read_dta("nca_acs_soc.dta")

csdid_age <- att_gt(
  yname = "age",
  tname = "year", 
  gname = "year_eff_ban",
  panel = FALSE, 
  bstrap = TRUE,
  cband = TRUE, 
  clustervars = "statefip",
  weightsname = "perwt",
  data = acs
)







