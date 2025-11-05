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







