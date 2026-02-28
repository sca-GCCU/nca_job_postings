##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "prep_ind_ban_codes" 
# by: Sebastian C. Anastasi
# Date of this version: February 27, 2026
#
# Description: This script identifies SOC-4 and NAICS-4 codes affected by 
# industry/occupation-based bans to be excluded from analysis sample. 
##############################################################################

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

library(tidyverse)

# Identify all states with industry-based bans (ban_ind and ban_health). 


# Identify affected SOC-4 and NAICS-4 codes by state. For each state, store 
# a vector of affected SOC-4 and NAICS-4 codes. These can then be combined into
# a two dataframes (one for SOC and one for FIPS) where the variable/column
# names are the state names and the values are the affected codes.

