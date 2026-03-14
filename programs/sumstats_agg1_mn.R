##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "sumstats_agg1_mn" 
# by: Sebastian C. Anastasi
# Date of this version: March 6, 2026
#
# Description: Creates the occupation-state-month level sumstats. 
#
# Dependencies: 
#
# Output: 
##############################################################################

# NOTE: When run in the cluster, I will need to update paths and (I believe)
# install the appropriate packages. 

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

library(tidyverse)

# Load analysis data 
agg1_mn_analysis <- read_csv("data/analysis-data/agg1_mn_analysis.csv")


# Create table of means and standard deviations by treatment status 


