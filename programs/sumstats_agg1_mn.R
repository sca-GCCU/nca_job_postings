##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "sumstats_agg1_mn" 
# by: Sebastian C. Anastasi
# Date of this version: March 6, 2026
#
# Description: This script prepares the occupation-state-month level sumstats  
# data for analyzing Minnesota's full noncompete ban. 
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

# 1. Load analysis data 
agg1_mn_analysis <- read_csv("data/analysis-data/agg1_mn_analysis.csv")


# 2. Create plots of raw means by treatment status 

# Total Postings 

# Any Education 
# Count
# Share


# Bachelor's 
# Count
# Share


# Master's
# Count
# Share


# Doctorate 
# Count
# Share


# Any Experience 
# Count
# Share


# Average Experience


# Full Time
# Count
# Share


# Part Time 
# Count
# Share


# Internship
# Count
# Share


# Average Salary 




# 3. Create table of means and standard deviations by treatment status 


