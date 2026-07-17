##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "0a_main_non_cluster.R" 
# by: Sebastian C. Anastasi
# Date of this version: April 3, 2026
#
# Description: This script runs all of the analysis that doesn't take place in
# the Palmetto Cluster.
#
# Dependencies: "1_map_state_laws.R", "2_table_state_laws.R"
#
# Output: 
##############################################################################

rm(list = ls())

# NOTE: echo = TRUE can be used inside source to create a readable log of the 
# code I ran and the outputs. 

# Generate map of NCA laws
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("1_map_state_laws.R", echo = TRUE, max.deparse.length = 1000) 

# Generate tables of NCA laws  
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("2_table_state_laws.R", echo = TRUE, max.deparse.length = 1000)

