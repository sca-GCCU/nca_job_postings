##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "0b_main_cluster.R" 
# by: Sebastian C. Anastasi
# Date of this version: April 3, 2026
#
# Description: This script runs all of the analysis that takes place in the
# Palmetto Cluster.
#
# Dependencies:
#
# Output: 
##############################################################################

rm(list = ls())

# NOTE: echo = TRUE can be used inside source to create a readable log of the 
# code I ran and the outputs. 

# Prep Main Data
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("3a_prep_covariates.R", echo = TRUE, max.deparse.length = 1000)
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("3b_prep_agg1_mn.R", echo = TRUE, max.deparse.length = 1000)
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("3c_prep_agg2_mn.R", echo = TRUE, max.deparse.length = 1000)
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("3d_prep_salary_mn.R", echo = TRUE, max.deparse.length = 1000)

# Prep Placebo Data 
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("3e_prep_agg1_placebo_mn.R", echo = TRUE, max.deparse.length = 1000)
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("3f_prep_agg2_placebo_mn.R", echo = TRUE, max.deparse.length = 1000)
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("3g_prep_salary_placebo_mn.R", echo = TRUE, max.deparse.length = 1000)

# Summary Statistics 
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("4a_sumstats_agg1_mn.R", echo = TRUE, max.deparse.length = 1000)
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("4b_sumstats_agg2_mn.R", echo = TRUE, max.deparse.length = 1000)
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("4c_sumstats_salary_mn.R", echo = TRUE, max.deparse.length = 1000)

# Descriptive Graphs 
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("5a_descriptive_graphs_agg1_mn.R", echo = TRUE, max.deparse.length = 1000)
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("5b_descriptive_graphs_salary_mn.R", echo = TRUE, max.deparse.length = 1000)

# TWFE 
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("6a_twfe_agg1_mn.R", echo = TRUE, max.deparse.length = 1000)
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("6b_twfe_agg2_mn.R", echo = TRUE, max.deparse.length = 1000)
setwd("C:/Users/scana/Desktop/Github_projects/nca_job_postings/programs")
source("6c_twfe_salary_mn.R", echo = TRUE, max.deparse.length = 1000)


