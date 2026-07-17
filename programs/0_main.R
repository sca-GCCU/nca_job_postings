##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "0_main.R" 
# by: Sebastian C. Anastasi
# Date of this version: July 17, 2026
#
# Description: This script runs all of the analysis in the Palmetto Cluster.
#
# Dependencies: "1_map_state_laws.R", "2_table_state_laws.R"
#
# Output: 
##############################################################################

# NOTE: PRIOR TO ANY RUN, CHECK THAT ALL FILE NAMES ARE UPDATED TO THE FULL 
# SAMPLE VERSIONS!!!

# ---------------------------- HOUSEKEEPING ------------------------------------
rm(list = ls())
gc()

setwd("/home/scanast/nca_job_postings") # for cluster run


# ---------------------- GENERAL BACKGROUND ANALYSIS ---------------------------
# 1. Map of NCA Bans 
source("programs/1_map_state_laws.R", echo = TRUE, max.deparse.length = 1000)

# 2. Table(s) of State Laws
source("programs/2_table_state_laws.R", echo = TRUE, max.deparse.length = 1000)


# ------------------------ MINNESOTA BAN ANALYSIS ------------------------------
# NOTE: echo = TRUE can be used inside source to create a readable log of the 
# code I ran and the outputs. 

# 3.A. Prep Main Data
source("programs/3a_prep_covariates.R", echo = TRUE, max.deparse.length = 1000)
source("programs/3b_prep_agg1_mn.R", echo = TRUE, max.deparse.length = 1000)
source("programs/3c_prep_agg2_mn.R", echo = TRUE, max.deparse.length = 1000)
source("programs/3d_prep_salary_mn.R", echo = TRUE, max.deparse.length = 1000)

# 3.B. Prep Placebo Data 
source("programs/3e_prep_agg1_placebo_mn.R", echo = TRUE, max.deparse.length = 1000)
source("programs/3f_prep_agg2_placebo_mn.R", echo = TRUE, max.deparse.length = 1000)
source("programs/3g_prep_salary_placebo_mn.R", echo = TRUE, max.deparse.length = 1000)

# 4. Summary Statistics 
source("programs/4a_sumstats_agg1_mn.R", echo = TRUE, max.deparse.length = 1000)
source("programs/4b_sumstats_agg2_mn.R", echo = TRUE, max.deparse.length = 1000)
source("programs/4c_sumstats_salary_mn.R", echo = TRUE, max.deparse.length = 1000)

# 5. Descriptive Graphs 
source("programs/5a_descriptive_graphs_agg1_mn.R", echo = TRUE, max.deparse.length = 1000)
source("programs/5b_descriptive_graphs_salary_mn.R", echo = TRUE, max.deparse.length = 1000)

# 6. TWFE 
source("programs/6a_twfe_agg1_mn.R", echo = TRUE, max.deparse.length = 1000)
source("programs/6b_twfe_agg2_mn.R", echo = TRUE, max.deparse.length = 1000)
source("programs/6c_twfe_salary_mn.R", echo = TRUE, max.deparse.length = 1000)

# 7. SCM Analysis 
source("programs/7a_scm_agg1_mn.R", echo = TRUE, max.deparse.length = 1000)

# 8. SDID Analysis
source("programs/8a_sdid_mn.R", echo = TRUE, max.deparse.length = 1000)


# --------------------- STAGGERED NCA BAN ANALYSIS -----------------------------
# 3.Prep Data 
# firm-soc-state-year (fab)
source("programs/3h_prep_data_fab.R", echo = TRUE, max.deparse.length = 1000)
# soc-state-year (sab)
source("programs/3i_prep_data_sab.R", echo = TRUE, max.deparse.length = 1000)

# 5. Descriptive Graphs 
source("programs/5d_descriptive_graphs_ab.R", echo = TRUE, max.deparse.length = 1000)

# 9. CS DID Analysis 
source("programs/9a_cs_did_fab.R", echo = TRUE, max.deparse.length = 1000)
source("programs/9b_cs_did_sab.R", echo = TRUE, max.deparse.length = 1000)


