##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "0b_main_cluster.R" 
# by: Sebastian C. Anastasi
# Date of this version: April 6, 2026
#
# Description: This script runs all of the analysis that takes place in the
# Palmetto Cluster.
#
# Dependencies: "0b_packages.R", "0c_paths.R"
#
# Output: 
##############################################################################

# 1. Clear environment 
rm(list = ls())

# 2. Load project paths
home <- path.expand("~") # home directory 
proj_root <- file.path(home, "nca_job_postings") # project root 
programs_dir    <- file.path(proj_root, "programs") # code directory
source(file.path(programs_dir, "0c_paths.R")) # run paths helper 

# 3. Install packages (COMMENT OUT AFTER FIRST RUN)
source(file.path(programs_dir, "0b_packages.R")) 

# NOTE: echo = TRUE can be used inside source to create a readable log of the 
# code I ran and the outputs. 

# 4. Prep Main Data
source(file.path(programs_dir, "3a_prep_covariates.R"), echo = TRUE, max.deparse.length = 1000)
source(file.path(programs_dir, "3b_prep_agg1_mn.R"), echo = TRUE, max.deparse.length = 1000)
source(file.path(programs_dir, "3c_prep_agg2_mn.R"), echo = TRUE, max.deparse.length = 1000)
source(file.path(programs_dir, "3d_prep_salary_mn.R"), echo = TRUE, max.deparse.length = 1000)

# 5. Prep Placebo Data 
source(file.path(programs_dir, "3e_prep_agg1_placebo_mn.R"), echo = TRUE, max.deparse.length = 1000)
source(file.path(programs_dir, "3f_prep_agg2_placebo_mn.R"), echo = TRUE, max.deparse.length = 1000)
source(file.path(programs_dir, "3g_prep_salary_placebo_mn.R"), echo = TRUE, max.deparse.length = 1000)

# 6. Summary Statistics 
source(file.path(programs_dir, "4a_sumstats_agg1_mn.R"), echo = TRUE, max.deparse.length = 1000)
source(file.path(programs_dir, "4b_sumstats_agg2_mn.R"), echo = TRUE, max.deparse.length = 1000)
source(file.path(programs_dir, "4c_sumstats_salary_mn.R"), echo = TRUE, max.deparse.length = 1000)

# 7. Descriptive Graphs 
source(file.path(programs_dir, "5a_descriptive_graphs_agg1_mn.R"), echo = TRUE, max.deparse.length = 1000)
source(file.path(programs_dir, "5b_descriptive_graphs_salary_mn.R"), echo = TRUE, max.deparse.length = 1000)

# 8. TWFE 
source(file.path(programs_dir, "6a_twfe_agg1_mn.R"), echo = TRUE, max.deparse.length = 1000)
source(file.path(programs_dir, "6b_twfe_agg2_mn.R"), echo = TRUE, max.deparse.length = 1000)
source(file.path(programs_dir, "6c_twfe_salary_mn.R"), echo = TRUE, max.deparse.length = 1000)


