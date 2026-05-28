##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "3h_prep_data_all_bans.R" 
# by: Sebastian C. Anastasi
# Date of this version: May 28, 2026
#
# Description:  
#
# Dependencies:  
#
# Output:
##############################################################################

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

library(readr)
library(tidyr)
library(dplyr)

# -------------------- Import & Preliminary Restrictions -----------------------
# --- Import Lightcast data ---

agg2 <- read_csv("data/raw-data/sample_anastasi_agg2_v2.csv")

n_start <- agg2 %>%
  summarise(N = n()) %>%
  pull(N)
n_start
# NOTE: Ignore writing of output to output files for now.

# --- Restrict to firms with at least 10 total listings --- 
agg2 <- agg2 %>%
  group_by(company) %>%
  mutate(
    total_postings_ever = sum(total_postings, na.rm = TRUE)
  ) %>%
  ungroup()

agg2 <- agg2 %>%
  mutate(
    drop_min_ads = (total_postings_ever < 10)
  )


# ------------------- Merge Treatment Data ------------------------------------- 
# --- Import treatment panel ---

# --- Keep only necessary variables --- 




# ----------------- Impose Sample Restrictions ---------------------------------









