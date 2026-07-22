##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "9a_cs_did_fab.R" 
# by: Sebastian C. Anastasi
# Date of this version: July 16, 2026
#
# Description: Runs CS DID analysis of states staggered noncompete bans for the
# firm-soc-state-year level sample.
#
# Dependencies:  
#
# Output:
##############################################################################

# CHANGE NUMBER OF CORES AND PARALLELIZATION BEFORE EACH RUN!!! 

# ---------------------------- HOUSEKEEPING ------------------------------------
rm(list = ls())
gc()

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")
#setwd("/home/scanast/nca_job_postings") # for cluster run

library(did)        # Callaway & Sant'Anna estimator 
library(fixest)     # High-performance fixed-effects regression (TWFE comparison)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

set.seed(87) # for reproducibility with bootstrap SEs 

par_proc = FALSE # for local runs 
#par_proc = TRUE # for parallel processing on cluster  
n_cores = 1 # for local runs 
#n_cores = 4 # for parallel processing on cluster


# ----------------------------- LOAD DATA --------------------------------------
df_firm <- read_csv("data/analysis-data/agg2_fab_analysis.csv")


# -------------------- CS DID ANALYSIS (UNCONDITIONAL) -------------------------
# --- Total Postings --- 
# NOTE: Unbalanced panel version (repeated cross-sections)
cs_tot_post <- att_gt(
  yname = "total_postings_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE, # makes this repeated cross-sections
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_tot_post)
csdid_cohort_tot_post <- ggdid(
  cs_tot_post, 
  xlab = "Year", 
  ylab = "Total Postings", 
  title = "",
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_tot_post_fab.pdf",
  csdid_cohort_tot_post,
  width = 8,
  height = 8,
  units = "in"
)

es_tot_post <- aggte(
  cs_tot_post,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_tot_post)
csdid_es_tot_post <- ggdid(
  es_tot_post, 
  xlab = "Event Time", 
  ylab = "Total Postings",
  title = " "
)
ggsave(
  "output/figures/csdid_es_tot_post_fab.pdf",
  csdid_es_tot_post,
  width = 7,
  height = 5,
  units = "in"
)

rm(cs_tot_post, csdid_cohort_tot_post, es_tot_post, csdid_es_tot_post)
gc()


# --- Any Experience --- 

cs_any_exp <- att_gt(
  yname = "any_exp_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_any_exp)
csdid_cohort_exp <- ggdid(
  cs_any_exp, 
  xlab = "Year",
  ylab = "Experience Required (Postings)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_exp_fab.pdf",
  csdid_cohort_exp,
  width = 8,
  height = 8,
  units = "in"
)

es_any_exp <- aggte(
  cs_any_exp,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_any_exp)
csdid_es_exp <- ggdid(
  es_any_exp, 
  xlab = "Event Time",
  ylab = "Experience Required (Postings)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_exp_fab.pdf",
  csdid_es_exp,
  width = 7,
  height = 5,
  units = "in"
)

rm(cs_any_exp, csdid_cohort_exp, es_any_exp, csdid_es_exp)
gc()


# --- Share Experience --- 

cs_share_exp <- att_gt(
  yname = "share_exp",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_share_exp)
csdid_cohort_exp_share <- ggdid(
  cs_share_exp, 
  xlab = "Year",
  ylab = "Experience Required (Share)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_exp_share_fab.pdf",
  csdid_cohort_exp_share,
  height = 8,
  width = 8,
  units = "in"
)

es_share_exp <- aggte(
  cs_share_exp,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_share_exp)
csdid_es_exp_share <- ggdid(
  es_share_exp, 
  xlab = "Event Time",
  ylab = "Experience Required (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_exp_share_fab.pdf",
  csdid_es_exp_share,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_share_exp, csdid_cohort_exp_share, es_share_exp, csdid_es_exp_share)
gc()


# --- Average Experience --- 

cs_ave_exp <- att_gt(
  yname = "ave_exp_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_ave_exp)
csdid_cohort_exp_ave <- ggdid(
  cs_ave_exp, 
  xlab = "Year",
  ylab = "Experience Required (Average)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_exp_ave_fab.pdf",
  csdid_cohort_exp_ave,
  height = 8,
  width = 8,
  units = "in"
)

es_ave_exp <- aggte(
  cs_ave_exp,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_ave_exp)
csdid_es_exp_ave <- ggdid(
  es_ave_exp, 
  xlab = "Event Time",
  ylab = "Experience Required (Average)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_exp_ave_fab.pdf",
  csdid_es_exp_ave,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_ave_exp, csdid_cohort_exp_ave, es_ave_exp, csdid_es_exp_ave)
gc()


# --- Any Bachelor's Degree ---- 

cs_bachelor <- att_gt(
  yname = "bachelor_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_bachelor)
csdid_cohort_bachelor <- ggdid(
  cs_bachelor,
  xlab = "Year",
  ylab = "Bachelor's Required (Postings)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_bachelor_fab.pdf",
  csdid_cohort_bachelor,
  height = 8,
  width = 8,
  units = "in"
)

es_bachelor <- aggte(
  cs_bachelor,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_bachelor)
csdid_es_bachelor <- ggdid(
  es_bachelor, 
  xlab = "Event Time",
  ylab = "Bachelor's Required (Postings)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_bachelor_fab.pdf",
  csdid_es_bachelor,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_bachelor, csdid_cohort_bachelor, es_bachelor, csdid_es_bachelor)
gc()


# --- Share Bachelor's Degree ---- 

cs_bachelor_share <- att_gt(
  yname = "share_bachelor",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_bachelor_share)
csdid_cohort_bachelor_share <- ggdid(
  cs_bachelor_share, 
  xlab = "Year",
  ylab = "Bachelor's Required (Share)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_bachelor_share_fab.pdf",
  csdid_cohort_bachelor_share,
  height = 8,
  width = 8,
  units = "in"
)

es_bachelor_share <- aggte(
  cs_bachelor_share,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_bachelor_share)
csdid_es_bachelor_share <- ggdid(
  es_bachelor_share, 
  xlab = "Event Time",
  ylab = "Bachelor's Required (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_bachelor_share_fab.pdf",
  csdid_es_bachelor_share,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_bachelor_share, 
   csdid_cohort_bachelor_share, 
   es_bachelor_share, 
   csdid_es_bachelor_share)
gc()


# --- Fulltime --- 

cs_fulltime <- att_gt(
  yname = "fulltime_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_fulltime)
csdid_cohort_fulltime <- ggdid(
  cs_fulltime, 
  xlab = "Year",
  ylab = "Full-Time Postings",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_fulltime_fab.pdf",
  csdid_cohort_fulltime,
  height = 8,
  width = 8,
  units = "in"
)

es_fulltime <- aggte(
  cs_fulltime,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_fulltime)
csdid_es_fulltime <- ggdid(
  es_fulltime, 
  xlab = "Event Time",
  ylab = "Full-Time Postings",
  title = " "
)
ggsave(
  "output/figures/csdid_es_fulltime_fab.pdf",
  csdid_es_fulltime,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_fulltime, csdid_cohort_fulltime, es_fulltime, csdid_es_fulltime)
gc()


# --- Fulltime (Share) --- 

cs_fulltime_share <- att_gt(
  yname = "share_fulltime",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_fulltime_share)
csdid_cohort_fulltime_share <- ggdid(
  cs_fulltime_share, 
  xlab = "Year",
  ylab = "Full-Time Postings (Share)",
  title = " ", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_fulltime_share_fab.pdf",
  csdid_cohort_fulltime_share,
  height = 8,
  width = 8,
  units = "in"
)

es_fulltime_share <- aggte(
  cs_fulltime_share,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_fulltime_share)
csdid_es_fulltime_share <- ggdid(
  es_fulltime_share, 
  xlab = "Event Time",
  ylab = "Full-Time Postings (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_fulltime_share_fab.pdf",
  csdid_es_fulltime_share,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_fulltime_share, 
   csdid_cohort_fulltime_share, 
   es_fulltime_share, 
   csdid_es_fulltime_share)
gc()


# --- Parttime --- 

cs_parttime <- att_gt(
  yname = "parttime_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_parttime)
csdid_cohort_parttime <- ggdid(
  cs_parttime, 
  xlab = "Year",
  ylab = "Part-Time Postings",
  title = " ", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_parttime_fab.pdf",
  csdid_cohort_parttime,
  height = 8,
  width = 8, 
  units = "in"
)

es_parttime <- aggte(
  cs_parttime,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_parttime)
csdid_es_parttime <- ggdid(
  es_parttime, 
  xlab = "Event Time",
  ylab = "Part-Time Postings",
  title = " "
)
ggsave(
  "output/figures/csdid_es_parttime_fab.pdf",
  csdid_es_parttime,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_parttime, csdid_cohort_parttime, es_parttime, csdid_es_parttime)
gc()


# --- Parttime (Share) --- 

cs_parttime_share <- att_gt(
  yname = "share_parttime",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_parttime_share)
csdid_cohort_parttime_share <- ggdid(
  cs_parttime_share, 
  xlab = "Year",
  ylab = "Part-Time Postings (Share)",
  title = " ", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_parttime_share_fab.pdf",
  csdid_cohort_parttime_share,
  height = 8,
  width = 8,
  units = "in"
)

es_parttime_share <- aggte(
  cs_parttime_share,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_parttime_share)
csdid_es_parttime_share <- ggdid(
  es_parttime_share, 
  xlab = "Event Time",
  ylab = "Part-Time Postings (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_parttime_share_fab.pdf",
  csdid_es_parttime_share,
  height = 5, 
  width = 7,
  units = "in"
)

rm(cs_parttime_share, 
   csdid_cohort_parttime_share,
   es_parttime_share,
   csdid_es_parttime_share)
gc()


# -------------------- CS DID ANALYSIS (CONDITIONAL - SOC) ---------------------
# --- Total Postings --- 
cs_tot_post_cond <- att_gt(
  yname = "total_postings_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE, # makes this repeated cross-sections
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_tot_post_cond)
csdid_cond_cohort_tot_post <- ggdid(
  cs_tot_post_cond, 
  xlab = "Year", 
  ylab = "Total Postings", 
  title = "",
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_tot_post_fab.pdf",
  csdid_cond_cohort_tot_post,
  width = 8,
  height = 8,
  units = "in"
)

es_cond_tot_post <- aggte(
  cs_tot_post_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_tot_post)
csdid_es_cond_tot_post <- ggdid(
  es_cond_tot_post, 
  xlab = "Event Time", 
  ylab = "Total Postings",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_tot_post_fab.pdf",
  csdid_es_cond_tot_post,
  width = 7,
  height = 5,
  units = "in"
)

rm(cs_tot_post_cond, csdid_cond_cohort_tot_post, es_cond_tot_post, csdid_es_cond_tot_post)
gc()


# --- Any Experience --- 

cs_any_exp_cond <- att_gt(
  yname = "any_exp_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_any_exp_cond)
csdid_cond_cohort_exp <- ggdid(
  cs_any_exp_cond, 
  xlab = "Year",
  ylab = "Experience Required (Postings)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_exp_fab.pdf",
  csdid_cond_cohort_exp,
  width = 8,
  height = 8,
  units = "in"
)

es_cond_any_exp <- aggte(
  cs_any_exp_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_any_exp)
csdid_es_cond_exp <- ggdid(
  es_cond_any_exp, 
  xlab = "Event Time",
  ylab = "Experience Required (Postings)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_exp_fab.pdf",
  csdid_es_cond_exp,
  width = 7,
  height = 5,
  units = "in"
)

rm(cs_any_exp_cond, csdid_cond_cohort_exp, es_cond_any_exp, csdid_es_cond_exp)
gc()


# --- Share Experience --- 

cs_share_exp_cond <- att_gt(
  yname = "share_exp",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_share_exp_cond)
csdid_cond_cohort_exp_share <- ggdid(
  cs_share_exp_cond, 
  xlab = "Year",
  ylab = "Experience Required (Share)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_exp_share_fab.pdf",
  csdid_cond_cohort_exp_share,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_share_exp <- aggte(
  cs_share_exp_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_share_exp)
csdid_es_cond_exp_share <- ggdid(
  es_cond_share_exp, 
  xlab = "Event Time",
  ylab = "Experience Required (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_exp_share_fab.pdf",
  csdid_es_cond_exp_share,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_share_exp_cond, csdid_cond_cohort_exp_share, es_cond_share_exp, csdid_es_cond_exp_share)
gc()


# --- Average Experience --- 

cs_ave_exp_cond <- att_gt(
  yname = "ave_exp_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_ave_exp_cond)
csdid_cond_cohort_exp_ave <- ggdid(
  cs_ave_exp_cond, 
  xlab = "Year",
  ylab = "Experience Required (Average)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_exp_ave_fab.pdf",
  csdid_cond_cohort_exp_ave,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_ave_exp <- aggte(
  cs_ave_exp_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_ave_exp)
csdid_es_cond_exp_ave <- ggdid(
  es_cond_ave_exp, 
  xlab = "Event Time",
  ylab = "Experience Required (Average)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_exp_ave_fab.pdf",
  csdid_es_cond_exp_ave,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_ave_exp_cond, csdid_cond_cohort_exp_ave, es_cond_ave_exp, csdid_es_cond_exp_ave)
gc()


# --- Any Bachelor's Degree ---- 

cs_bachelor_cond <- att_gt(
  yname = "bachelor_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_bachelor_cond)
csdid_cond_cohort_bachelor <- ggdid(
  cs_bachelor_cond,
  xlab = "Year",
  ylab = "Bachelor's Required (Postings)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_bachelor_fab.pdf",
  csdid_cond_cohort_bachelor,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_bachelor <- aggte(
  cs_bachelor_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_bachelor)
csdid_es_cond_bachelor <- ggdid(
  es_cond_bachelor, 
  xlab = "Event Time",
  ylab = "Bachelor's Required (Postings)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_bachelor_fab.pdf",
  csdid_es_cond_bachelor,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_bachelor_cond, csdid_cond_cohort_bachelor, es_cond_bachelor, csdid_es_cond_bachelor)
gc()


# --- Share Bachelor's Degree ---- 

cs_bachelor_share_cond <- att_gt(
  yname = "share_bachelor",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_bachelor_share_cond)
csdid_cond_cohort_bachelor_share <- ggdid(
  cs_bachelor_share_cond, 
  xlab = "Year",
  ylab = "Bachelor's Required (Share)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_bachelor_share_fab.pdf",
  csdid_cond_cohort_bachelor_share,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_bachelor_share <- aggte(
  cs_bachelor_share_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_bachelor_share)
csdid_es_cond_bachelor_share <- ggdid(
  es_cond_bachelor_share, 
  xlab = "Event Time",
  ylab = "Bachelor's Required (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_bachelor_share_fab.pdf",
  csdid_es_cond_bachelor_share,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_bachelor_share_cond, 
   csdid_cond_cohort_bachelor_share, 
   es_cond_bachelor_share, 
   csdid_es_cond_bachelor_share)
gc()


# --- Fulltime --- 

cs_fulltime_cond <- att_gt(
  yname = "fulltime_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_fulltime_cond)
csdid_cond_cohort_fulltime <- ggdid(
  cs_fulltime_cond, 
  xlab = "Year",
  ylab = "Full-Time Postings",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_fulltime_fab.pdf",
  csdid_cond_cohort_fulltime,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_fulltime <- aggte(
  cs_fulltime_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_fulltime)
csdid_es_cond_fulltime <- ggdid(
  es_cond_fulltime, 
  xlab = "Event Time",
  ylab = "Full-Time Postings",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_fulltime_fab.pdf",
  csdid_es_cond_fulltime,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_fulltime_cond, csdid_cond_cohort_fulltime, es_cond_fulltime, csdid_es_cond_fulltime)
gc()


# --- Fulltime (Share) --- 

cs_fulltime_share_cond <- att_gt(
  yname = "share_fulltime",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_fulltime_share_cond)
csdid_cond_cohort_fulltime_share <- ggdid(
  cs_fulltime_share_cond, 
  xlab = "Year",
  ylab = "Full-Time Postings (Share)",
  title = " ", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_fulltime_share_fab.pdf",
  csdid_cond_cohort_fulltime_share,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_fulltime_share <- aggte(
  cs_fulltime_share_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_fulltime_share)
csdid_es_cond_fulltime_share <- ggdid(
  es_cond_fulltime_share, 
  xlab = "Event Time",
  ylab = "Full-Time Postings (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_fulltime_share_fab.pdf",
  csdid_es_cond_fulltime_share,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_fulltime_share_cond, 
   csdid_cond_cohort_fulltime_share, 
   es_cond_fulltime_share, 
   csdid_es_cond_fulltime_share)
gc()


# --- Parttime --- 

cs_parttime_cond <- att_gt(
  yname = "parttime_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_parttime_cond)
csdid_cond_cohort_parttime <- ggdid(
  cs_parttime_cond, 
  xlab = "Year",
  ylab = "Part-Time Postings",
  title = " ", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_parttime_fab.pdf",
  csdid_cond_cohort_parttime,
  height = 8,
  width = 8, 
  units = "in"
)

es_cond_parttime <- aggte(
  cs_parttime_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_parttime)
csdid_es_cond_parttime <- ggdid(
  es_cond_parttime, 
  xlab = "Event Time",
  ylab = "Part-Time Postings",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_parttime_fab.pdf",
  csdid_es_cond_parttime,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_parttime_cond, csdid_cond_cohort_parttime, es_cond_parttime, csdid_es_cond_parttime)
gc()


# --- Parttime (Share) --- 

cs_parttime_share_cond <- att_gt(
  yname = "share_parttime",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_parttime_share_cond)
csdid_cond_cohort_parttime_share <- ggdid(
  cs_parttime_share_cond, 
  xlab = "Year",
  ylab = "Part-Time Postings (Share)",
  title = " ", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_parttime_share_fab.pdf",
  csdid_cond_cohort_parttime_share,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_parttime_share <- aggte(
  cs_parttime_share_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_parttime_share)
csdid_es_cond_parttime_share <- ggdid(
  es_cond_parttime_share, 
  xlab = "Event Time",
  ylab = "Part-Time Postings (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_parttime_share_fab.pdf",
  csdid_es_cond_parttime_share,
  height = 5, 
  width = 7,
  units = "in"
)

rm(cs_parttime_share_cond, 
   csdid_cond_cohort_parttime_share,
   es_cond_parttime_share,
   csdid_es_cond_parttime_share)
gc()


