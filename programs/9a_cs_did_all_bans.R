##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "9a_cs_did_all_bans.R" 
# by: Sebastian C. Anastasi
# Date of this version: June 8, 2026
#
# Description:  
#
# Dependencies:  
#
# Output:
##############################################################################

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")
#setwd("/home/scanast/nca_job_postings") # for cluster run

library(did)        # Callaway & Sant'Anna estimator 
library(panelView)  # Visualize treatment rollout  
library(fixest)     # High-performance fixed-effects regression (TWFE comparison)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)

df <- read_csv("data/analysis-data/agg2_analysis.csv")

# ------------------------ 1. PREP AGGREGATIONS --------------------------------
# --- FIRM-STATE LEVEL AGGREGATION --- 

# Collapse to firm-by-state-by-year panel 
df_firm <- df %>%
  group_by(company, state, year) %>% 
  summarise(
    state_name = first(state_name),
    
    any_educ_firm = sum(any_educ, na.rm = TRUE),
    bachelor_firm = sum(bachelor, na.rm = TRUE),
    master_firm = sum(master, na.rm = TRUE),
    doctorate_firm = sum(doctorate, na.rm = TRUE),
    
    any_exp_firm = sum(any_exp, na.rm = TRUE),
    ave_exp_firm = weighted.mean(ave_exp, total_postings, na.rm = TRUE),
    
    total_postings_firm = sum(total_postings, na.rm = TRUE),
    
    fulltime_firm = sum(fulltime, na.rm = TRUE),
    parttime_firm = sum(parttime, na.rm = TRUE),
    flextime_firm = sum(flextime, na.rm = TRUE),
    
    ban_inc1 = first(ban_inc1),
    ban_inc2 = first(ban_inc2),
    eff_inc1_year = first(eff_inc1_year),
    eff_inc2_year = first(eff_inc2_year),
    inc_threshold1 = first(inc_threshold1),
    inc_threshold2 = first(inc_threshold2),
    inc_threshold_2024 = first(inc_threshold_2024),
    
    .groups = "drop"
  )

# Checking whether some values of ave_exp SHOULD be NA and why they are NA 
# test <- read_csv("data/analysis-data/agg2_analysis.csv")
# 
# test %>%
#   filter(is.na(ave_exp)) %>%
#   nrow()
# 
# test <- test %>%
#   filter(is.na(ave_exp)) %>%
#   select(
#     company, state, year,
#     ave_exp, any_exp
#   )

# Replace NaN values in ave_exp_firm
df_firm <- df_firm %>%
  mutate(
    ave_exp_firm = ifelse(is.nan(ave_exp_firm), NA, ave_exp_firm)
  )

# Create share variables 
df_firm <- df_firm %>%
  mutate(
    share_any_educ = any_educ_firm / total_postings_firm, 
    share_bachelor = bachelor_firm / total_postings_firm,
    share_master = master_firm / total_postings_firm, 
    share_doctorate = doctorate_firm / total_postings_firm, 
    
    share_exp = any_exp_firm / total_postings_firm,
    
    share_fulltime = fulltime_firm / total_postings_firm,
    share_parttime = parttime_firm / total_postings_firm,
    flextime_firm = flextime_firm / total_postings_firm
  )

# Create treatment indicator 
df_firm <- df_firm %>%
  mutate(
    treated = as.integer(!is.na(eff_inc1_year) & year >= eff_inc1_year)
  )

# Create unique unit (company-by-state) identifier 
df_firm <- df_firm %>% 
  mutate(
    company_state_id = as.integer(interaction(company, state, drop = TRUE))
  )

# Create a cohort variable (year must be 0 for never-treated)
df_firm <- df_firm %>%
  mutate(
    cohort = ifelse(is.na(eff_inc1_year), 0, eff_inc1_year)
  )

# Drop 2025 because not full year 
df_firm <- df_firm %>%
  filter(year != 2025)


# --- STATE LEVEL AGGREGATION --- 

# Create a state-level panel for panelview
df_state <- df %>%
  group_by(state, year) %>%
  summarise(
    state_name = first(state_name),
    
    any_educ_state = sum(any_educ, na.rm = TRUE),
    bachelor_state = sum(bachelor, na.rm = TRUE),
    master_state = sum(master, na.rm = TRUE),
    doctorate_state = sum(doctorate, na.rm = TRUE),
    
    any_exp_state = sum(any_exp, na.rm = TRUE),
    ave_exp_state = weighted.mean(ave_exp, total_postings, na.rm = TRUE),
    
    total_postings_state = sum(total_postings, na.rm = TRUE),
    
    fulltime_state = sum(fulltime, na.rm = TRUE),
    parttime_state = sum(parttime, na.rm = TRUE),
    flextime_state = sum(flextime, na.rm = TRUE),
    
    ban_inc1 = first(ban_inc1),
    ban_inc2 = first(ban_inc2),
    eff_inc1_year = first(eff_inc1_year),
    eff_inc2_year = first(eff_inc2_year),
    inc_threshold1 = first(inc_threshold1),
    inc_threshold2 = first(inc_threshold2),
    inc_threshold_2024 = first(inc_threshold_2024),
    
    .groups = "drop"
  )

# Replace NaN values in ave_exp_firm
df_state <- df_state %>%
  mutate(
    ave_exp_state = ifelse(is.nan(ave_exp_state), NA, ave_exp_state)
  )

# Create share variables 
df_state <- df_state %>%
  mutate(
    share_any_educ = any_educ_state / total_postings_state, 
    share_bachelor = bachelor_state / total_postings_state,
    share_master = master_state / total_postings_state, 
    share_doctorate = doctorate_state / total_postings_state, 
    
    share_exp = any_exp_state / total_postings_state,
    
    share_fulltime = fulltime_state / total_postings_state,
    share_parttime = parttime_state / total_postings_state,
    flextime_state = flextime_state / total_postings_state
  )

# Create treatment indicator 
df_state <- df_state %>%
  mutate(
    treated = as.integer(!is.na(eff_inc1_year) & year >= eff_inc1_year)
  )

df_state <- df_state %>%
  mutate(
    cohort = ifelse(is.na(eff_inc1_year), 0, eff_inc1_year)
  )

# Change name of D.C. for treatment rollout plot
df_state <- df_state %>%
  mutate(
    state_name = if_else(
      state_name == "Washington, D.C. (District of Columbia)",
      "District of Columbia",
      state_name
    )
  )

# Drop 2025 because not a full year 
df_state <- df_state %>%
  filter(year != 2025)

# Drop raw data
rm(df)
gc()

# ------------------------- 2. VISUALIZE ROLLOUT -------------------------------

# NOTE: Only going to worry about year for the time being.

treatment_rollout <- panelview(
  total_postings_state ~ treated,
  data = df_state,
  index = c("state_name", "year"),
  pre.post = TRUE,
  main = "", # currently not creating title 
  xlab = "Year", ylab = "State"
)

ggsave(
  "output/figures/treatment_rollout_all_bans.pdf",
  treatment_rollout,
  width = 7,
  height = 5,
  units = "in"
)

cohort_summary <- df_firm %>%
  distinct(company_state_id, cohort) %>%
  count(cohort) %>%
  mutate(
    cohort_label = case_when(
      cohort == 0 ~ "Never Treated",
      TRUE ~ paste0("Treated in ", cohort)
    )
  )

print(cohort_summary)

# NOTE: Eventually construct histogram of this cohort_summary.

rm(treatment_rollout, cohort_summary)
gc()


# ----------------------- 3. PLOT RAW MEANS ------------------------------------

# Total Postings
mean_tot_post <- panelview(total_postings_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Total Postings")
ggsave(
  "output/figures/mean_tot_post_all_ban.pdf",
  mean_tot_post,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_tot_post)
gc()

mean_tot_post_cohort <- panelview(total_postings_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Total Postings",
          by.cohort = TRUE)
ggsave(
  "output/figures/mean_tot_post_cohort_all_ban.pdf",
  mean_tot_post_cohort,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_tot_post_cohort)
gc()


# Any Experienced Required
mean_any_exp <- panelview(any_exp_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Experience Required")
ggsave(
  "output/figures/mean_any_exp_all_bans.pdf",
  mean_any_exp,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_any_exp)
gc()

mean_any_exp_cohort <- panelview(any_exp_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Experience Required",
          by.cohort = TRUE)
ggsave(
  "output/figures/mean_any_exp_cohort_all_bans.pdf",
  mean_any_exp_cohort,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_any_exp_cohort)
gc()


# Experience Required (Average) 
mean_ave_exp <- panelview(ave_exp_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Experience Required (Average)")
ggsave(
  "output/figures/mean_ave_exp_all_bans.pdf",
  mean_ave_exp,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_ave_exp)
gc()

mean_ave_exp_cohort <- panelview(ave_exp_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Experience Required (Average)",
          by.cohort = TRUE)
ggsave(
  "output/figures/mean_ave_exp_cohort_all_bans.pdf",
  mean_ave_exp_cohort,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_ave_exp_cohort)
gc()


# Share Experience Required 
mean_share_exp <- panelview(share_exp ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Experience Required (Share)")
ggsave(
  "output/figures/mean_share_exp_all_bans.pdf",
  mean_share_exp,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_share_exp)
gc()

mean_share_exp_cohort <- panelview(share_exp ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Experience Required (Share)",
          by.cohort = TRUE)
ggsave(
  "output/figures/mean_share_exp_cohort_all_bans.pdf",
  mean_share_exp_cohort,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_share_exp_cohort)
gc()


# Bachelor's 
mean_bachelor <- panelview(bachelor_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Bachelor's Required")
ggsave(
  "output/figures/mean_bachelor_all_bans.pdf",
  mean_bachelor,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_bachelor)
gc()

mean_bachelor_cohort <- panelview(bachelor_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Bachelor's Required",
          by.cohort = TRUE)
ggsave(
  "output/figures/mean_bachelor_cohort_all_bans.pdf",
  mean_bachelor_cohort,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_bachelor_cohort)
gc()


# Share Bachelor's
mean_share_bachelor <- panelview(share_bachelor ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Bachelor's Required (Share)")
ggsave(
  "output/figures/mean_bachelor_share_all_bans.pdf",
  mean_share_bachelor,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_share_bachelor)
gc()

mean_share_bachelor_cohort <- panelview(share_bachelor ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Bachelor's Required (Share)",
          by.cohort = TRUE)
ggsave(
  "output/figures/mean_bachelor_share_cohort_all_bans.pdf",
  mean_share_bachelor_cohort,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_share_bachelor_cohort)
gc()


# Fulltime 
mean_fulltime <- panelview(fulltime_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Full-Time Postings")
ggsave(
  "output/figures/mean_fulltime_all_bans.pdf",
  mean_fulltime,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_fulltime)
gc()

mean_fulltime_cohort <- panelview(fulltime_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Full-Time Postings",
          by.cohort = TRUE)
ggsave(
  "output/figures/mean_fulltime_cohort_all_bans.pdf",
  mean_fulltime_cohort,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_fulltime_cohort)
gc()


# Fulltime Share 
mean_fulltime_share <- panelview(share_fulltime ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Full-Time Postings (Share)")
ggsave(
  "output/figures/mean_fulltime_share_all_bans.pdf",
  mean_fulltime_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_fulltime_share)
gc()

mean_fulltime_share_cohort <- panelview(share_fulltime ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Full-Time Postings (Share)",
          by.cohort = TRUE)
ggsave(
  "output/figures/mean_fulltime_share_cohort_all_bans.pdf",
  mean_fulltime_share_cohort,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_fulltime_share_cohort)
gc()


# Parttime 
mean_parttime <- panelview(parttime_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Part-Time Postings")
ggsave(
  "output/figures/mean_parttime_all_bans.pdf",
  mean_parttime,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_parttime)
gc()

mean_parttime_cohort <- panelview(parttime_state ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Part-Time Postings",
          by.cohort = TRUE)
ggsave(
  "output/figures/mean_parttime_cohort_all_bans.pdf",
  mean_parttime_cohort,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_parttime_cohort)
gc()


# Parttime Share 
mean_parttime_share <- panelview(share_parttime ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Part-Time Postings (Share)")
ggsave(
  "output/figures/mean_parttime_share_all_bans.pdf",
  mean_parttime_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_parttime_share)
gc()

mean_parttime_share_cohort <- panelview(share_parttime ~ treated,
          data = df_state, index = c("state", "year"),
          type = "outcome", main = "",
          xlab = "Year", ylab = "Part-Time Postings (Share)",
          by.cohort = TRUE)
ggsave(
  "output/figures/mean_parttime_share_cohort_all_bans.pdf",
  mean_parttime_share_cohort,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_parttime_share_cohort)
gc()


# ------------------------- 4. CS DID ANALYSIS ---------------------------------

set.seed(87) # for reproducibility with bootstrap SEs. 

# --- Total Postings --- 

# Balanced panel version
# cs_tot_post_balanced <- att_gt(
#   yname = "total_postings_firm",
#   tname = "year",
#   idname = "company_state_id",
#   gname = "cohort",
#   data = df_firm,
#   control_group = "nevertreated",
#   est_method = "dr",
#   bstrap = TRUE,
#   cband = TRUE,
#   clustervars = "state",
#   base_period = "universal",
#   anticipation = 0
# )
# summary(cs_tot_post_balanced)
# NOTE: n = 431

# Unbalanced panel version (repeated cross-sections)
cs_tot_post <- att_gt(
  yname = "total_postings_firm",
  tname = "year",
  idname = "company_state_id",
  gname = "cohort",
  panel = FALSE, # makes this repeated cross-sections
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0
)
summary(cs_tot_post)
csdid_cohort_tot_post <- ggdid(
  cs_tot_post, 
  xlab = "Year", 
  ylab = "Total Postings", 
  title = "",
  grtitle = "Ban in"
)
# NOTE: Seeing more of an effect in the 2022 cohort (where 
# inc_threshold was high).
ggsave(
  "output/figures/csdid_cohort_tot_post.pdf",
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
  "output/figures/csdid_es_tot_post.pdf",
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
  idname = "company_state_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0
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
  "output/figures/csdid_cohort_exp.pdf",
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
  "output/figures/csdid_es_exp.pdf",
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
  idname = "company_state_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0
)
summary(cs_share_exp)
csdid_cohort_exp_share <- ggdid(
  cs_share_exp, 
  xlab = "Year",
  ylab = "Experience Required (Share)",
  title = "", 
  grtitle = "Ban in"
)
# NOTE: Again, I'm seeing the biggest effect in CO and DC.
ggsave(
  "output/figures/csdid_cohort_exp_share.pdf",
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
  "output/figures/csdid_es_exp_share.pdf",
  csdid_es_exp_share,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_share_exp, csdid_cohort_exp_share, es_share_exp, csdid_es_exp_share)
rm()


# --- Average Experience --- 

cs_ave_exp <- att_gt(
  yname = "ave_exp_firm",
  tname = "year",
  idname = "company_state_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0
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
  "output/figures/csdid_cohort_exp_ave.pdf",
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
  "output/figures/csdid_es_exp_ave.pdf",
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
  idname = "company_state_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0
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
  "output/figures/csdid_cohort_bachelor.pdf",
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
  "output/figures/csdid_es_bachelor.pdf",
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
  idname = "company_state_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0
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
  "output/figures/csdid_cohort_bachelor_share.pdf",
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
  "output/figures/csdid_es_bachelor_share.pdf",
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
  idname = "company_state_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0
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
  "output/figures/csdid_cohort_fulltime.pdf",
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
  "output/figures/csdid_es_fulltime.pdf",
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
  idname = "company_state_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0
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
  "output/figures/csdid_cohort_fulltime_share.pdf",
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
  "output/figures/csdid_es_fulltime_share.pdf",
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
  idname = "company_state_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0
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
  "output/figures/csdid_cohort_parttime.pdf",
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
  "output/figures/csdid_es_parttime.pdf",
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
  idname = "company_state_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0
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
  "output/figures/csdid_cohort_parttime_share.pdf",
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
  "output/figures/csdid_es_parttime_share.pdf",
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

