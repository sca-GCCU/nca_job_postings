##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "5d_descriptive_graphs_all_bans.R" 
# by: Sebastian C. Anastasi
# Date of this version: July 16, 2026
#
# Description: Generates descriptive graphs for analysis of states staggered
# (income-based) noncompete bans. Specifically, generates plot of treatment 
# rollout, cohort histogram, and plots of state-level raw means.
#
# Dependencies:  
#
# Output:
##############################################################################

# ---------------------------- HOUSEKEEPING ------------------------------------
rm(list = ls())
gc()

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")
#setwd("/home/scanast/nca_job_postings") # for cluster run

library(panelView)  # Visualize treatment rollout  
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)


# --------------------------- PREPARE THE DATA ---------------------------------
# --- Load Analysis Data --- 
agg2 <- read_csv("data/analysis-data/agg2_sab_analysis.csv") # prepared prior

# --- Aggregate to State-Year Level ---
df_state <- agg2 %>%
  group_by(state, year) %>%
  summarise(
    state_name = first(state_name),
    
    any_educ_state = sum(any_educ_soc, na.rm = TRUE),
    bachelor_state = sum(bachelor_soc, na.rm = TRUE),
    master_state = sum(master_soc, na.rm = TRUE),
    doctorate_state = sum(doctorate_soc, na.rm = TRUE),
    
    any_exp_state = sum(any_exp_soc, na.rm = TRUE),
    ave_exp_state = sum(ave_exp_num) / sum(ave_exp_den),
    
    total_postings_state = sum(total_postings_soc, na.rm = TRUE),
    
    fulltime_state = sum(fulltime_soc, na.rm = TRUE),
    parttime_state = sum(parttime_soc, na.rm = TRUE),
    flextime_state = sum(flextime_soc, na.rm = TRUE),
    
    treated = first(treated),
    cohort = first(cohort),
    
    .groups = "drop"
  ) %>%
  mutate(
    share_any_educ = any_educ_state / total_postings_state, 
    share_bachelor = bachelor_state / total_postings_state,
    share_master = master_state / total_postings_state, 
    share_doctorate = doctorate_state / total_postings_state, 
    
    share_exp = any_exp_state / total_postings_state,
    
    share_fulltime = fulltime_state / total_postings_state,
    share_parttime = parttime_state / total_postings_state,
    share_flextime = flextime_state / total_postings_state
  ) %>%
  mutate(
    state_name = if_else(
      state_name == "Washington, D.C. (District of Columbia)",
      "District of Columbia",
      state_name
    )
  )


# ---------------------- VISUALIZE TREATMENT ROLLOUT ---------------------------
# --- panelview rollout --- 
treatment_rollout <- panelview(
  total_postings_state ~ treated,
  data = df_state,
  index = c("state_name", "year"),
  pre.post = TRUE,
  main = "", # currently not creating title 
  xlab = "Year", ylab = "State"
)

ggsave(
  "output/figures/treatment_rollout_ab.pdf", # ab = all ban
  treatment_rollout,
  width = 7,
  height = 5,
  units = "in"
)

# --- Histogram Showing Distribution of Occ-State Pairs Across Cohorts --- 
cohort_summary <- agg2 %>%
  distinct(panel_id, cohort) %>%
  count(cohort) %>%
  mutate(
    cohort_label = case_when(
      cohort == 0 ~ "Never Treated",
      TRUE ~ paste0("Treated in ", cohort)
    )
  )

hist_cohorts <- ggplot(cohort_summary, aes(x = reorder(cohort_label, cohort), 
                                           y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.3, size = 3.5) +
  labs(
    x = "Cohort",
    y = "Distinct Occupation-State Pairs"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  "output/figures/hist_cohorts_sab.pdf", # sab = soc all ban
  hist_cohorts,
  width = 7,
  height = 5,
  units = "in"
)

rm(agg2, cohort_summary, hist_cohorts, treatment_rollout)
gc()


# -------------------- PLOT RAW MEANS (STATE-LEVEL) ----------------------------
# NOTE: In future I may want to plot firm-level means (probably without panelview). 

# Total Postings
mean_tot_post <- panelview(total_postings_state ~ treated,
                           data = df_state, index = c("state", "year"),
                           type = "outcome", main = "",
                           xlab = "Year", ylab = "Total Postings")
ggsave(
  "output/figures/mean_tot_post_ab.pdf",
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
  "output/figures/mean_tot_post_cohort_ab.pdf",
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
  "output/figures/mean_any_exp_ab.pdf",
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
  "output/figures/mean_any_exp_cohort_ab.pdf",
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
  "output/figures/mean_ave_exp_ab.pdf",
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
  "output/figures/mean_ave_exp_cohort_ab.pdf",
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
  "output/figures/mean_share_exp_ab.pdf",
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
  "output/figures/mean_share_exp_cohort_ab.pdf",
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
  "output/figures/mean_bachelor_ab.pdf",
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
  "output/figures/mean_bachelor_cohort_ab.pdf",
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
  "output/figures/mean_bachelor_share_ab.pdf",
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
  "output/figures/mean_bachelor_share_cohort_ab.pdf",
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
  "output/figures/mean_fulltime_ab.pdf",
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
  "output/figures/mean_fulltime_cohort_ab.pdf",
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
  "output/figures/mean_fulltime_share_ab.pdf",
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
  "output/figures/mean_fulltime_share_cohort_ab.pdf",
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
  "output/figures/mean_parttime_ab.pdf",
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
  "output/figures/mean_parttime_cohort_ab.pdf",
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
  "output/figures/mean_parttime_share_ab.pdf",
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
  "output/figures/mean_parttime_share_cohort_ab.pdf",
  mean_parttime_share_cohort,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(mean_parttime_share_cohort)
gc()


