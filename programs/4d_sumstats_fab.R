##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "4d_sumstats_fab.R" 
# by: Sebastian C. Anastasi
# Date of this version: July 23, 2026
#
# Description: Generates summary statistics for the firm-state-year level 
# aggregation.
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

# --- Load packages --- 
library(dplyr)
library(tidyr)
library(readr)
library(kableExtra)
library(scales)

# Load analysis data 
agg2 <- read_csv("data/analysis-data/agg2_fab_analysis.csv")

# Create vectors of key variables 
outcome_var <- c(
  "total_postings_firm", "any_exp_firm", "share_exp", "ave_exp_firm",
  "bachelor_firm", "share_bachelor", "fulltime_firm", "share_fulltime",
  "parttime_firm", "share_parttime", "flextime_firm", "share_flextime"
)
# NOTE: Include description of flextime in tablenotes. 

control_var <- c("real_income", "real_hpi", "unemp_rate", "frac_male",
                 "frac_black", "frac_college", "mean_age")


# ---------------- LIGHTCAST SUMMARY STATS BY TREATMENT STATUS -----------------  
# ---- Impose Restrictions ----
# Drop the always treated (Oregon)
# NOTE: This is what CS DID does anyhow.
agg2 <- agg2 %>%
  filter(
    eff_inc1_year > 2008 | is.na(eff_inc1_year)
  )

# ---- Compute Sum Stats ----
# NOTE: Compute Sum Stats for 2016 (since that's Illinois pre-period).
# Create ever-treated indicator 
agg2 <- agg2 %>%
  mutate(
    ever_treated = as.integer(!is.na(eff_inc1_year))
  )

sumtab_out <- agg2 %>%
  filter(year == 2016) %>%
  group_by(ever_treated) %>%
  summarise(
    across(
      all_of(outcome_var), # col.
      list( # fn.
        Mean = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}" # names
    ),
    N = n(),
    .groups = "drop"
  ) %>%
  mutate(
    ever_treated = if_else(ever_treated == 1, "Treatment", "Control") 
  )

# ---- Generate Table ---- 
# Means and SD
sumtab_out_main <- sumtab_out %>%
  select(-N) %>%
  pivot_longer(
    cols = -ever_treated,
    names_to = c("variable", ".value"),
    names_pattern = "(.+)_(Mean|SD)"
  ) %>%
  pivot_wider(
    names_from = ever_treated,
    values_from = c(Mean, SD)
  ) %>%
  select(
    variable,
    Mean_Treatment,
    SD_Treatment,
    Mean_Control,
    SD_Control
  ) %>%
  mutate(
    variable = c("Postings", "Experience Required (Postings)",
                 "Experience Required (Share)", "Experience Required (Average)",
                 "Bachelor's Required (Postings)", "Bachelor's Required (Share)",
                 "Full Time (Postings)", "Full Time (Share)",
                 "Part Time (Postings)", "Part Time (Share)",
                 "Flex Time (Postings)", "Flex Time (Share)")
  ) %>%
  mutate(
    across(
      c(Mean_Treatment, SD_Treatment, Mean_Control, SD_Control),
      ~ sprintf("%.2f", .x) # 2-digits after decimal; fixed-point notation 
    )
  )

# Obs Count 
obs_row_out <- sumtab_out %>%
  select(ever_treated, N) %>%
  pivot_wider(
    names_from = ever_treated,
    values_from = N
  ) %>%
  transmute(
    variable = "Observations",
    Mean_Treatment = comma(Treatment),
    SD_Treatment = "",
    Mean_Control = comma(Control),
    SD_Control = ""
  )

# combined Table 
sumtab_out_final <- bind_rows(sumtab_out_main, obs_row_out)

# ---- Print Table ----
table_sumstat_outcomes <- sumtab_out_final %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    align = c("lcccc"),
    digits = 2,
    caption = "Summary Statistics - Firm-by-State-by-Year Sample",
    label = "tab:sumstats_outcomes_agg2_ab",
    col.names = c("", "Mean", "SD", "Mean", "SD")
  ) %>%
  add_header_above(c(" " = 1, "Treatment" = 2, "Control" = 2)) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the pre-ban means and standard deviations of key variables from the firm-by-state-by-year-level aggregation of the Lightcast job-postings data. Specifically, it reports the mean values of these variables in 2016, before all of the states whose bans we study occur. Observations from Oregon---which implements an NCA ban in 2008 and is, consequently, always treated in our sample---are dropped.",
    threeparttable = TRUE,
    escape = FALSE
  ) 

writeLines(table_sumstat_outcomes, "output/tables/table_sumstat_outcomes_agg2_ab.tex")

rm(sumtab_out, sumtab_out_main, sumtab_controls_final, obs_row_out)
gc()

# -------------------------- BALANCE TABLE -------------------------------------
# NOTE: These covariates vary at the state-by-year level. Because agg2 is a
# firm-by-state-by-year dataset, the statistics below weight states according
# to the number of firm observations in each state.

# ---- Compute Sum Stats ----
sumtab_controls <- agg2 %>%
  filter(year == 2016) %>%
  group_by(ever_treated) %>%
  summarise(
    across(
      all_of(control_var),
      list(
        Mean = ~mean(.x, na.rm = TRUE),
        SD = ~sd(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ),
    N = n(),
    .groups = "drop"
  ) %>%
  mutate(
    ever_treated = if_else(ever_treated == 1, "Treatment", "Control")
  )

# ---- Generate Table ----
# Means and SD
sumtab_controls_main <- sumtab_controls %>%
  select(-N) %>%
  pivot_longer(
    cols = -ever_treated,
    names_to = c("variable", ".value"),
    names_pattern = "(.+)_(Mean|SD)"
  ) %>%
  pivot_wider(
    names_from = ever_treated,
    values_from = c(Mean, SD)
  ) %>%
  select(
    variable,
    Mean_Treatment,
    SD_Treatment,
    Mean_Control,
    SD_Control
  ) %>%
  mutate(
    variable = c(
      "Real Income",
      "Real House Price Index",
      "Unemployment Rate",
      "Male Share",
      "Black Share",
      "College Share",
      "Mean Age"
    )
  ) %>%
  mutate(
    across(
      c(Mean_Treatment, SD_Treatment, Mean_Control, SD_Control),
      ~ sprintf("%.2f", .x)
    )
  )

# Obs Count
obs_row_controls <- sumtab_controls %>%
  select(ever_treated, N) %>%
  pivot_wider(
    names_from = ever_treated,
    values_from = N
  ) %>%
  transmute(
    variable = "Observations",
    Mean_Treatment = comma(Treatment),
    SD_Treatment = "",
    Mean_Control = comma(Control),
    SD_Control = ""
  )

# Combined Table
sumtab_controls_final <- bind_rows(sumtab_controls_main, obs_row_controls)

# ---- Print Table ----
table_balance_controls <- sumtab_controls_final %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    align = c("lcccc"),
    digits = 2,
    caption = "Balance of State-Level Control Variables",
    label = "tab:balance_controls_agg2_ab",
    col.names = c("", "Mean", "SD", "Mean", "SD")
  ) %>%
  add_header_above(c(" " = 1, "Treatment" = 2, "Control" = 2)) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the 2016 means and standard deviations of the state-level control variables for states that are ever treated and states that are never treated. Observations from Oregon---which implements an NCA ban in 2008 and is, consequently, always treated in our sample---are dropped. Because the underlying analysis dataset is at the firm-by-state-by-year level, states are implicitly weighted by their number of firm observations.",
    threeparttable = TRUE,
    escape = FALSE
  )

writeLines(
  table_balance_controls,
  "output/tables/table_balance_controls_agg2_ab.tex"
)


