##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "sumstats_salary_mn" 
# by: Sebastian C. Anastasi
# Date of this version: March 24, 2026
#
# Description: Creates the listings level sumstats for salary analysis. 
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
library(stargazer)
library(kableExtra)
library(scales)

# Load analysis data 
salary_mn_analysis <- read_csv("data/analysis-data/salary_mn_analysis.csv")


# Create vectors of key variables  
outcome_var <- c("real_salary", "real_salary_from", "real_salary_to")

control_var <- c("real_income", "real_hpi", "unemp_rate", "frac_male",
                 "frac_black", "frac_college", "mean_age")

# MN ban date vector 
mn_ban_date <- salary_mn_analysis %>%
  filter(ban_full == 1) %>%
  summarise(ban_date = first(date_eff_full)) %>%
  pull(ban_date)


# A. Lightcast Summary Statistics by Treatment Status

# --- 1. Compute Summaries --- 
sumtab_out <- salary_mn_analysis %>%
  filter(date < mn_ban_date) %>%
  group_by(ban_full) %>%
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
    ban_full = if_else(ban_full == 1, "Treatment", "Control")
  ) 


#--- 2. Build Main Summary Stat Rows --- 
sumtab_out_main <- sumtab_out %>%
  select(-N) %>%
  pivot_longer(
    cols = -ban_full,
    names_to = c("variable", ".value"),
    names_pattern = "(.+)_(Mean|SD)"
  ) %>%
  pivot_wider(
    names_from = ban_full,
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
    variable = c("Salary (Real)", "Salary Lower Threshold (Real)",
                 "Salary Upper Threshold (Real)")
  ) %>%
  mutate(
    across(
      c(Mean_Treatment, SD_Treatment, Mean_Control, SD_Control),
      ~ number(.x, accuracy = 0.01, big.mark = ",")
    )
  )


#--- 3. Create obs row --- 
obs_row_out <- sumtab_out %>%
  select(ban_full, N) %>%
  pivot_wider(
    names_from = ban_full,
    values_from = N
  ) %>%
  transmute(
    variable = "Observations",
    Mean_Treatment = comma(Treatment),
    SD_Treatment = "",
    Mean_Control = comma(Control),
    SD_Control = ""
  )


#--- 4. Combine ---
sumtab_out_final <- bind_rows(sumtab_out_main, obs_row_out)


# -- 5. Print Table --- 
table_sumstat_outcomes <- sumtab_out_final %>%
  kable(
    format = "latex",
    booktabs = TRUE,
    linesep = "",
    align = c("lcccc"),
    caption = "Summary Statistics - Listing-Level Salary Variables",
    label = "sumstats_outcomes",
    col.names = c("", "Mean", "SD", "Mean", "SD")
  ) %>%
  add_header_above(c(" " = 1, "Treatment" = 2, "Control" = 2)) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "Notes: This table reports the pre-ban means and standard deviations of salary key variables from the listings-level aggregation of the Lightcast job-postings data.",
    threeparttable = TRUE
  ) 

print(table_sumstat_outcomes)

# --- 6. Write table to a .tex file ---  
writeLines(table_sumstat_outcomes, "output/tables/table_sumstat_outcomes_salary_mn.tex")



# B. Salary Summary Statistics by Any Experience




# C. Balance Table of Control Variables ??? 

# NOTE/QUESTION: These are state-level covariates. Estimating SD for them
# seems misleading. How should I go about presenting such state-level-controls?








