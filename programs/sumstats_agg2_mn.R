##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "sumstats_agg2_mn.R" 
# by: Sebastian C. Anastasi
# Date of this version: March 25, 2026
#
# Description: Creates the firm-occupation-state-month level sumstats. 
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
agg2_mn_analysis <- read_csv("data/analysis-data/agg2_mn_analysis.csv")


# Create vectors of key variables  
outcome_var <- c("any_educ_share", "bachelor_share", "master_share", 
                 "doctorate_share", "any_exp_share", "ave_exp", "fulltime_share",
                 "internship_share")

control_var <- c("real_income", "real_hpi", "unemp_rate", "frac_male",
                 "frac_black", "frac_college", "mean_age")

# MN ban date vector 
mn_ban_date <- agg2_mn_analysis %>%
  filter(ban_full == 1) %>%
  summarise(ban_date = first(date_eff_full)) %>%
  pull(ban_date)


# A. Lightcast Summary Statistics by Treatment Status

# --- 1. Compute Summaries --- 
sumtab_out <- agg2_mn_analysis %>%
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
    variable = c("Any Education", "Bachelor's", "Master's", "Doctorate",
                 "Any Experience", "Average Experience", 
                 "Fulltime", "Internship")
  ) %>%
  mutate(
    across(
      c(Mean_Treatment, SD_Treatment, Mean_Control, SD_Control),
      ~ sprintf("%.2f", .x) # 2-digits after decimal; fixed-point notation
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
    digits = 2,
    caption = "Summary Statistics - Firm-by-Occupation-by-State-by-Date",
    label = "sumstats_outcomes_agg2",
    col.names = c("", "Mean", "SD", "Mean", "SD")
  ) %>%
  add_header_above(c(" " = 1, "Treatment" = 2, "Control" = 2)) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the pre-ban means and standard deviations of key variables from the firm-by-occupation-by-state-by-date-level aggregation of the Lightcast job-postings data.",
    threeparttable = TRUE,
    escape = FALSE
  ) 

print(table_sumstat_outcomes)

# --- 6. Write table to a .tex file ---  
writeLines(table_sumstat_outcomes, "output/tables/table_sumstat_outcomes_agg2_mn.tex")



# B. Balance Table of Control Variables ??? 

# NOTE/QUESTION: These are state-level covariates. Estimating SD for them
# seems misleading. How should I go about presenting such state-level-controls?








