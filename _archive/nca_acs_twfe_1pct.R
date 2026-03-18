# PREAMBLE ---------------------------------------------------------------------

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/data")

# packages 

library(tidyverse)
library(haven)
library(fixest)
library(modelsummary)


# DATA -------------------------------------------------------------------------

nca_acs_soc_1pct <- read_dta("nca_acs_soc_1pct.dta")

# VARIABLE VECTORS -------------------------------------------------------------

keep_vars <- c("year", "statefip", "perwt", "treated_eff")

outcome_vars <- c("age", "pot_exp", "incwage", "young_adult", "earlyc_adult", 
                  "mlc_adult", "older_adult", "no_high_school", "high_school", 
                  "some_college", "college")


# VERSION W/ LOOPS ------------------------------------------------------------- 

# STILL EDITING!

results_dir <- "C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/results_acs_1pct"
dir.create(results_dir, showWarnings = FALSE, recursive = TRUE)

# formulas made on the fly
mk_fml <- function(y) as.formula(paste0(y, " ~ treated_eff | statefip + year"))

# run one feols with your usual spec
fit_one <- function(data, y, subset_expr = NULL) {
  feols(
    formula = mk_fml(y),
    data = data,
    subset = subset_expr,      # e.g., ~ young_adult == 1, or NULL
    weights = ~ perwt,
    cluster = ~ statefip
  )
}

# tidy label dictionary for etable/modelsummary
pretty_names <- c(
  treated_eff = "Post × Treated",
  "(Intercept)" = "Constant"
)

# -------------------------------------------
# CLEAN DATA MAKERS (your original filters, with select fix)
# -------------------------------------------
mk_hw_data <- function(yr) {
  nca_acs_soc_1pct %>%
    dplyr::filter(year_eff_ban %in% c(0, yr), is.na(hw_ban) | hw_ban == 1) %>%
    dplyr::select(dplyr::all_of(c(keep_vars, outcome_vars)))
}

mk_lw_data <- function(yr) {
  nca_acs_soc_1pct %>%
    dplyr::filter(year_eff_ban %in% c(0, yr), is.na(hw_ban) | hw_ban == 0) %>%
    dplyr::select(dplyr::all_of(c(keep_vars, outcome_vars)))
}

# -------------------------------------------
# RUNNER: fits all outcomes (+ a "young-only" wage spec), returns named list
# -------------------------------------------
run_block <- function(dat, tag) {
  out <- list()
  
  # wage (all)
  out[[paste0("wage_all_", tag)]]  <- fit_one(dat, "incwage", NULL)
  # wage (young only)
  out[[paste0("wage_young_", tag)]] <- fit_one(dat, "incwage", ~ young_adult == 1)
  
  # the rest of outcomes (levels/rates)
  for (y in setdiff(outcome_vars, "incwage")) {
    out_name <- paste0(y, "_", tag)
    out[[out_name]] <- fit_one(dat, y, NULL)
  }
  
  out
}

# -------------------------------------------
# RUN EVERYTHING (HW cohorts then LW cohorts)
# -------------------------------------------
all_models <- list()

# HW cohorts
for (yr in c(2008, 2020, 2022)) {
  dat <- mk_hw_data(yr)
  tag <- paste0("hw", yr)
  all_models <- c(all_models, run_block(dat, tag))
}

# LW cohorts
for (yr in c(2017, 2018, 2019, 2020, 2021)) {
  dat <- mk_lw_data(yr)
  tag <- paste0("lw", yr)
  all_models <- c(all_models, run_block(dat, tag))
}

# Save everything (RDS) for later use
saveRDS(all_models, file.path(results_dir, "twfe_fixest_models_1pct.rds"))

# -------------------------------------------
# EXAMPLE EXPORTS TO LATEX
# (1) fixest::etable: one table per cohort, many outcomes side-by-side
# -------------------------------------------
mk_table_for_tag <- function(tag, file) {
  # pick a subset of models for the table (customize as you like)
  # here: wage all, wage young, age, pot_exp, education levels
  wanted <- c(
    paste0("wage_all_", tag),
    paste0("wage_young_", tag),
    paste0("age_", tag),
    paste0("pot_exp_", tag),
    paste0("no_high_school_", tag),
    paste0("high_school_", tag),
    paste0("some_college_", tag),
    paste0("college_", tag)
  )
  mdl <- all_models[wanted]
  
  fixest::etable(
    mdl,
    dict = pretty_names,
    se = "cluster",        # already clustered; this instructs display
    cluster = "statefip",  # label in footer
    fitstat = ~ n + r2,    # add N and R2
    tex = TRUE,
    style.tex = "aer",     # or "default", "qje", etc.
    title = paste0("TWFE Results: ", toupper(tag)),
    notes = "All models include state and year fixed effects and are weighted by ACS perwt.",
    file = file
  )
}

# produce one table per cohort tag (you can expand/alter groupings)
for (tag in c("hw2008","hw2020","hw2022","lw2017","lw2018","lw2019","lw2020","lw2021")) {
  mk_table_for_tag(tag, file.path(results_dir, paste0("etable_", tag, ".tex")))
}

# -------------------------------------------
# (2) modelsummary export (alternative; very flexible)
#    — e.g., a compact “wage only” comparison across cohorts
# -------------------------------------------
wage_models <- all_models[c("wage_all_hw2008","wage_all_hw2020","wage_all_hw2022",
                            "wage_all_lw2017","wage_all_lw2018","wage_all_lw2019",
                            "wage_all_lw2020","wage_all_lw2021")]

modelsummary::modelsummary(
  wage_models,
  coef_map = pretty_names,
  gof_map = c("nobs","r.squared"),
  output = file.path(results_dir, "modelsummary_wage_all.tex"),
  title = "TWFE: Effect on Wages (All, by Cohort)",
  add_rows = data.frame(
    term = "FE & Weights",
    value = "State & Year FE; Weighted by perwt"
  )
)











# OLD VERSION ------------------------------------------------------------------

# NOTE: Run the analysis for each HW ban cohort first. 

# TWFE - HW (2008) -------------------------------------------------------------

# Create "clean" dataset: Drop all other ban states

acs_hw_2008 <- nca_acs_soc_1pct %>%
  filter(year_eff_ban %in% c(0, 2008), is.na(hw_ban) | hw_ban == 1) %>%
  select(keep_vars, outcome_vars)

# 1) Wage

  # a) All 

est_wage_hw_08 <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_hw_2008, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_hw_08)
rm(est_wage_hw_08)

  # b) Young 

est_wage_hw_08_young <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_hw_2008, 
  subset = ~ young_adult == 1,
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_hw_08_young)
rm(est_wage_hw_08_young)

# 2) Age 

# a) Age (proper)

est_age_hw_08 <- feols(
  age ~ treated_eff | statefip + year,
  data = acs_hw_2008, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_age_hw_08)
rm(est_age_hw_08)

# b) Young 

est_ya_hw_08 <- feols(
  young_adult ~ treated_eff | statefip + year,
  data = acs_hw_2008, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ya_hw_08)
rm(est_ya_hw_08)

# c) Early Career 

est_ec_hw_08 <- feols(
  earlyc_adult ~ treated_eff | statefip + year,
  data = acs_hw_2008, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ec_hw_08)
rm(est_ec_hw_08)

# d) Mid-to-Late Career 

est_mlc_hw_08 <- feols(
  mlc_adult ~ treated_eff | statefip + year,
  data = acs_hw_2008, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_mlc_hw_08)
rm(est_mlc_hw_08)

# e) Near Retirement 

est_old_hw_08 <- feols(
  older_adult ~ treated_eff | statefip + year,
  data = acs_hw_2008, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_old_hw_08)
rm(est_old_hw_08)

# 3) Potential Experience 

est_pot_exp_hw_08 <- feols(
  pot_exp ~ treated_eff | statefip + year,
  data = acs_hw_2008, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_pot_exp_hw_08)
rm(est_pot_exp_hw_08)

# 4) Education 

# a) No High School

est_nhs_hw_08 <- feols(
  no_high_school ~ treated_eff | statefip + year,
  data = acs_hw_2008, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_nhs_hw_08)
rm(est_nhs_hw_08)

# b) High School 

est_hs_hw_08 <- feols(
  high_school ~ treated_eff | statefip + year,
  data = acs_hw_2008, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_hs_hw_08)
rm(est_hs_hw_08)

# c) Some College 

est_scoll_hw_08 <- feols(
  some_college ~ treated_eff | statefip + year,
  data = acs_hw_2008, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_scoll_hw_08)
rm(est_scoll_hw_08)

# d) College 

est_coll_hw_08 <- feols(
  college ~ treated_eff | statefip + year,
  data = acs_hw_2008, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_coll_hw_08)
rm(est_coll_hw_08)

rm(acs_hw_2008)


# TWFE - HW (2020) -------------------------------------------------------------

# Create "clean" dataset: Drop all other ban states

acs_hw_2020 <- nca_acs_soc_1pct %>%
  filter(year_eff_ban %in% c(0, 2020), is.na(hw_ban) | hw_ban == 1) %>%
  select(keep_vars, outcome_vars)

# 1) Wage

# a) All 

est_wage_hw_20 <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_hw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_hw_20)
rm(est_wage_hw_20)

# b) Young 

est_wage_hw_20_young <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_hw_2020, 
  subset = ~ young_adult == 1,
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_hw_20_young)
rm(est_wage_hw_20_young)

# 2) Age 

# a) Age (proper)

est_age_hw_20 <- feols(
  age ~ treated_eff | statefip + year,
  data = acs_hw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_age_hw_20)
rm(est_age_hw_20)

# b) Young 

est_ya_hw_20 <- feols(
  young_adult ~ treated_eff | statefip + year,
  data = acs_hw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ya_hw_20)
rm(est_ya_hw_20)

# c) Early Career 

est_ec_hw_20 <- feols(
  earlyc_adult ~ treated_eff | statefip + year,
  data = acs_hw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ec_hw_20)
rm(est_ec_hw_20)

# d) Mid-to-Late Career 

est_mlc_hw_20 <- feols(
  mlc_adult ~ treated_eff | statefip + year,
  data = acs_hw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_mlc_hw_20)
rm(est_mlc_hw_20)

# e) Near Retirement 

est_old_hw_20 <- feols(
  older_adult ~ treated_eff | statefip + year,
  data = acs_hw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_old_hw_20)
rm(est_old_hw_20)

# 3) Potential Experience 

est_pot_exp_hw_20 <- feols(
  pot_exp ~ treated_eff | statefip + year,
  data = acs_hw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_pot_exp_hw_20)
rm(est_pot_exp_hw_20)

# 4) Education 

# a) No High School

est_nhs_hw_20 <- feols(
  no_high_school ~ treated_eff | statefip + year,
  data = acs_hw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_nhs_hw_20)
rm(est_nhs_hw_20)

# b) High School 

est_hs_hw_20 <- feols(
  high_school ~ treated_eff | statefip + year,
  data = acs_hw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_hs_hw_20)
rm(est_hs_hw_20)

# c) Some College 

est_scoll_hw_20 <- feols(
  some_college ~ treated_eff | statefip + year,
  data = acs_hw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_scoll_hw_20)
rm(est_scoll_hw_20)

# d) College 

est_coll_hw_20 <- feols(
  college ~ treated_eff | statefip + year,
  data = acs_hw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_coll_hw_20)
rm(est_coll_hw_20)

rm(acs_hw_2020)



# TWFE - HW (2022) -------------------------------------------------------------

# Create "clean" dataset: Drop all other ban states

acs_hw_2022 <- nca_acs_soc_1pct %>%
  filter(year_eff_ban %in% c(0, 2022), is.na(hw_ban) | hw_ban == 1) %>%
  select(keep_vars, outcome_vars)

# 1) Wage

# a) All 

est_wage_hw_22 <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_hw_2022, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_hw_22)
rm(est_wage_hw_22)

# b) Young 

est_wage_hw_22_young <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_hw_2022, 
  subset = ~ young_adult == 1,
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_hw_22_young)
rm(est_wage_hw_22_young)

# 2) Age 

# a) Age (proper)

est_age_hw_22 <- feols(
  age ~ treated_eff | statefip + year,
  data = acs_hw_2022, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_age_hw_22)
rm(est_age_hw_22)

# b) Young 

est_ya_hw_22 <- feols(
  young_adult ~ treated_eff | statefip + year,
  data = acs_hw_2022, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ya_hw_22)
rm(est_ya_hw_22)

# c) Early Career 

est_ec_hw_22 <- feols(
  earlyc_adult ~ treated_eff | statefip + year,
  data = acs_hw_2022, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ec_hw_22)
rm(est_ec_hw_22)

# d) Mid-to-Late Career 

est_mlc_hw_22 <- feols(
  mlc_adult ~ treated_eff | statefip + year,
  data = acs_hw_2022, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_mlc_hw_22)
rm(est_mlc_hw_22)

# e) Near Retirement 

est_old_hw_22 <- feols(
  older_adult ~ treated_eff | statefip + year,
  data = acs_hw_2022, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_old_hw_22)
rm(est_old_hw_22)

# 3) Potential Experience 

est_pot_exp_hw_22 <- feols(
  pot_exp ~ treated_eff | statefip + year,
  data = acs_hw_2022, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_pot_exp_hw_22)
rm(est_pot_exp_hw_22)

# 4) Education 

# a) No High School

est_nhs_hw_22 <- feols(
  no_high_school ~ treated_eff | statefip + year,
  data = acs_hw_2022, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_nhs_hw_22)
rm(est_nhs_hw_22)

# b) High School 

est_hs_hw_22 <- feols(
  high_school ~ treated_eff | statefip + year,
  data = acs_hw_2022, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_hs_hw_22)
rm(est_hs_hw_22)

# c) Some College 

est_scoll_hw_22 <- feols(
  some_college ~ treated_eff | statefip + year,
  data = acs_hw_2022, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_scoll_hw_22)
rm(est_scoll_hw_22)

# d) College 

est_coll_hw_22 <- feols(
  college ~ treated_eff | statefip + year,
  data = acs_hw_2022, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_coll_hw_22)
rm(est_coll_hw_22)

rm(acs_hw_2022)




# TWFE - LW (2017) -------------------------------------------------------------

# Create "clean" dataset: Drop all other ban states

acs_lw_2017 <- nca_acs_soc_1pct %>%
  filter(year_eff_ban %in% c(0, 2017), is.na(hw_ban) | hw_ban == 0) %>%
  select(keep_vars, outcome_vars)

# 1) Wage

# a) All 

est_wage_lw_17 <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_lw_2017, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_lw_17)
rm(est_wage_lw_17)

# b) Young 

est_wage_lw_17_young <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_lw_2017, 
  subset = ~ young_adult == 1,
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_lw_17_young)
rm(est_wage_lw_17_young)

# 2) Age 

# a) Age (proper)

est_age_lw_17 <- feols(
  age ~ treated_eff | statefip + year,
  data = acs_lw_2017, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_age_lw_17)
rm(est_age_lw_17)

# b) Young 

est_ya_lw_17 <- feols(
  young_adult ~ treated_eff | statefip + year,
  data = acs_lw_2017, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ya_lw_17)
rm(est_ya_lw_17)

# c) Early Career 

est_ec_lw_17 <- feols(
  earlyc_adult ~ treated_eff | statefip + year,
  data = acs_lw_2017, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ec_lw_17)
rm(est_ec_lw_17)

# d) Mid-to-Late Career 

est_mlc_lw_17 <- feols(
  mlc_adult ~ treated_eff | statefip + year,
  data = acs_lw_2017, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_mlc_lw_17)
rm(est_mlc_lw_17)

# e) Near Retirement 

est_old_lw_17 <- feols(
  older_adult ~ treated_eff | statefip + year,
  data = acs_lw_2017, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_old_lw_17)
rm(est_old_lw_17)

# 3) Potential Experience 

est_pot_exp_lw_17 <- feols(
  pot_exp ~ treated_eff | statefip + year,
  data = acs_lw_2017, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_pot_exp_lw_17)
rm(est_pot_exp_lw_17)

# 4) Education 

# a) No High School

est_nhs_lw_17 <- feols(
  no_high_school ~ treated_eff | statefip + year,
  data = acs_lw_2017, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_nhs_lw_17)
rm(est_nhs_lw_17)

# b) High School 

est_hs_lw_17 <- feols(
  high_school ~ treated_eff | statefip + year,
  data = acs_lw_2017, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_hs_lw_17)
rm(est_hs_lw_17)

# c) Some College 

est_scoll_lw_17 <- feols(
  some_college ~ treated_eff | statefip + year,
  data = acs_lw_2017, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_scoll_lw_17)
rm(est_scoll_lw_17)

# d) College 

est_coll_lw_17 <- feols(
  college ~ treated_eff | statefip + year,
  data = acs_lw_2017, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_coll_lw_17)
rm(est_coll_lw_17)

rm(acs_lw_2017)

# NOTE: It appears here we aren't seeing a major effect on the hiring of young
# workers, but we are seeing a negative effect on young workers wages. 


# TWFE - LW (2018) -------------------------------------------------------------

# Create "clean" dataset: Drop all other ban states

acs_lw_2018 <- nca_acs_soc_1pct %>%
  filter(year_eff_ban %in% c(0, 2018), is.na(hw_ban) | hw_ban == 0) %>%
  select(keep_vars, outcome_vars)

# 1) Wage

# a) All 

est_wage_lw_18 <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_lw_2018, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_lw_18)
rm(est_wage_lw_18)

# b) Young 

est_wage_lw_18_young <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_lw_2018, 
  subset = ~ young_adult == 1,
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_lw_18_young)
rm(est_wage_lw_18_young)

# 2) Age 

# a) Age (proper)

est_age_lw_18 <- feols(
  age ~ treated_eff | statefip + year,
  data = acs_lw_2018, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_age_lw_18)
rm(est_age_lw_18)

# b) Young 

est_ya_lw_18 <- feols(
  young_adult ~ treated_eff | statefip + year,
  data = acs_lw_2018, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ya_lw_18)
rm(est_ya_lw_18)

# c) Early Career 

est_ec_lw_18 <- feols(
  earlyc_adult ~ treated_eff | statefip + year,
  data = acs_lw_2018, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ec_lw_18)
rm(est_ec_lw_18)

# d) Mid-to-Late Career 

est_mlc_lw_18 <- feols(
  mlc_adult ~ treated_eff | statefip + year,
  data = acs_lw_2018, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_mlc_lw_18)
rm(est_mlc_lw_18)

# e) Near Retirement 

est_old_lw_18 <- feols(
  older_adult ~ treated_eff | statefip + year,
  data = acs_lw_2018, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_old_lw_18)
rm(est_old_lw_18)

# 3) Potential Experience 

est_pot_exp_lw_18 <- feols(
  pot_exp ~ treated_eff | statefip + year,
  data = acs_lw_2018, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_pot_exp_lw_18)
rm(est_pot_exp_lw_18)

# 4) Education 

# a) No High School

est_nhs_lw_18 <- feols(
  no_high_school ~ treated_eff | statefip + year,
  data = acs_lw_2018, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_nhs_lw_18)
rm(est_nhs_lw_18)

# b) High School 

est_hs_lw_18 <- feols(
  high_school ~ treated_eff | statefip + year,
  data = acs_lw_2018, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_hs_lw_18)
rm(est_hs_lw_18)

# c) Some College 

est_scoll_lw_18 <- feols(
  some_college ~ treated_eff | statefip + year,
  data = acs_lw_2018, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_scoll_lw_18)
rm(est_scoll_lw_18)

# d) College 

est_coll_lw_18 <- feols(
  college ~ treated_eff | statefip + year,
  data = acs_lw_2018, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_coll_lw_18)
rm(est_coll_lw_18)

rm(acs_lw_2018)


# TWFE - LW (2019) -------------------------------------------------------------

# Create "clean" dataset: Drop all other ban states

acs_lw_2019 <- nca_acs_soc_1pct %>%
  filter(year_eff_ban %in% c(0, 2019), is.na(hw_ban) | hw_ban == 0) %>%
  select(keep_vars, outcome_vars)

# 1) Wage

# a) All 

est_wage_lw_19 <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_lw_2019, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_lw_19)
rm(est_wage_lw_19)

# b) Young 

est_wage_lw_19_young <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_lw_2019, 
  subset = ~ young_adult == 1,
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_lw_19_young)
rm(est_wage_lw_19_young)

# 2) Age 

# a) Age (proper)

est_age_lw_19 <- feols(
  age ~ treated_eff | statefip + year,
  data = acs_lw_2019, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_age_lw_19)
rm(est_age_lw_19)

# b) Young 

est_ya_lw_19 <- feols(
  young_adult ~ treated_eff | statefip + year,
  data = acs_lw_2019, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ya_lw_19)
rm(est_ya_lw_19)

# c) Early Career 

est_ec_lw_19 <- feols(
  earlyc_adult ~ treated_eff | statefip + year,
  data = acs_lw_2019, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ec_lw_19)
rm(est_ec_lw_19)

# d) Mid-to-Late Career 

est_mlc_lw_19 <- feols(
  mlc_adult ~ treated_eff | statefip + year,
  data = acs_lw_2019, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_mlc_lw_19)
rm(est_mlc_lw_19)

# e) Near Retirement 

est_old_lw_19 <- feols(
  older_adult ~ treated_eff | statefip + year,
  data = acs_lw_2019, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_old_lw_19)
rm(est_old_lw_19)

# 3) Potential Experience 

est_pot_exp_lw_19 <- feols(
  pot_exp ~ treated_eff | statefip + year,
  data = acs_lw_2019, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_pot_exp_lw_19)
rm(est_pot_exp_lw_19)

# 4) Education 

# a) No High School

est_nhs_lw_19 <- feols(
  no_high_school ~ treated_eff | statefip + year,
  data = acs_lw_2019, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_nhs_lw_19)
rm(est_nhs_lw_19)

# b) High School 

est_hs_lw_19 <- feols(
  high_school ~ treated_eff | statefip + year,
  data = acs_lw_2019, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_hs_lw_19)
rm(est_hs_lw_19)

# c) Some College 

est_scoll_lw_19 <- feols(
  some_college ~ treated_eff | statefip + year,
  data = acs_lw_2019, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_scoll_lw_19)
rm(est_scoll_lw_19)

# d) College 

est_coll_lw_19 <- feols(
  college ~ treated_eff | statefip + year,
  data = acs_lw_2019, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_coll_lw_19)
rm(est_coll_lw_19)

rm(acs_lw_2019)


# TWFE - LW (2020) -------------------------------------------------------------

# Create "clean" dataset: Drop all other ban states

acs_lw_2020 <- nca_acs_soc_1pct %>%
  filter(year_eff_ban %in% c(0, 2020), is.na(hw_ban) | hw_ban == 0) %>%
  select(keep_vars, outcome_vars)

# 1) Wage

# a) All 

est_wage_lw_20 <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_lw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_lw_20)
rm(est_wage_lw_20)

# b) Young 

est_wage_lw_20_young <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_lw_2020, 
  subset = ~ young_adult == 1,
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_lw_20_young)
rm(est_wage_lw_20_young)

# 2) Age 

# a) Age (proper)

est_age_lw_20 <- feols(
  age ~ treated_eff | statefip + year,
  data = acs_lw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_age_lw_20)
rm(est_age_lw_20)

# b) Young 

est_ya_lw_20 <- feols(
  young_adult ~ treated_eff | statefip + year,
  data = acs_lw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ya_lw_20)
rm(est_ya_lw_20)

# c) Early Career 

est_ec_lw_20 <- feols(
  earlyc_adult ~ treated_eff | statefip + year,
  data = acs_lw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ec_lw_20)
rm(est_ec_lw_20)

# d) Mid-to-Late Career 

est_mlc_lw_20 <- feols(
  mlc_adult ~ treated_eff | statefip + year,
  data = acs_lw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_mlc_lw_20)
rm(est_mlc_lw_20)

# e) Near Retirement 

est_old_lw_20 <- feols(
  older_adult ~ treated_eff | statefip + year,
  data = acs_lw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_old_lw_20)
rm(est_old_lw_20)

# 3) Potential Experience 

est_pot_exp_lw_20 <- feols(
  pot_exp ~ treated_eff | statefip + year,
  data = acs_lw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_pot_exp_lw_20)
rm(est_pot_exp_lw_20)

# 4) Education 

# a) No High School

est_nhs_lw_20 <- feols(
  no_high_school ~ treated_eff | statefip + year,
  data = acs_lw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_nhs_lw_20)
rm(est_nhs_lw_20)

# b) High School 

est_hs_lw_20 <- feols(
  high_school ~ treated_eff | statefip + year,
  data = acs_lw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_hs_lw_20)
rm(est_hs_lw_20)

# c) Some College 

est_scoll_lw_20 <- feols(
  some_college ~ treated_eff | statefip + year,
  data = acs_lw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_scoll_lw_20)
rm(est_scoll_lw_20)

# d) College 

est_coll_lw_20 <- feols(
  college ~ treated_eff | statefip + year,
  data = acs_lw_2020, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_coll_lw_20)
rm(est_coll_lw_20)

rm(acs_lw_2020)


# TWFE - LW (2021) -------------------------------------------------------------

# Create "clean" dataset: Drop all other ban states

acs_lw_2021 <- nca_acs_soc_1pct %>%
  filter(year_eff_ban %in% c(0, 2021), is.na(hw_ban) | hw_ban == 0) %>%
  select(keep_vars, outcome_vars)

# 1) Wage

# a) All 

est_wage_lw_21 <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_lw_2021, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_lw_21)
rm(est_wage_lw_21)

# b) Young 

est_wage_lw_21_young <- feols(
  incwage ~ treated_eff | statefip + year,
  data = acs_lw_2021, 
  subset = ~ young_adult == 1,
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_wage_lw_21_young)
rm(est_wage_lw_21_young)

# 2) Age 

# a) Age (proper)

est_age_lw_21 <- feols(
  age ~ treated_eff | statefip + year,
  data = acs_lw_2021, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_age_lw_21)
rm(est_age_lw_21)

# b) Young 

est_ya_lw_21 <- feols(
  young_adult ~ treated_eff | statefip + year,
  data = acs_lw_2021, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ya_lw_21)
rm(est_ya_lw_21)

# c) Early Career 

est_ec_lw_21 <- feols(
  earlyc_adult ~ treated_eff | statefip + year,
  data = acs_lw_2021, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_ec_lw_21)
rm(est_ec_lw_21)

# d) Mid-to-Late Career 

est_mlc_lw_21 <- feols(
  mlc_adult ~ treated_eff | statefip + year,
  data = acs_lw_2021, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_mlc_lw_21)
rm(est_mlc_lw_21)

# e) Near Retirement 

est_old_lw_21 <- feols(
  older_adult ~ treated_eff | statefip + year,
  data = acs_lw_2021, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_old_lw_21)
rm(est_old_lw_21)

# 3) Potential Experience 

est_pot_exp_lw_21 <- feols(
  pot_exp ~ treated_eff | statefip + year,
  data = acs_lw_2021, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_pot_exp_lw_21)
rm(est_pot_exp_lw_21)

# 4) Education 

# a) No High School

est_nhs_lw_21 <- feols(
  no_high_school ~ treated_eff | statefip + year,
  data = acs_lw_2021, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_nhs_lw_21)
rm(est_nhs_lw_21)

# b) High School 

est_hs_lw_21 <- feols(
  high_school ~ treated_eff | statefip + year,
  data = acs_lw_2021, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_hs_lw_21)
rm(est_hs_lw_21)

# c) Some College 

est_scoll_lw_21 <- feols(
  some_college ~ treated_eff | statefip + year,
  data = acs_lw_2021, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_scoll_lw_21)
rm(est_scoll_lw_21)

# d) College 

est_coll_lw_21 <- feols(
  college ~ treated_eff | statefip + year,
  data = acs_lw_2021, 
  cluster = ~ statefip,
  weights = ~ perwt
)

summary(est_coll_lw_21)
rm(est_coll_lw_21)

rm(acs_lw_2021)



