# PREAMBLE ---------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(haven)
library(srvyr)
library(survey)
library(readr)
library(purrr)
library(tibble)

set.seed(1390)
setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/data")

# LOAD DATA --------------------------------------------------------------------
nca_acs_soc_1pct <- read_dta("nca_acs_soc_1pct.dta")

# VECTORS ----------------------------------------------------------------------
outcome_vars <- c("age","early_career", "mid_career", "late_career", "no_high_school","high_school",
                  "some_college","college")
control_vars <- c("employment_sa_l1","inc_pcap_r_l1","hpi_r_l1")
combined_vars <- c(outcome_vars, control_vars)

# RESULTS DIRS -----------------------------------------------------------------
res_dir <- file.path(
  "C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings",
  "results_acs_1pct"
)
if (!dir.exists(res_dir)) dir.create(res_dir, recursive = TRUE)

sum_stats_dir <- file.path(res_dir, "sum_stats_R")
if (!dir.exists(sum_stats_dir)) dir.create(sum_stats_dir, recursive = TRUE)

# SUMMARY STATS (srvyr) --------------------------------------------------------
des <- nca_acs_soc_1pct %>%
  mutate(across(all_of(combined_vars), as.numeric)) %>%  # drop haven labels
  as_survey(weights = perwt)

summary_srvyr <- des %>%
  group_by(ban) %>%
  summarise(
    across(
      all_of(combined_vars),
      list(
        mean = ~ as.numeric(survey_mean(.x, na.rm = TRUE, vartype = NULL)),
        sd   = ~ as.numeric(survey_sd(.x,   na.rm = TRUE, vartype = NULL)),
        n    = ~ unweighted(sum(!is.na(.x)))
      ),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

write_csv(summary_srvyr, file.path(sum_stats_dir, "summary_srvyr.csv"))
saveRDS(summary_srvyr, file.path(sum_stats_dir, "summary_srvyr.rds"))

# DESIGN-BASED TWO-SAMPLE T-TESTS ---------------------------------------------
nca_acs_soc_1pct[combined_vars] <- lapply(nca_acs_soc_1pct[combined_vars], as.numeric)
des_svy <- svydesign(ids = ~1, weights = ~perwt, data = nca_acs_soc_1pct)

tidy_svyttest <- function(var, design, rhs = "factor(ban)") {
  fml <- as.formula(paste(var, "~", rhs))
  tt  <- svyttest(fml, design = design)
  est  <- unname(tt$estimate)
  tval <- unname(tt$statistic)
  df   <- unname(tt$parameter)
  pval <- unname(tt$p.value)
  ci   <- unname(tt$conf.int)
  se   <- if (!is.null(tt$stderr)) unname(tt$stderr)
  else if (is.finite(tval) && tval != 0) abs(est / tval) else NA_real_
  tibble(
    variable  = var,
    diff_mean = as.numeric(est),
    se        = as.numeric(se),
    t         = as.numeric(tval),
    df        = as.numeric(df),
    p_value   = as.numeric(pval),
    conf_low  = if (length(ci) >= 1) ci[1] else NA_real_,
    conf_high = if (length(ci) >= 2) ci[2] else NA_real_,
    method    = tt$method,
    formula   = tt$data.name
  )
}

safe_tidy <- purrr::safely(function(v) tidy_svyttest(v, des_svy))
out_list  <- map(combined_vars, safe_tidy)
svy_ttests_results <- bind_rows(compact(map(out_list, "result")))

errors <- tibble(
  variable = combined_vars,
  error    = map_chr(out_list, ~ if (is.null(.x$error)) "" else .x$error$message)
) |> filter(error != "")

write_csv(svy_ttests_results, file.path(sum_stats_dir, "svy_ttests_results.csv"))
saveRDS(svy_ttests_results, file.path(sum_stats_dir, "svy_ttests_results.rds"))

if (nrow(errors) > 0) {
  write_csv(errors, file.path(sum_stats_dir, "svy_ttests_errors.csv"))
  saveRDS(errors, file.path(sum_stats_dir, "svy_ttests_errors.rds"))
}

