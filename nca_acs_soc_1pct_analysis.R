# PREAMBLE ---------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(did)
library(haven)
library(srvyr)
library(survey)
library(purrr)
library(readr)
library(tibble)

set.seed(1390)

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/data")


# LOAD DATA --------------------------------------------------------------------
nca_acs_soc_1pct <- read_dta("nca_acs_soc_1pct.dta")

# VECTORS ----------------------------------------------------------------------
outcome_vars <- c("age", "pot_exp", "young_adult", "earlyc_adult", "mlc_adult", 
                  "older_adult", "yrschool", "no_high_school", "high_school",
                  "some_college", "college")

control_vars <- c("employment_sa", "income_pcap", "hpi", "male", "black")

combined_vars <- c(outcome_vars, control_vars)

# RESULTS DIR ------------------------------------------------------------------
res_dir <- file.path(
  "C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings",
  "results_acs"
)
if (!dir.exists(res_dir)) dir.create(res_dir, recursive = TRUE)

# SUMMARY STATISTICS (srvyr) ---------------------------------------------------
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

# SAVE SUMMARY STATS
write_csv(summary_srvyr, file.path(res_dir, "summary_srvyr.csv"))
saveRDS(summary_srvyr, file.path(res_dir, "summary_srvyr.rds"))

# DESIGN-BASED TWO-SAMPLE T-TESTS (survey::svyttest) ---------------------------
nca_acs_soc_1pct[combined_vars] <- lapply(nca_acs_soc_1pct[combined_vars], as.numeric)

des_svy <- svydesign(ids = ~1, weights = ~perwt, data = nca_acs_soc_1pct)

tidy_svyttest <- function(var, design, rhs = "factor(ban)") {
  fml <- as.formula(paste(var, "~", rhs))
  tt  <- svyttest(fml, design = design)
  
  est  <- unname(tt$estimate)            # mean(ban==1) - mean(ban==0)
  tval <- unname(tt$statistic)
  df   <- unname(tt$parameter)
  pval <- unname(tt$p.value)
  ci   <- unname(tt$conf.int)
  se   <- if (!is.null(tt$stderr)) unname(tt$stderr)
  else if (is.finite(tval) && tval != 0) abs(est / tval) else NA_real_
  
  tibble(
    variable   = var,
    diff_mean  = as.numeric(est),
    se         = as.numeric(se),
    t          = as.numeric(tval),
    df         = as.numeric(df),
    p_value    = as.numeric(pval),
    conf_low   = if (length(ci) >= 1) ci[1] else NA_real_,
    conf_high  = if (length(ci) >= 2) ci[2] else NA_real_,
    method     = tt$method,
    formula    = tt$data.name
  )
}

# Run with safety
safe_tidy <- purrr::safely(function(v) tidy_svyttest(v, des_svy))
out_list  <- map(combined_vars, safe_tidy)

svy_ttests_results <- bind_rows(compact(map(out_list, "result")))

errors <- tibble(
  variable = combined_vars,
  error    = map_chr(out_list, ~ if (is.null(.x$error)) "" else .x$error$message)
) |> filter(error != "")

# SAVE T-TEST RESULTS + ERROR LOG
write_csv(svy_ttests_results, file.path(res_dir, "svy_ttests_results.csv"))
saveRDS(svy_ttests_results, file.path(res_dir, "svy_ttests_results.rds"))

if (nrow(errors) > 0) {
  write_csv(errors, file.path(res_dir, "svy_ttests_errors.csv"))
  saveRDS(errors, file.path(res_dir, "svy_ttests_errors.rds"))
}

# OPTIONAL: message
cat("Files written to:", res_dir, "\n")


# CS DID -----------------------------------------------------------------------




