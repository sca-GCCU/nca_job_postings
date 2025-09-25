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
library(rlang)

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
  "results_acs_1pct"
)
if (!dir.exists(res_dir)) dir.create(res_dir, recursive = TRUE)

plot_dir  <- file.path(res_dir, "plots")
table_dir <- file.path(res_dir, "tables")
obj_dir   <- file.path(res_dir, "objects")

for (d in c(plot_dir, table_dir, obj_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

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

# Tabulate Groups 
#nca_acs_soc_1pct %>%
#  count(year_eff_ban, name = "count")

# I) SET-UP 
# a) Subsetting for Group-Time Plots 

subset1 <- c("2008", "2017", "2018")
subset2 <- c("2019", "2020", "2021")
subset3 <- c("2022") # I'll need to revise the code so that this plot has proper height

# b) Function to extract event-study vectors (regardless of object names)

get_event_df <- function(ev_obj) {
  # dynamic aggte usually stores these as egt/att.egt/se.egt
  nm <- names(ev_obj)
  e   <- ev_obj[[if ("egt"      %in% nm) "egt"      else "e"]]
  att <- ev_obj[[if ("att.egt"  %in% nm) "att.egt"  else "att"]]
  se  <- ev_obj[[if ("se.egt"   %in% nm) "se.egt"   else "se"]]
  tibble(e = e, att = att, se = se)
}

# II) Function to Run DID (fixed)
run_did_for_y <- function(yvar, 
                          data = nca_acs_soc_1pct,  # CHANGE IN CLUSTER
                          tname = "year",
                          gname = "year_eff_ban",
                          panel = FALSE, 
                          bstrap = TRUE, 
                          cband = TRUE, 
                          clustervars = "statefip",
                          min_e = -5, max_e = 5) {
  yvar <- as.character(yvar)
  message("Running DID for outcome: ", yvar)
  
  # A) Group-time ATTs
  gt <- did::att_gt(
    yname      = yvar, 
    tname      = tname, 
    gname      = gname, 
    data       = data, 
    panel      = panel, 
    bstrap     = bstrap, 
    cband      = cband, 
    clustervars = clustervars
  )
  
  # B) Group-time plots  
  p_gt_1 <- ggdid(gt, group = subset1, title = paste0(yvar, " - subset 1")) + 
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  
  p_gt_2 <- ggdid(gt, group = subset2, title = paste0(yvar, " - subset 2")) + 
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  
  p_gt_3 <- ggdid(gt, group = subset3, title = paste0(yvar, " - subset 3")) + 
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  
  # C) Simple aggregation 
  agg_simple <- aggte(gt, type = "simple")
  
  # D) Event-study aggregation + plot 
  agg_event <- aggte(gt, type = "dynamic", min_e = min_e, max_e = max_e)
  p_event <- ggdid(agg_event) + 
    ggtitle(paste0(yvar, " - event study")) +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  
  # Tidy-ish tables 
  gt_df <- tibble(
    y    = yvar, 
    group = gt$group, 
    t     = gt$t, 
    att   = gt$att, 
    se    = gt$se
  )
  
  simple_df <- tibble(
    y           = yvar,
    overall_att = agg_simple$overall.att,
    overall_se  = agg_simple$overall.se,
    overall_p   = agg_simple$overall.p
  )
  
  event_df <- get_event_df(agg_event) %>% dplyr::mutate(y = yvar, .before = 1)
  
  list(
    y          = yvar,
    gt         = gt,
    agg_simple = agg_simple,
    agg_event  = agg_event,
    plots      = list(gt_subset1 = p_gt_1, gt_subset2 = p_gt_2, gt_subset3 = p_gt_3, event = p_event),
    tables     = list(gt = gt_df, simple = simple_df, event = event_df)
  )
}

# III) Run across all outcomes
did_results <- outcome_vars %>%
  set_names() %>%
  map(~ tryCatch(run_did_for_y(.x),
                        error = function(e) { warning("Failed for ", .x, ": ", e$message); NULL }))

# TRY FOR JUST ONE OUTCOME TO MAKE SURE ITS WORKING.


# IV) Combine tidy tables across outcomes (skip failures safely)
did_results_ok <- compact(did_results)

gt_all     <- map(did_results_ok, ~ .x$tables$gt)     |> dplyr::bind_rows()
simple_all <- map(did_results_ok, ~ .x$tables$simple) |> dplyr::bind_rows()
event_all  <- map(did_results_ok, ~ .x$tables$event)  |> dplyr::bind_rows()

# ADD PLOTS AND CODE FOR SAVING CSV FILES 


# ADD HW AND LW PARTS OF CODE 





