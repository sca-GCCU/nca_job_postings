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
# 1. UNCONDITIONAL, ALL BANS ---------------------------------------------------

# Tabulate Groups 
#nca_acs_soc_1pct %>%
#  count(year_eff_ban, name = "count")

# I) SET-UP 
# a) Subsetting for Group-Time Plots 

subset1 <- c(2008L, 2017L, 2018L)
subset2 <- c(2019L, 2020L, 2021L)
subset3 <- 2022L # I'll need to revise the code so that this plot has proper height

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
                          weightsname = "perwt",
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
    clustervars = clustervars,
    weightsname = weightsname
  )
  
  # B) Group-time plots  
  p_gt_1 <- ggdid(gt, group = subset1, title = paste0(yvar, " - subset 1")) + 
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  attr(p_gt_1, "n_groups") <- length(subset1)
  
  p_gt_2 <- ggdid(gt, group = subset2, title = paste0(yvar, " - subset 2")) + 
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  attr(p_gt_2, "n_groups") <- length(subset2)
  
  p_gt_3 <- ggdid(gt, group = subset3, title = paste0(yvar, " - subset 3")) + 
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  attr(p_gt_3, "n_groups") <- length(subset3)
  
  # C) Simple aggregation 
  agg_simple <- aggte(gt, type = "simple")
  
  # D) Event-study aggregation + plot 
  agg_event <- aggte(gt, type = "dynamic", min_e = min_e, max_e = max_e)
  
  p_event <- ggdid(agg_event) + 
    ggtitle(paste0(yvar, " - event study")) +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  attr(p_event, "n_groups") <- 1L   # single panel
  
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
    overall_p   = agg_simple$overall.p,
    n_total_unit = gt$n   # total units used
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
did_results <- outcome_vars %>% # Can add [1] to test on the first outcome var
  set_names() %>%
  map(~ tryCatch(run_did_for_y(.x),
                        error = function(e) { warning("Failed for ", .x, ": ", e$message); NULL }))

# IV) Combine tidy tables across outcomes (skip failures safely)
did_results_ok <- compact(did_results)

gt_all     <- map(did_results_ok, ~ .x$tables$gt)     |> dplyr::bind_rows()
simple_all <- map(did_results_ok, ~ .x$tables$simple) |> dplyr::bind_rows()
event_all  <- map(did_results_ok, ~ .x$tables$event)  |> dplyr::bind_rows()


# V) Saving the Results 

# i) Helper to calculate number of panels 

num_panels <- function(p) {
  if (!is.null(attr(p, "n_groups"))) return(attr(p, "n_groups"))
  # fall-back if I add plots and don't attach n_group attributes 
  b <- ggplot_build(p)
  if (!is.null(b$layout$layout$PANEL)) {
    return(unique(length(b$layout$layout$PANEL)))
  }
  1L
}

# ii) Helper to calculate height 

calc_height <- function(n, base = 2.0, per_group = 1.2, max_height = 14) {
  height <- base + per_group * n
  pmin(height, max_height)
}

# iii) Plot saver 

save_plot <- function(p, file, pname, 
                      width = 7,
                      base_height = 2.0,
                      per_group = 1.2,
                      max_height = 14,
                      dpi = 300) {
  
  n <- num_panels(p)
  height <- calc_height(n, base = base_height, per_group = per_group, max_height = max_height)
  
  p <- p + theme(plot.margin = margin(t = 12, r = 6, b = 12, l = 8))
  
  ggplot2::ggsave(
    filename = file,
    plot     = p,
    path     = plot_dir,
    width    = width,
    height   = height,
    dpi      = dpi,
    units    = "in"
  )
}

# iv) Iterate outcomes and their plot lists 
iwalk(did_results_ok, function(res, yname) { 
  if (is.null(res)) return(invisible(NULL)) # skip failures
  iwalk(res$plots, function(p,pname) {
    fname <- paste0(yname, "_", pname, ".png")
    save_plot(p, file = fname, pname = pname)
  })
})

# v) Save objects + tables in results subfolders 
saveRDS(did_results_ok, file = file.path(obj_dir, "did_results.rds"))

write_csv(gt_all, file.path(table_dir, "did_group_time_all_outcomes.csv"))
write_csv(simple_all, file.path(table_dir, "did_simple_agg_all_outcomes.csv"))
write_csv(event_all, file.path(table_dir, "did_event_study_all_outcomes.csv"))


# 2. UNCONDITIONAL, HW BANS ----------------------------------------------------

# Tabulate HW ban groups 

#nca_acs_soc_1pct %>% 
#  filter(hw_ban == 1) %>%
#  distinct(year_eff_ban)
# NOTE: Three the groups. Enough for a single group-time plot.

# I) Subset 

# NOTE: There's only one. I do this to control height of plot.

subset_hw <- c(2008L, 2020L, 2022L)

# II) Filter the data 

hw_1pct <- nca_acs_soc_1pct %>%
  filter(is.na(hw_ban) | hw_ban == 1) # no ban or hw ban included 

# II) New did running function for HW ban 

run_did_for_y_hw <- function(yvar, 
                          data = hw_1pct,  # CHANGE IN CLUSTER
                          tname = "year",
                          gname = "year_eff_ban",
                          panel = FALSE, 
                          bstrap = TRUE, 
                          cband = TRUE, 
                          clustervars = "statefip",
                          weightsname = "perwt",
                          min_e = -5, max_e = 5) {
  yvar <- as.character(yvar)
  message("Running HW-ban DID for outcome: ", yvar)
  
  # A) Group-time ATTs
  gt <- did::att_gt(
    yname      = yvar, 
    tname      = tname, 
    gname      = gname, 
    data       = data, 
    panel      = panel, 
    bstrap     = bstrap, 
    cband      = cband, 
    clustervars = clustervars,
    weightsname = weightsname
  )
  
  # B) Group-time plots  
  p_gt <- ggdid(gt, group = subset_hw, title = paste0(yvar, " - group-time plot")) + 
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  attr(p_gt, "n_groups") <- length(subset_hw)
  
  # C) Simple aggregation 
  agg_simple <- aggte(gt, type = "simple")
  
  # D) Event-study aggregation + plot 
  agg_event <- aggte(gt, type = "dynamic", min_e = min_e, max_e = max_e)
  
  p_event <- ggdid(agg_event) + 
    ggtitle(paste0(yvar, " - event study")) +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  attr(p_event, "n_groups") <- 1L   # single panel
  
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
    overall_p   = agg_simple$overall.p,
    n_total_unit = gt$n   # total units used
  )
  
  event_df <- get_event_df(agg_event) %>% dplyr::mutate(y = yvar, .before = 1)
  
  list(
    y          = yvar,
    gt         = gt,
    agg_simple = agg_simple,
    agg_event  = agg_event,
    plots      = list(gt = p_gt, event = p_event),
    tables     = list(gt = gt_df, simple = simple_df, event = event_df)
  )
}

# III) Run across all outcomes
did_results_hw <- outcome_vars[1] %>% # Can add [1] to test on the first outcome var
  set_names() %>%
  map(~ tryCatch(run_did_for_y_hw(.x),
                 error = function(e) { warning("Failed for ", .x, ": ", e$message); NULL }))

# IV) Combine tidy tables across outcomes (skip failures safely)
did_results_hw_ok <- compact(did_results_hw)

gt_all_hw     <- map(did_results_hw_ok, ~ .x$tables$gt)     |> bind_rows()
simple_all_hw <- map(did_results_hw_ok, ~ .x$tables$simple) |> bind_rows()
event_all_hw  <- map(did_results_hw_ok, ~ .x$tables$event)  |> bind_rows()


# V) Saving the Results 

# i) Plot saver for one strata (HW/LW)

save_did_bundle <- function(did_list,
                            tag,
                            base_dir) {
  stopifnot(is.list(did_list), length(tag) == 1L)
  
  # A) Create dirs
  results_dir <- file.path(base_dir, tag)
  plot_dir    <- file.path(results_dir, "plots")
  table_dir   <- file.path(results_dir, "tables")
  obj_dir     <- file.path(results_dir, "objects")
  for (d in c(plot_dir, table_dir, obj_dir)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  
  # B) Drop NULL outcome results
  did_list_clean <- compact(did_list)
  
  # C) Bind tidy tables defensively (use rlang::`%||%` and coerce NULL->empty tibble)
  gt_all <- bind_rows(map(did_list_clean, ~ `%||%`(.x$tables$gt,     NULL)))
  simple_all <- bind_rows(map(did_list_clean, ~ `%||%`(.x$tables$simple, NULL)))
  event_all  <- bind_rows(map(did_list_clean, ~ `%||%`(.x$tables$event,  NULL)))
  
  if (is.null(gt_all))   gt_all   <- tibble()
  if (is.null(simple_all))  simple_all  <- tibble()
  if (is.null(event_all))   event_all   <- tibble()
  
  # D) Plot saver 
  save_plot_strat <- function(p, file, pname, 
                              width = 7,
                              base_height = 2.0,
                              per_group = 1.2,
                              max_height = 14,
                              dpi = 300) {
    
    n <- num_panels(p)
    height <- calc_height(n, base = base_height, per_group = per_group, max_height = max_height)
    
    p <- p + theme(plot.margin = margin(t = 12, r = 6, b = 12, l = 8))
    
    ggplot2::ggsave(
      filename = file,
      plot     = p,
      path     = plot_dir,
      width    = width,
      height   = height,
      dpi      = dpi,
      units    = "in"
    )
  }
  
  # E) Save plots (one file per outcome x plot kind)
  purrr::iwalk(did_list_clean, function(res, yname) {
    if (is.null(res$plots)) return(invisible(NULL))
    purrr::iwalk(res$plots, function(p, pname) {
      fname <- paste0(tag, "_", yname, "_", pname, ".png")
      save_plot_strat(p, file = fname, pname = pname)
    })
  })
  
  # F) Save objects + tables
  saveRDS(did_list_clean, file = file.path(obj_dir, paste0(tag, "_did_results.rds")))
  if (nrow(gt_all)  > 0) readr::write_csv(gt_all,  file.path(table_dir, paste0(tag, "_did_group_time_all_outcomes.csv")))
  if (nrow(simple_all) > 0) readr::write_csv(simple_all, file.path(table_dir, paste0(tag, "_did_simple_agg_all_outcomes.csv")))
  if (nrow(event_all)  > 0) readr::write_csv(event_all,  file.path(table_dir, paste0(tag, "_did_event_study_all_outcomes.csv")))
  
  # G) Minimal run metadata
  meta <- tibble::tibble(
    tag = tag,
    n_outcomes_requested = length(did_list),
    n_outcomes_success   = length(did_list_clean),
    saved_at             = Sys.time()
  )
  readr::write_csv(meta, file.path(results_dir, paste0(tag, "_run_meta.csv")))
  
  invisible(list(
    results = did_list_clean,
    tables  = list(gt = gt_all, simple = simple_all, event = event_all),
    dirs    = list(results_dir = results_dir, plot_dir = plot_dir, table_dir = table_dir, obj_dir = obj_dir)
  ))
}

# VI) Running the "Save" Function

save_did_bundle(did_list = did_results_hw, tag = "hw", base_dir = res_dir) 


# 3. UNCONDITIONAL, LW BANS ----------------------------------------------------

# Tabulate LW ban groups 

#nca_acs_soc_1pct %>%
#  filter(hw_ban == 0) %>%
#  distinct(year_eff_ban)
# NOTE: Two subsets, which must be scaled differently. 

# I) Filter the data 

lw_1pct <- nca_acs_soc_1pct %>%
  filter(is.na(hw_ban) | hw_ban == 0) # no ban or LW ban included 

# II) Subsets for LW ban group-time plots 

subset1_lw <- c(2017L, 2018L, 2019L)
subset2_lw <- c(2020L, 2021L)

# III) New did running function for LW ban

run_did_for_y_lw <- function(yvar, 
                             data = lw_1pct,
                             tname = "year",
                             gname = "year_eff_ban",
                             panel = FALSE, 
                             bstrap = TRUE, 
                             cband = TRUE, 
                             clustervars = "statefip",
                             weightsname = "perwt",
                             min_e = -5, max_e = 5) {
  yvar <- as.character(yvar)
  message("Running LW-ban DID for outcome: ", yvar)
  
  gt <- did::att_gt(
    yname       = yvar, 
    tname       = tname, 
    gname       = gname, 
    data        = data, 
    panel       = panel, 
    bstrap      = bstrap, 
    cband       = cband, 
    clustervars = clustervars,
    weightsname = weightsname
  )
  
  p_gt_1 <- ggdid(gt, group = subset1_lw, title = paste0(yvar, " - subset 1 group-time plot")) +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  attr(p_gt_1, "n_groups") <- length(subset1_lw)
  
  p_gt_2 <- ggdid(gt, group = subset2_lw, title = paste0(yvar, " - subset 2 group-time plot")) +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  attr(p_gt_2, "n_groups") <- length(subset2_lw)
  
  agg_simple <- aggte(gt, type = "simple")
  agg_event  <- aggte(gt, type = "dynamic", min_e = min_e, max_e = max_e)
  
  p_event <- ggdid(agg_event) +
    ggtitle(paste0(yvar, " - event study")) +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  attr(p_event, "n_groups") <- 1L
  
  gt_df <- tibble(y = yvar, group = gt$group, t = gt$t, att = gt$att, se = gt$se)
  simple_df <- tibble(
    y            = yvar,
    overall_att  = agg_simple$overall.att,
    overall_se   = agg_simple$overall.se,
    overall_p    = agg_simple$overall.p,
    n_total_unit = gt$n
  )
  event_df <- get_event_df(agg_event) %>% dplyr::mutate(y = yvar, .before = 1)
  
  list(
    y          = yvar,
    gt         = gt,
    agg_simple = agg_simple,
    agg_event  = agg_event,
    plots      = list(gt_subset1 = p_gt_1, gt_subset2 = p_gt_2, event = p_event),
    tables     = list(gt = gt_df, simple = simple_df, event = event_df)
  )
}

# IV) Run across all outcomes 

did_results_lw <- outcome_vars %>%
  set_names() %>%
  purrr::map(~ tryCatch(run_did_for_y_lw(.x),
                        error = function(e) { warning("Failed for ", .x, ": ", e$message); NULL }))

# V) Combining tidy tables across outcomes (for inspection in R)

did_results_lw_ok <- compact(did_results_lw)

gt_all_lw     <- bind_rows(map(did_results_lw_ok, ~ .x$tables$gt))
simple_all_lw <- bind_rows(map(did_results_lw_ok, ~ .x$tables$simple))
event_all_lw  <- bind_rows(map(did_results_lw_ok, ~ .x$tables$event))


# VI) Running the "Save" Function 

save_did_bundle(did_results_lw, tag = "lw", base_dir = res_dir)





