# PREAMBLE ---------------------------------------------------------------------

rm(list = ls())

library(did)
library(tidyverse)
library(tigris)
library(purrr)
library(broom)
library(rlang)

set.seed(9284)

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/data")

# LOADING THE DATA -------------------------------------------------------------

# 1. Lightcast

occ_listings <- read.csv("anastasi_data_2010_2025_agg1.csv")  

data(fips_codes)

ref <- fips_codes %>%
  distinct(state_code, state_name) %>%
  mutate(
    state_code = as.integer(state_code)
  )

# Check that "state" is "statefip" 

#checked <- occ_listings %>%
#  left_join(ref, by = c("state" = "state_code")) %>%
#  mutate(
#    name_mismatch = state_name.x != state_name.y, # flag mismatches
#    missing_ref = is.na(state_name.y) # codes not found in ref
#  )

#checked %>%
#  summarize(
#    total = n(),
#    mismatches = sum(name_mismatch, na.rm = TRUE),
#    missing_codes = sum(missing_ref),
#    mismatches_samples = list(unique(state[name_mismatch])),
#    missing_samples = list(unique(state[missing_ref]))
#  )

#checked_inspect <- checked %>% 
#  filter(name_mismatch) %>% distinct(state, state_name.x, state_name.y)

# NOTE: Looks like "state" is indeed "statefip", but the formatting of the 
# state_names variable in the Lightcast dataset is inconsistent. 


# Standardizing the names 

occ_listings <- occ_listings %>%
  rename("statefip" = "state") %>%
  left_join(ref, by = c("statefip" = "state_code")) %>%
  rename("state" = "state_name.y") %>%
  relocate(state, .after = statefip) %>%
  select(-state_name.x)

# State-Occupation IDs

occ_listings <- occ_listings %>%
  mutate(occ_state_id = as.integer(interaction(soc_3, statefip, drop = TRUE)))

# Checking the min value of total_postings for zeros

#min(occ_listings$total_postings)
#occ_listings %>% count(total_postings == 0)

# Note: Includes zeros. 

# Checking the min values of some of the other variables 

#min(occ_listings$any_educ)
#occ_listings %>% count(any_educ == 0)

# Note: Includes NAs and zeros.

# Recode variables as shares 

occ_listings <- occ_listings %>%
  mutate(
    sany_educ = ifelse(total_postings == 0, NA, any_educ / total_postings),
    sbachelor = ifelse(total_postings == 0, NA, bachelor / total_postings),
    smaster = ifelse(total_postings == 0, NA, master / total_postings),
    sdoctorate = ifelse(total_postings == 0, NA, doctorate / total_postings),
    scomputer_skills = ifelse(total_postings == 0, NA, computer_skills / total_postings),
    scognitive_skills = ifelse(total_postings == 0, NA, cognitive_skills / total_postings)
  )

# Checking my coding of the share variables 

#occ_listings %>% filter(is.na(sany_educ) & total_postings > 0) # shouldn't happen
# verified all NAs occur only when total_postings = 0 

#occ_listings %>% filter(sany_educ == 0 & total_postings == 0) # shouldn't happen
# verified all zeros occur only when total_postings > 0 

#class(occ_listings$sany_educ)
#numeric

#unique(occ_listings$sany_educ[occ_listings$sany_educ == 0])
# zeros appear to be numeric (as they should be) and distinct from NA.

#table(
#  is_na = is.na(occ_listings$sany_educ), 
#  is_zero = occ_listings$sany_educ == 0,
#  useNA = "ifany"
#)
# zeros and NAs appear to be distinct (they're in different cells).

# Drop non-share versions of variables 

occ_listings <- occ_listings %>%
  select(-any_educ:-cognitive_skills)


# 2. Treatment Panel 

treatment_panel <- read.csv("lightcast_treatment_panel.csv")

# 3. Covariate Panel 

covariate_panel <- read.csv("lightcast_covariates.csv")


# MERGING DATA -----------------------------------------------------------------

occ_listings_merged <- list(occ_listings, treatment_panel, covariate_panel) %>%
  reduce(inner_join, by = c("statefip", "year_month")) %>%
  select(-state.x, -state.y, -X.x, -X.y) %>% 
  relocate(state, .after = statefip)


# IMPOSING RESTRICTIONS FOR INDUSTRY BANS --------------------------------------

# 1. Health 

occ_listings_merged <- occ_listings_merged %>%
  mutate(
    soc_maj2 = str_sub(str_remove_all(soc_3, "\\D"), 1, 2),
    healthcare_soc = soc_maj2 %in% c("29", "31")
  ) %>%
  filter(!healthcare_soc) %>%
  select(-healthcare_soc)

# 2. Broadcast 

#273 – Media and Communication Workers
#274 – Media and Communication Equipment Workers
#272 – News Analysts, Reporters, and Journalists
#275 – Photographers and Camera Operators

broadcast_socs <- c("272", "273", "274", "275")

#occ_listings_merged %>%
#  mutate(soc3_trim = substr(gsub("\\D", "", soc_3), 1, 3)) %>%  # strip non-digits, keep first 3
#  filter(soc3_trim %in% broadcast_socs) %>%
#  count(soc3_trim, sort = TRUE)

occ_listings_merged <- occ_listings_merged %>%
  mutate(soc3_trim = substr(gsub("\\D", "", soc_3), 1, 3)) %>%  # normalize to 3-digit SOC
  filter(!soc3_trim %in% broadcast_socs) %>%                    # drop broadcast SOCs
  select(-soc3_trim)                                            # optional: drop helper column

# 3. High Tech (Hawaii)

occ_listings_merged <- occ_listings_merged %>%
  filter(!(state == "Hawaii"))

# 4. Motor Vehicle Industry (Montana)

occ_listings_merged <- occ_listings_merged %>%
  filter(!(state == "Montana"))


# EXCLUDING FULL-BAN STATES ---------------------------------------------------- 

occ_listings_inc_ban <- occ_listings_merged %>%
  filter(!(state %in% c("California", "Minnesota", "North Dakota", "Oklahoma")))

# NOTE: Will have to create a new dataset for analyzing Minnesota's ban. It will
# exclude other full ban states in addition to income ban states.


# SUM STAT CODE ----------------------------------------------------------------

# Means and standard deviations 

by_group <- occ_listings_inc_ban %>%
  group_by(ever_treated) %>%
  summarize(
    across(
      c(total_postings, sany_educ, sbachelor, smaster, sdoctorate, 
        scomputer_skills, scognitive_skills, inc_pcap, employment_sa, hpi_sa),
      list(mean = ~mean(.x, na.rm = TRUE),
           sd = ~sd(.x, na.rm = TRUE),
           n = ~sum(!is.na(.x))),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(-ever_treated,
               names_to = c("variable", ".value"),
               names_pattern = "(.*)_(mean|sd|n)$") %>%
  pivot_wider(names_from = ever_treated,
              values_from = c(mean, sd, n),
              names_glue = "{.value}_{ever_treated}")

# Welch test 

vars <- c("total_postings", "sany_educ", "sbachelor", "smaster", "sdoctorate",
          "scomputer_skills", "scognitive_skills", 
          "inc_pcap", "employment_sa", "hpi_sa")

welch_results <- map_dfr(vars, function(v) {
  fm1 <- as.formula(paste(v, "~ ever_treated"))
  
  t_out <- t.test(fm1, data = occ_listings_inc_ban, var.equal = FALSE)
  
  tidy(t_out) %>%
    mutate(variable = v) %>%
    select(variable, estimate1, estimate2, estimate, statistic, p.value, 
           conf.low, conf.high, method)
})

# Balance Table 

balance_table <- by_group %>%
  left_join(
    welch_results %>% select(variable, p.value),
    by = "variable"
  ) 

# NOTE: Will need to separately add the total number of listings, and maybe 
# the total number of unique occ-state pairings. 


# DID CODE --------------------------------------------------------------------- 

# General Set-up --------------------------------------------------------------- 

# Outcome Var Vector 

outcomes <- c(
  "total_postings", "sany_educ", "sbachelor", "smaster", "sdoctorate", 
  "scomputer_skills", "scognitive_skills"
)

# Function to extract event-study vectors (regardless of object names)

get_event_df <- function(ev_obj) {
  # dynamic aggte usually stores these as egt/att.egt/se.egt
  nm <- names(ev_obj)
  e   <- ev_obj[[if ("egt"      %in% nm) "egt"      else "e"]]
  att <- ev_obj[[if ("att.egt"  %in% nm) "att.egt"  else "att"]]
  se  <- ev_obj[[if ("se.egt"   %in% nm) "se.egt"   else "se"]]
  tibble(e = e, att = att, se = se)
}

# NOTE: Can probably simplify since I do know what the object names are.


# 1. UNCONDITIONAL, ALL BANS ---------------------------------------------------

# I) Subgroups for "All Ban" figures 

subset1 <- c("24205", "24226", "24237")
subset2 <- c("24238", "24241", "24247")
subset3 <- c("24262", "24272", "24274")

# II) Function to run DID for single outcome 

run_did_for_y <- function(yvar,
                          data = occ_listings_inc_ban,
                          tname = "time_id",
                          idname = "occ_state_id",
                          gname = "gvar_eff",
                          clust = "statefip",
                          allow_unbalanced_panel = TRUE,
                          min_e = -36, max_e = 36) {
  message("Running DID for outcome: ", yvar)
  
  # A) Group-time ATTs
  gt <- att_gt(
    yname  = yvar,
    tname  = tname,
    idname = idname,
    gname  = gname,
    data   = data,
    panel  = TRUE,
    allow_unbalanced_panel = allow_unbalanced_panel,
    bstrap = TRUE,
    cband  = TRUE,
    clustervars = clust
  )
  
  # B) Three group-time plots (same subsets you specified)
  p_gt_1 <- ggdid(gt, group = subset1, title = paste0(yvar, " — subset 1")) +
    guides(x = guide_axis(check.overlap = TRUE)) +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  
  p_gt_2 <- ggdid(gt, group = subset2, title = paste0(yvar, " — subset 2")) +
    guides(x = guide_axis(check.overlap = TRUE)) +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  
  p_gt_3 <- ggdid(gt, group = subset3, title = paste0(yvar, " — subset 3")) +
    guides(x = guide_axis(check.overlap = TRUE)) +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  
  # C) Simple aggregation
  agg_simple <- aggte(gt, type = "simple")
  
  # D) Event study aggregation + plot
  agg_event  <- aggte(gt, type = "dynamic", min_e = min_e, max_e = max_e)
  p_event    <- ggdid(agg_event, xgap = 2) +
    ggtitle(paste0(yvar, " — event study")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  
  # Tidy-ish tables to be bound across outcomes
  gt_df <- tibble(
    y    = yvar,
    group = gt$group,
    t     = gt$t,
    att   = gt$att,
    se    = gt$se
  )
  
  simple_df <- tibble(
    y          = yvar,
    overall_att = agg_simple$overall.att,
    overall_se  = agg_simple$overall.se,
    overall_p   = agg_simple$overall.p
  )
  
  event_df <- get_event_df(agg_event) %>% # Calling functrion to retrieve event-study
    mutate(y = yvar, .before = 1)
  
  list(
    y         = yvar,
    gt        = gt,
    agg_simple = agg_simple,
    agg_event  = agg_event,
    plots     = list(gt_subset1 = p_gt_1, gt_subset2 = p_gt_2, gt_subset3 = p_gt_3, event = p_event),
    tables    = list(gt = gt_df, simple = simple_df, event = event_df)
  )
}

# III) Run across all outcomes (with error-robust mapping) 
did_results <- outcomes %>%
  set_names() %>%
  map(~ tryCatch(run_did_for_y(.x), error = function(e) { warning("Failed for ", .x, ": ", e$message); NULL }))

# IV) Combine tidy tables across outcomes (for export/reporting) -------------
gt_all     <- bind_rows(map(did_results, ~ .x$tables$gt))
simple_all <- bind_rows(map(did_results, ~ .x$tables$simple))
event_all  <- bind_rows(map(did_results, ~ .x$tables$event))


# V) Saving the Results 

# i) Base + results directory 
base_dir    <- "C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings"
results_dir <- file.path(base_dir, "results")

# ii) Subdirectories
plot_dir  <- file.path(results_dir, "plots")
table_dir <- file.path(results_dir, "tables")
obj_dir   <- file.path(results_dir, "objects")

# iii) Create directories if they don't exist
for (d in c(plot_dir, table_dir, obj_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# iv) Plot saver with dynamic height 
save_plot <- function(p, file, pname,
                      width = 7, height_default = 4.5, height_tall = 13.5, dpi = 300) {
  # Make subset plots taller
  height <- if (grepl("^gt_", pname)) height_tall else height_default
  
  # Add a bit of margin to help crowded x-axes
  p <- p + theme(plot.margin = margin(t = 12, r = 8, b = 12, l = 8))
  
  ggplot2::ggsave(
    filename = file,
    plot = p,
    path = plot_dir,
    width = width,
    height = height,
    dpi = dpi,
    units = "in"
  )
}

# v) Iterate outcomes and their plot lists 
# did_results is a *named* list: names are the outcomes
purrr::iwalk(did_results, function(res, yname) {
  if (is.null(res)) return(invisible(NULL))  # skip failures
  purrr::iwalk(res$plots, function(p, pname) {
    fname <- paste0(yname, "_", pname, ".png")  # e.g. total_postings_gt_subset1.png
    save_plot(p, file = fname, pname = pname)
  })
})

# vi) Save objects + tables in the results subfolders 
saveRDS(did_results, file = file.path(obj_dir, "did_results.rds"))

readr::write_csv(gt_all,     file.path(table_dir, "did_group_time_all_outcomes.csv"))
readr::write_csv(simple_all, file.path(table_dir, "did_simple_agg_all_outcomes.csv"))
readr::write_csv(event_all,  file.path(table_dir, "did_event_study_all_outcomes.csv"))


# 2. UNCONDITIONAL, HW BANS ----------------------------------------------------

# I) Filter the data 

occ_listings_hw <- occ_listings_inc_ban %>%
  filter(is.na(hw_ban) | hw_ban == 1)

# Tabulate orginal groups 

#occ_listings_inc_ban %>% 
#  count(gvar_eff, name = "count")

#occ_listings_inc_ban %>%
#  filter(gvar_eff == time_id) %>%
#  distinct(gvar_eff, year_month) %>%
#  arrange(gvar_eff)

# Tabulate HW groups only 

#occ_listings_hw %>%
#  count(gvar_eff, name = "count")

#occ_listings_hw %>%
#  filter(gvar_eff == time_id) %>%
#  distinct(gvar_eff, year_month) %>%
#  arrange(gvar_eff)

# II) New did running function for HW ban

run_did_for_y_hw <- function(yvar,
                             data = occ_listings_hw,
                             tname = "time_id",
                             idname = "occ_state_id",
                             gname = "gvar_eff",
                             clust = "statefip",
                             allow_unbalanced_panel = TRUE, # may be forced to change this
                             min_e = -36, max_e = 36) {
  message("Running HW-ban DID for outcome: ", yvar)
  
  # A) Group-time ATTs
  gt <- att_gt(
    yname = yvar,
    tname = tname,
    idname = idname,
    gname = gname, 
    data = data,
    panel = TRUE,
    allow_unbalanced_panel = allow_unbalanced_panel,
    bstrap = TRUE,
    cband = TRUE,
    clustervars = clust
  )
  
  # NOTE: Forgoing splitting the group-time plots up for the time being
  
  # B) Group-time plot 
  p_gt <- ggdid(gt, title = paste0(yvar, " - group-time plot")) + 
    guides(x = guide_axis(check.overlap = TRUE)) +
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  
  # C) Simple Aggregation 
  agg_simple <- aggte(gt, type = "simple")
  
  # D) Event study aggregation + plot 
  agg_event <- aggte(gt, type = "dynamic", min_e = min_e, max_e = max_e)
  p_event <- ggdid(agg_event, xgap = 2) +
    ggtitle(paste0(yvar, " - event study")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  
  ## ---- NEW: carry per-(g,t) sample sizes from did into the tidy table ----
  gt_n <- if (!is.null(gt$n)) gt$n else rep(NA_real_, length(gt$att))  
  
  # Tidy-ish tables to be bound across outcomes 
  gt_df <- tibble(
    y = yvar,
    group = gt$group,
    t = gt$t,
    att = gt$att,
    se = gt$se,
    n = gt_n
  )
  
  ## ---- NEW: put run-size summaries into the simple table -----------------
  # distinct units (ids) in this run with non-missing outcome
  n_units_panel <- data %>%
    filter(!is.na(.data[[yvar]])) %>%      # keep only rows with non-missing outcome
    distinct(!!sym(idname)) %>%            # !! unquotes the symbol made from the string
    nrow()                                 # count distinct ids
  
  # number of (g,t) cells that actually have an ATT estimate
  n_gt_cells <- sum(!is.na(gt$att))
  
  # total unit-periods used across cells (if att_gt stored 'n')
  n_unit_periods_used <- if (!is.null(gt$n)) sum(gt$n, na.rm = TRUE) else NA_real_
    
  simple_df <- tibble(
    y = yvar, 
    overall_att = agg_simple$overall.att,
    overall_se = agg_simple$overall.se,
    overall_p = agg_simple$overall.p,
    n_units_panel        = n_units_panel,        # distinct ids in this run
    n_gt_cells           = n_gt_cells,           # number of ATT(g,t) cells
    n_unit_periods_used  = n_unit_periods_used   # total “n” summed over cells
  )
  
  event_df <- get_event_df(agg_event) %>%
    mutate(y = yvar, .before = 1)
  
  list(
    y = yvar,
    gt = gt,
    agg_simple = agg_simple,
    agg_event = agg_event,
    plots = list(gt = p_gt, event = p_event),
    tables = list(gt = gt_df, simple = simple_df, event = event_df)
  )
}

# III) Run across all outcomes (with error-robust mapping)

did_results_hw <- outcomes %>%
  set_names() %>% 
  map(~ tryCatch(run_did_for_y_hw(.x), error = function(e){
    warning("Failed for ", .x, ": ", e$message); NULL
  }))

# IV) Combing tidy tables across outcomes (for inspection in R)

gt_all <- bind_rows(map(did_results_hw, ~ .x$tables$gt))
simple_all <- bind_rows(map(did_results_hw, ~.x$tables$simple))
event_all <- bind_rows(map(did_results_hw, ~ .x$tables$event))

# V) Saving the Results (NOTE: THIS FUNCTION WORKS FOR HW AND LW BAN ANALYSIS)

save_did_bundle <- function(did_list,
                            tag,
                            base_dir = "C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings") {
  stopifnot(is.list(did_list), length(tag) == 1L)
  
  # A) Create dirs
  results_dir <- file.path(base_dir, "results", tag)
  plot_dir    <- file.path(results_dir, "plots")
  table_dir   <- file.path(results_dir, "tables")
  obj_dir     <- file.path(results_dir, "objects")
  for (d in c(plot_dir, table_dir, obj_dir)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  
  # B) Drop NULL outcome results
  did_list_clean <- purrr::compact(did_list)
  
  # C) Bind tidy tables defensively (use rlang::`%||%` and coerce NULL->empty tibble)
  gt_all <- dplyr::bind_rows(purrr::map(did_list_clean, ~ rlang::`%||%`(.x$tables$gt,     NULL)))
  simple_all <- dplyr::bind_rows(purrr::map(did_list_clean, ~ rlang::`%||%`(.x$tables$simple, NULL)))
  event_all  <- dplyr::bind_rows(purrr::map(did_list_clean, ~ rlang::`%||%`(.x$tables$event,  NULL)))
  
  if (is.null(gt_all))   gt_all   <- tibble::tibble()
  if (is.null(simple_all))  simple_all  <- tibble::tibble()
  if (is.null(event_all))   event_all   <- tibble::tibble()
  
  # D) Plot saver — tall for group-time or names starting with "gt_"
  save_plot_strat <- function(p, file, pname,
                              width = 7, height_default = 4.5, height_tall = 13.5, dpi = 300) {
    is_tall <- grepl("^gt", pname)
    height  <- if (is_tall) height_tall else height_default
    p <- p + ggplot2::theme(plot.margin = ggplot2::margin(t = 12, r = 8, b = 12, l = 8))
    ggplot2::ggsave(filename = file, plot = p, path = plot_dir,
                    width = width, height = height, dpi = dpi, units = "in")
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

save_did_bundle(did_results_hw, tag = "hw") 



# 3. UNCONDITIONAL, LW BANS ----------------------------------------------------

# I) Filter the data 

occ_listings_lw <- occ_listings_inc_ban %>%
  filter(is.na(hw_ban) | hw_ban == 0)

# Tabulate LW groups only 

#occ_listings_lw %>%
#  count(gvar_eff, name = "count")

#occ_listings_lw %>%
#  filter(gvar_eff == time_id) %>% # i.e., only obs when treatment occurs 
#  distinct(gvar_eff, year_month) %>%
#  arrange(gvar_eff)

# II) Subsets for LW ban group-time plots 

subset1_lw <- c("24205", "24226", "24237")
subset2_lw <- c("24238", "24241", "24247")
subset3_lw <- c("24262")

# III) New did running function for LW ban

run_did_for_y_lw <- function(yvar,
                             data = occ_listings_lw,
                             tname = "time_id",
                             idname = "occ_state_id",
                             gname = "gvar_eff",
                             clust = "statefip",
                             allow_unbalanced_panel = TRUE, 
                             min_e = -36, max_e = 36) {
  message("Running LW-ban DID for outcome: ", yvar)
  
  # A) Group-time ATTs 
  gt <- att_gt(
    yname = yvar,
    tname = tname,
    idname = idname,
    gname = gname, 
    data = data, 
    panel = TRUE,
    allow_unbalanced_panel = allow_unbalanced_panel,
    bstrap = TRUE,
    cband = TRUE,
    clustervars = clust
  )
  
  # B) Group time plots 
  p_gt_1 <- ggdid(gt, group = subset1_lw, title = paste0(yvar, " - subset 1 group-time plot")) +
    guides(x = guide_axis(check.overlap = TRUE)) + 
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  
  p_gt_2 <- ggdid(gt, group = subset2_lw, title = paste0(yvar, " - subset 2 group-time plot")) +
    guides(x = guide_axis(check.overlap = TRUE)) + 
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  
  p_gt_3 <- ggdid(gt, group = subset3_lw, title = paste0(yvar, " - subset 3 group-time plot")) +
    guides(x = guide_axis(check.overlap = TRUE)) + 
    theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))
  
  # C) Simple Aggregation 
  agg_simple <- aggte(gt, type = "simple")
  
  # D) Event study aggregation + plot 
  agg_event <- aggte(gt, type = "dynamic", min_e = min_e, max_e = max_e)
  p_event <- ggdid(agg_event, xgap = 2) + 
    ggtitle(paste0(yvar, " - event study")) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))
  
  # Carry per-(g,t) sample sizes into the tidy table 
  gt_n <- if(!is.null(gt$n)) gt$n else rep(NA_real_, length(gt$att))
  
  gt_df <- tibble(
    y = yvar, 
    group = gt$group, 
    t = gt$t,
    att = gt$att, 
    se = gt$se, 
    n = gt_n
  )
  
  # distinct units (ids) in this run with non-missing outcome
  n_units_panel <- data %>%
    filter(!is.na(.data[[yvar]])) %>%      # keep only rows with non-missing outcome
    distinct(!!sym(idname)) %>%            # !! unquotes the symbol made from the string
    nrow()                                 # count distinct ids
  
  # number of (g,t) cells that actually have an ATT estimate
  n_gt_cells <- sum(!is.na(gt$att))
  
  # total unit-periods used across cells (if att_gt stored 'n')
  n_unit_periods_used <- if (!is.null(gt$n)) sum(gt$n, na.rm = TRUE) else NA_real_
  
  simple_df <- tibble(
    y = yvar,
    overall_att = agg_simple$overall.att,
    overall_se = agg_simple$overall.se, 
    overall_p = agg_simple$overall.p,
    n_units_panel = n_units_panel,
    n_gt_cells = n_gt_cells,
    n_unit_periods_used = n_unit_periods_used
  )
  
  event_df <- get_event_df(agg_event) %>%
    mutate(y = yvar, .before = 1)
  
  list(
    y = yvar,
    gt = gt, 
    agg_simple = agg_simple,
    agg_event = agg_event,
    plots = list(gt_1 = p_gt_1, gt_2 = p_gt_2, gt_3 = p_gt_3, event = p_event),
    tables = list(gt = gt_df, simple = simple_df, event = event_df)
  )
}

# IV) Run across all outcomes (with error-robust mapping)

did_results_lw <- outcomes %>%
  set_names() %>%
  map(~ tryCatch(run_did_for_y_lw(.x), error = function(e){
    warning("Failed for ", .x, ": ", e$message); NULL
  }))

# V) Combining tidy tables across outcomes (for inspection in R)

gt_all <- bind_rows(map(did_results_lw, ~ .x$tables$gt))
simple_all <- bind_rows(map(did_results_lw, ~ .x$tables$simple))
event_all <- bind_rows(map(did_results_lw, ~ .x$tables$event))

# VI) Running the "Save" Function 

save_did_bundle(did_results_lw, tag = "lw") 



















