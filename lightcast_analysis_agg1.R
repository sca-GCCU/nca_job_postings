# PREAMBLE 

rm(list = ls())

library(did)
library(tidyverse)
library(tigris)
library(purrr)
library(broom)
library(rlang)
library(lubridate)

set.seed(9284)

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/data")

# LOADING THE DATA 

# 1. Lightcast

occ_listings <- read.csv("anastasi_data_2010_2025_agg1.csv")  

# Check that "state" is "statefip" 

data(fips_codes)
ref <- fips_codes %>%
  distinct(state_code, state_name) %>%
  mutate(
    state_code = as.integer(state_code)
  )

####################### CAN SKIP THIS PART #############################
checked <- occ_listings %>%
  left_join(ref, by = c("state" = "state_code")) %>%
  mutate(
    name_mismatch = state_name.x != state_name.y, # flag mismatches
    missing_ref = is.na(state_name.y) # codes not found in ref
  )

checked %>%
  summarize(
    total = n(),
    mismatches = sum(name_mismatch, na.rm = TRUE),
    missing_codes = sum(missing_ref),
    mismatches_samples = list(unique(state[name_mismatch])),
    missing_samples = list(unique(state[missing_ref]))
  )

checked_inspect <- checked %>% 
  filter(name_mismatch) %>% distinct(state, state_name.x, state_name.y)

######################################################################

# NOTE: Looks like "state" is indeed "statefip", but the formatting of the 
# state_names variable in the Lightcast dataset is inconsistent. 

# Standardizing the names 

occ_listings <- occ_listings %>%
  rename("statefip" = "state") %>%
  left_join(ref, by = c("statefip" = "state_code")) %>%
  rename("state" = "state_name.y") %>%
  relocate(state, .after = statefip) %>%
  select(-state_name.x)
  
# 2. Treatment Panel 

treatment_panel <- read.csv("lightcast_treatment_panel.csv")

# 3. Covariate Panel 

covariate_panel <- read.csv("lightcast_covariates.csv")

# MERGING DATA  

occ_listings_merged <- list(occ_listings, treatment_panel, covariate_panel) %>%
  reduce(inner_join, by = c("statefip", "year_month")) %>%
  select(-state.x, -state.y, -X.x, -X.y) %>% 
  relocate(state, .after = statefip)

# IMPOSING RESTRICTIONS FOR INDUSTRY BANS  

# 1. Health (might already have been done)

occ_listings_merged <- occ_listings_merged %>%
  mutate(
    soc_maj2 = str_sub(str_remove_all(soc_3, "\\D"), 1, 2),
    healthcare_soc = soc_maj2 %in% c("29", "31")
  )

#occ_listings_merged %>%
#  count(soc_maj2, healthcare_soc, sort = TRUE)

occ_listings_merged <- occ_listings_merged %>%
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


# EXCLUDING FULL-BAN STATES 

occ_listings_merged <- occ_listings_merged %>%
  filter(!(state %in% c("California", "Minnesota", "North Dakota", "Oklahoma")))


# Restricting sample window

occ_listings_merged <- occ_listings_merged %>%
  mutate(ym_date = ym(year_month)) %>%
  filter(ym_date >= ymd("2014-01-01"))

# Dropping Oregon 

occ_listings_merged <- occ_listings_merged %>%
  filter(!(state == "Oregon"))


# SUM STAT CODE 

# Means and standard deviations 

by_group <- occ_listings_merged %>%
  group_by(ever_treated) %>%
  summarize(
    across(
      c(total_postings, any_educ, bachelor, master, doctorate, 
        computer_skills, cognitive_skills, inc_pcap, employment_sa, hpi_sa),
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

vars <- c("total_postings", "any_educ", "bachelor", "master", "doctorate",
          "computer_skills", "cognitive_skills", 
          "inc_pcap", "employment_sa", "hpi_sa")

welch_results <- map_dfr(vars, function(v) {
  fm1 <- as.formula(paste(v, "~ ever_treated"))
  
  t_out <- t.test(fm1, data = occ_listings_merged, var.equal = FALSE)
  
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


# DID CODE 

# Create idname variable 

occ_listings_merged <- occ_listings_merged %>%
  mutate(occ_state_id = as.integer(interaction(soc_3, statefip, drop = TRUE)))

# Calculate the number of unique occ_state_id codes  

occ_listings_merged %>%
  distinct(occ_state_id) %>%
  nrow()

# Count the number of distinct treatment groups 

occ_listings_merged %>% 
  distinct(gvar_eff) %>%
  nrow()

# Tabulate the number of distinct treatment groups 

occ_listings_merged %>%
  count(gvar_eff, name = "n_units") %>%
  arrange(gvar_eff)

# 1. Unconditional, All Bans 

# A. Estimate Group-Time Average Treatment Effects 

cs_total_postings <- att_gt(
  yname = "total_postings",
  tname = "time_id",
  idname = "occ_state_id",
  gname = "gvar_eff",
  #xformla = ~ inc_pcap + employment_sa + hpi_sa,
  data = occ_listings_merged,
  panel = TRUE,
  allow_unbalanced_panel = TRUE,
  control_group = "notyettreated",
  bstrap = TRUE, # Must set TRUE for bootstrap se with cluster 
  cband = TRUE, # Uniform confidence interval (TRUE is the default)
  clustervars = "statefip"
)

summary(cs_total_postings)

# B. Plotting Group-Time Average Treatment Effects 

subset1 <- c("24205", "24226", "24237")
subset2 <- c("24238", "24241", "24247")
subset3 <- c("24262", "24272", "24274")

ggdid(cs_total_postings, group = subset1, title = "") +
  guides(x = guide_axis(check.overlap = TRUE)) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))

ggdid(cs_total_postings, group = subset2, title = "") +
  guides(x = guide_axis(check.overlap = TRUE)) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))

ggdid(cs_total_postings, group = subset3, title = "") +
  guides(x = guide_axis(check.overlap = TRUE)) +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1))

# C. Simple Aggregation 

agg_total_postings <- aggte(cs_total_postings, type = "simple")

summary(agg_total_postings)

# D. Event Study (i.e., treatment by length of exposure)

event_total_postings <- aggte(cs_total_postings, 
                              type = "dynamic", 
                              min_e = -36,
                              max_e = 36)

summary(event_total_postings)

ggdid(event_total_postings, xgap = 2) + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))






#--- 0) IDs (as you already do) -----------------------------------------------
occ_listings_merged <- occ_listings_merged %>%
  mutate(occ_state_id = as.integer(interaction(soc_3, statefip, drop = TRUE)))

#--- 1) Define outcomes & group subsets ---------------------------------------
outcomes <- c(
  "total_postings", "any_educ", "bachelor", "master", "doctorate", 
  "computer_skills", "cognitive_skills"
)

subset1 <- c("24205", "24226", "24237")
subset2 <- c("24238", "24241", "24247")
subset3 <- c("24262", "24272", "24274")

#--- 2) Helper to safely extract event-study vectors no matter the object names
get_event_df <- function(ev_obj) {
  # dynamic aggte usually stores these as egt/att.egt/se.egt
  nm <- names(ev_obj)
  e   <- ev_obj[[if ("egt"      %in% nm) "egt"      else "e"]]
  att <- ev_obj[[if ("att.egt"  %in% nm) "att.egt"  else "att"]]
  se  <- ev_obj[[if ("se.egt"   %in% nm) "se.egt"   else "se"]]
  tibble(e = e, att = att, se = se)
}

#--- 3) Core runner for a single outcome --------------------------------------
run_did_for_y <- function(yvar,
                          data = occ_listings_merged,
                          tname = "time_id",
                          idname = "occ_state_id",
                          gname = "gvar_eff",
                          clust = "statefip",
                          allow_unbalanced_panel = FALSE,
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
  
  # Tidy-ish tables you can bind across outcomes
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
  
  event_df <- get_event_df(agg_event) %>%
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

#--- 4) Run across all outcomes (with error-robust mapping) --------------------
did_results <- outcomes %>%
  set_names() %>%
  map(~ tryCatch(run_did_for_y(.x), error = function(e) { warning("Failed for ", .x, ": ", e$message); NULL }))

#--- 5) Combine tidy tables across outcomes (for export/reporting) -------------
gt_all     <- bind_rows(map(did_results, ~ .x$tables$gt))
simple_all <- bind_rows(map(did_results, ~ .x$tables$simple))
event_all  <- bind_rows(map(did_results, ~ .x$tables$event))




# 0) Base + results directory
base_dir    <- "C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings"
results_dir <- file.path(base_dir, "results")

# Subdirectories
plot_dir  <- file.path(results_dir, "plots")
table_dir <- file.path(results_dir, "tables")
obj_dir   <- file.path(results_dir, "objects")

# Create directories if they don't exist
for (d in c(plot_dir, table_dir, obj_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# 1) Plot saver with dynamic height
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

# 2) Iterate outcomes and their plot lists
# did_results is a *named* list: names are the outcomes
purrr::iwalk(did_results, function(res, yname) {
  if (is.null(res)) return(invisible(NULL))  # skip failures
  purrr::iwalk(res$plots, function(p, pname) {
    fname <- paste0(yname, "_", pname, ".png")  # e.g. total_postings_gt_subset1.png
    save_plot(p, file = fname, pname = pname)
  })
})

# 3) Save objects + tables in the results subfolders
saveRDS(did_results, file = file.path(obj_dir, "did_results.rds"))

readr::write_csv(gt_all,     file.path(table_dir, "did_group_time_all_outcomes.csv"))
readr::write_csv(simple_all, file.path(table_dir, "did_simple_agg_all_outcomes.csv"))
readr::write_csv(event_all,  file.path(table_dir, "did_event_study_all_outcomes.csv"))



















