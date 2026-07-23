##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "9a_cs_did_fab.R" 
# by: Sebastian C. Anastasi
# Date of this version: July 23, 2026
#
# Description: Runs CS DID analysis of states staggered noncompete bans for the
# firm-soc-state-year level sample.
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

library(did)        # Callaway & Sant'Anna estimator 
library(fixest)     # High-performance fixed-effects regression (TWFE comparison)
library(readr)
library(tidyr)
library(dplyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(knitr)
library(kableExtra)

set.seed(87) # for reproducibility with bootstrap SEs 

par_proc = FALSE # for local runs 
#par_proc = TRUE # for parallel processing on cluster  
n_cores = 1 # for local runs 
#n_cores = 4 # for parallel processing on cluster


# ----------------------------- LOAD DATA --------------------------------------
df_firm <- read_csv("data/analysis-data/agg2_fab_analysis.csv")


# -------------------- CS DID ANALYSIS (UNCONDITIONAL) -------------------------
# --- Total Postings --- 
# NOTE: Unbalanced panel version (repeated cross-sections)

cs_tot_post <- att_gt(
  yname = "total_postings_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE, # makes this repeated cross-sections
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_tot_post)

csdid_cohort_tot_post <- ggdid(
  cs_tot_post, 
  xlab = "Year", 
  ylab = "Total Postings", 
  title = "",
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_tot_post_fab.pdf",
  csdid_cohort_tot_post,
  width = 8,
  height = 8,
  units = "in"
)

es_tot_post <- aggte(
  cs_tot_post,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_tot_post)
csdid_es_tot_post <- ggdid(
  es_tot_post, 
  xlab = "Event Time", 
  ylab = "Total Postings",
  title = " "
)
ggsave(
  "output/figures/csdid_es_tot_post_fab.pdf",
  csdid_es_tot_post,
  width = 7,
  height = 5,
  units = "in"
)

# NOTE: Could later combine this little table with tables of other results.
simple_tot_post <- aggte(cs_tot_post, type = "simple")

tib_tot_post <- tibble(
  `Estimate` = simple_tot_post$overall.att,
  `Std. Error` = simple_tot_post$overall.se,
  `95% CI` = sprintf(
    "[%.3f, %.3f]",
    simple_tot_post$overall.att - 1.96*simple_tot_post$overall.se,
    simple_tot_post$overall.att + 1.96*simple_tot_post$overall.se
  )
)

tab_tot_post <- kable(
  tib_tot_post,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  align = c("ccc"),
  caption = "Overall Effect - Total Postings",
  label = "csdid_tab_tot_post_fab"
) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the overall effect of NCA bans on total postings across all treated states.",
    threeparttable = TRUE,
    escape = FALSE
  ) 

writeLines(
  tab_tot_post,
  "output/tables/csdid_tab_tot_post_fab.tex"
)

rm(cs_tot_post, csdid_cohort_tot_post, es_tot_post, csdid_es_tot_post,
   tib_tot_post, tab_tot_post, simple_tot_post)
gc()


# --- Any Experience --- 

cs_any_exp <- att_gt(
  yname = "any_exp_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_any_exp)
csdid_cohort_exp <- ggdid(
  cs_any_exp, 
  xlab = "Year",
  ylab = "Experience Required (Postings)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_exp_fab.pdf",
  csdid_cohort_exp,
  width = 8,
  height = 8,
  units = "in"
)

es_any_exp <- aggte(
  cs_any_exp,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_any_exp)
csdid_es_exp <- ggdid(
  es_any_exp, 
  xlab = "Event Time",
  ylab = "Experience Required (Postings)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_exp_fab.pdf",
  csdid_es_exp,
  width = 7,
  height = 5,
  units = "in"
)

# NOTE: Could later combine this little table with tables of other results.
simple_any_exp <- aggte(cs_any_exp, type = "simple")

tib_any_exp <- tibble(
  `Estimate` = simple_any_exp$overall.att,
  `Std. Error` = simple_any_exp$overall.se,
  `95% CI` = sprintf(
    "[%.3f, %.3f]",
    simple_any_exp$overall.att - 1.96*simple_any_exp$overall.se,
    simple_any_exp$overall.att + 1.96*simple_any_exp$overall.se
  )
)

tab_any_exp <- kable(
  tib_any_exp,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  align = c("ccc"),
  caption = "Overall Effect - Experience Required (Postings)",
  label = "csdid_tab_any_exp_fab"
) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the overall effect of NCA bans on the number of postings requiring experience across all treated states.",
    threeparttable = TRUE,
    escape = FALSE
  ) 

writeLines(
  tab_any_exp,
  "output/tables/csdid_tab_any_exp_fab.tex"
)

rm(cs_any_exp, csdid_cohort_exp, es_any_exp, csdid_es_exp,
   tib_any_exp, tab_any_exp, simple_any_exp)
gc()


# --- Share Experience --- 

cs_share_exp <- att_gt(
  yname = "share_exp",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_share_exp)
csdid_cohort_exp_share <- ggdid(
  cs_share_exp, 
  xlab = "Year",
  ylab = "Experience Required (Share)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_exp_share_fab.pdf",
  csdid_cohort_exp_share,
  height = 8,
  width = 8,
  units = "in"
)

es_share_exp <- aggte(
  cs_share_exp,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_share_exp)
csdid_es_exp_share <- ggdid(
  es_share_exp, 
  xlab = "Event Time",
  ylab = "Experience Required (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_exp_share_fab.pdf",
  csdid_es_exp_share,
  height = 5,
  width = 7,
  units = "in"
)

# NOTE: Could later combine this little table with tables of other results.
simple_share_exp <- aggte(cs_share_exp, type = "simple")

tib_share_exp <- tibble(
  `Estimate` = simple_share_exp$overall.att,
  `Std. Error` = simple_share_exp$overall.se,
  `95% CI` = sprintf(
    "[%.3f, %.3f]",
    simple_share_exp$overall.att - 1.96*simple_share_exp$overall.se,
    simple_share_exp$overall.att + 1.96*simple_share_exp$overall.se
  )
)

tab_share_exp <- kable(
  tib_share_exp,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  align = c("ccc"),
  caption = "Overall Effect - Experience Required (Share)",
  label = "csdid_tab_share_exp_fab"
) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the overall effect of NCA bans on the share of postings requiring experience across all treated states.",
    threeparttable = TRUE,
    escape = FALSE
  ) 

writeLines(
  tab_share_exp,
  "output/tables/csdid_tab_share_exp_fab.tex"
)

rm(cs_share_exp, csdid_cohort_exp_share, es_share_exp, csdid_es_exp_share,
   tib_share_exp, tab_share_exp, simple_share_exp)
gc()


# --- Average Experience --- 

cs_ave_exp <- att_gt(
  yname = "ave_exp_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_ave_exp)
csdid_cohort_exp_ave <- ggdid(
  cs_ave_exp, 
  xlab = "Year",
  ylab = "Experience Required (Average)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_exp_ave_fab.pdf",
  csdid_cohort_exp_ave,
  height = 8,
  width = 8,
  units = "in"
)

es_ave_exp <- aggte(
  cs_ave_exp,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_ave_exp)
csdid_es_exp_ave <- ggdid(
  es_ave_exp, 
  xlab = "Event Time",
  ylab = "Experience Required (Average)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_exp_ave_fab.pdf",
  csdid_es_exp_ave,
  height = 5,
  width = 7,
  units = "in"
)

# NOTE: Could later combine this little table with tables of other results.
simple_ave_exp <- aggte(cs_ave_exp, type = "simple")

tib_ave_exp <- tibble(
  `Estimate` = simple_ave_exp$overall.att,
  `Std. Error` = simple_ave_exp$overall.se,
  `95% CI` = sprintf(
    "[%.3f, %.3f]",
    simple_ave_exp$overall.att - 1.96*simple_ave_exp$overall.se,
    simple_ave_exp$overall.att + 1.96*simple_ave_exp$overall.se
  )
)

tab_ave_exp <- kable(
  tib_ave_exp,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  align = c("ccc"),
  caption = "Overall Effect - Experience Required (Average)",
  label = "csdid_tab_ave_exp_fab"
) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the overall effect of NCA bans on average required experience across all treated states.",
    threeparttable = TRUE,
    escape = FALSE
  ) 

writeLines(
  tab_ave_exp,
  "output/tables/csdid_tab_ave_exp_fab.tex"
)

rm(cs_ave_exp, csdid_cohort_exp_ave, es_ave_exp, csdid_es_exp_ave,
   tib_ave_exp, tab_ave_exp, simple_ave_exp)
gc()


# --- Any Bachelor's Degree ---- 

cs_bachelor <- att_gt(
  yname = "bachelor_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_bachelor)
csdid_cohort_bachelor <- ggdid(
  cs_bachelor,
  xlab = "Year",
  ylab = "Bachelor's Required (Postings)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_bachelor_fab.pdf",
  csdid_cohort_bachelor,
  height = 8,
  width = 8,
  units = "in"
)

es_bachelor <- aggte(
  cs_bachelor,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_bachelor)
csdid_es_bachelor <- ggdid(
  es_bachelor, 
  xlab = "Event Time",
  ylab = "Bachelor's Required (Postings)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_bachelor_fab.pdf",
  csdid_es_bachelor,
  height = 5,
  width = 7,
  units = "in"
)

# NOTE: Could later combine this little table with tables of other results.
simple_bachelor <- aggte(cs_bachelor, type = "simple")

tib_bachelor <- tibble(
  `Estimate` = simple_bachelor$overall.att,
  `Std. Error` = simple_bachelor$overall.se,
  `95% CI` = sprintf(
    "[%.3f, %.3f]",
    simple_bachelor$overall.att - 1.96*simple_bachelor$overall.se,
    simple_bachelor$overall.att + 1.96*simple_bachelor$overall.se
  )
)

tab_bachelor <- kable(
  tib_bachelor,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  align = c("ccc"),
  caption = "Overall Effect - Bachelor's Required (Postings)",
  label = "csdid_tab_bachelor_fab"
) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the overall effect of NCA bans on the number of postings requiring a bachelor's degree across all treated states.",
    threeparttable = TRUE,
    escape = FALSE
  ) 

writeLines(
  tab_bachelor,
  "output/tables/csdid_tab_bachelor_fab.tex"
)

rm(cs_bachelor, csdid_cohort_bachelor, es_bachelor, csdid_es_bachelor,
   tib_bachelor, tab_bachelor, simple_bachelor)
gc()


# --- Share Bachelor's Degree ---- 

cs_bachelor_share <- att_gt(
  yname = "share_bachelor",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_bachelor_share)
csdid_cohort_bachelor_share <- ggdid(
  cs_bachelor_share, 
  xlab = "Year",
  ylab = "Bachelor's Required (Share)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_bachelor_share_fab.pdf",
  csdid_cohort_bachelor_share,
  height = 8,
  width = 8,
  units = "in"
)

es_bachelor_share <- aggte(
  cs_bachelor_share,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_bachelor_share)
csdid_es_bachelor_share <- ggdid(
  es_bachelor_share, 
  xlab = "Event Time",
  ylab = "Bachelor's Required (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_bachelor_share_fab.pdf",
  csdid_es_bachelor_share,
  height = 5,
  width = 7,
  units = "in"
)

# NOTE: Could later combine this little table with tables of other results.
simple_bachelor_share <- aggte(cs_bachelor_share, type = "simple")

tib_bachelor_share <- tibble(
  `Estimate` = simple_bachelor_share$overall.att,
  `Std. Error` = simple_bachelor_share$overall.se,
  `95% CI` = sprintf(
    "[%.3f, %.3f]",
    simple_bachelor_share$overall.att - 1.96*simple_bachelor_share$overall.se,
    simple_bachelor_share$overall.att + 1.96*simple_bachelor_share$overall.se
  )
)

tab_bachelor_share <- kable(
  tib_bachelor_share,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  align = c("ccc"),
  caption = "Overall Effect - Bachelor's Required (Share)",
  label = "csdid_tab_bachelor_share_fab"
) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the overall effect of NCA bans on the share of postings requiring a bachelor's degree across all treated states.",
    threeparttable = TRUE,
    escape = FALSE
  ) 

writeLines(
  tab_bachelor_share,
  "output/tables/csdid_tab_bachelor_share_fab.tex"
)

rm(cs_bachelor_share, 
   csdid_cohort_bachelor_share, 
   es_bachelor_share, 
   csdid_es_bachelor_share,
   tib_bachelor_share,
   tab_bachelor_share,
   simple_bachelor_share)
gc()


# --- Fulltime --- 

cs_fulltime <- att_gt(
  yname = "fulltime_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_fulltime)
csdid_cohort_fulltime <- ggdid(
  cs_fulltime, 
  xlab = "Year",
  ylab = "Full-Time Postings",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_fulltime_fab.pdf",
  csdid_cohort_fulltime,
  height = 8,
  width = 8,
  units = "in"
)

es_fulltime <- aggte(
  cs_fulltime,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_fulltime)
csdid_es_fulltime <- ggdid(
  es_fulltime, 
  xlab = "Event Time",
  ylab = "Full-Time Postings",
  title = " "
)
ggsave(
  "output/figures/csdid_es_fulltime_fab.pdf",
  csdid_es_fulltime,
  height = 5,
  width = 7,
  units = "in"
)

# NOTE: Could later combine this little table with tables of other results.
simple_fulltime <- aggte(cs_fulltime, type = "simple")

tib_fulltime <- tibble(
  `Estimate` = simple_fulltime$overall.att,
  `Std. Error` = simple_fulltime$overall.se,
  `95% CI` = sprintf(
    "[%.3f, %.3f]",
    simple_fulltime$overall.att - 1.96*simple_fulltime$overall.se,
    simple_fulltime$overall.att + 1.96*simple_fulltime$overall.se
  )
)

tab_fulltime <- kable(
  tib_fulltime,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  align = c("ccc"),
  caption = "Overall Effect - Full-Time Postings",
  label = "csdid_tab_fulltime_fab"
) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the overall effect of NCA bans on the number of full-time postings across all treated states.",
    threeparttable = TRUE,
    escape = FALSE
  ) 

writeLines(
  tab_fulltime,
  "output/tables/csdid_tab_fulltime_fab.tex"
)

rm(cs_fulltime, csdid_cohort_fulltime, es_fulltime, csdid_es_fulltime,
   tib_fulltime, tab_fulltime, simple_fulltime)
gc()


# --- Fulltime (Share) --- 

cs_fulltime_share <- att_gt(
  yname = "share_fulltime",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_fulltime_share)
csdid_cohort_fulltime_share <- ggdid(
  cs_fulltime_share, 
  xlab = "Year",
  ylab = "Full-Time Postings (Share)",
  title = " ", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_fulltime_share_fab.pdf",
  csdid_cohort_fulltime_share,
  height = 8,
  width = 8,
  units = "in"
)

es_fulltime_share <- aggte(
  cs_fulltime_share,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_fulltime_share)
csdid_es_fulltime_share <- ggdid(
  es_fulltime_share, 
  xlab = "Event Time",
  ylab = "Full-Time Postings (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_fulltime_share_fab.pdf",
  csdid_es_fulltime_share,
  height = 5,
  width = 7,
  units = "in"
)

# NOTE: Could later combine this little table with tables of other results.
simple_fulltime_share <- aggte(cs_fulltime_share, type = "simple")

tib_fulltime_share <- tibble(
  `Estimate` = simple_fulltime_share$overall.att,
  `Std. Error` = simple_fulltime_share$overall.se,
  `95% CI` = sprintf(
    "[%.3f, %.3f]",
    simple_fulltime_share$overall.att - 1.96*simple_fulltime_share$overall.se,
    simple_fulltime_share$overall.att + 1.96*simple_fulltime_share$overall.se
  )
)

tab_fulltime_share <- kable(
  tib_fulltime_share,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  align = c("ccc"),
  caption = "Overall Effect - Full-Time Postings (Share)",
  label = "csdid_tab_fulltime_share_fab"
) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the overall effect of NCA bans on the share of postings that are full-time across all treated states.",
    threeparttable = TRUE,
    escape = FALSE
  ) 

writeLines(
  tab_fulltime_share,
  "output/tables/csdid_tab_fulltime_share_fab.tex"
)

rm(cs_fulltime_share, 
   csdid_cohort_fulltime_share, 
   es_fulltime_share, 
   csdid_es_fulltime_share,
   tib_fulltime_share,
   tab_fulltime_share,
   simple_fulltime_share)
gc()


# --- Parttime --- 

cs_parttime <- att_gt(
  yname = "parttime_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_parttime)
csdid_cohort_parttime <- ggdid(
  cs_parttime, 
  xlab = "Year",
  ylab = "Part-Time Postings",
  title = " ", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_parttime_fab.pdf",
  csdid_cohort_parttime,
  height = 8,
  width = 8, 
  units = "in"
)

es_parttime <- aggte(
  cs_parttime,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_parttime)
csdid_es_parttime <- ggdid(
  es_parttime, 
  xlab = "Event Time",
  ylab = "Part-Time Postings",
  title = " "
)
ggsave(
  "output/figures/csdid_es_parttime_fab.pdf",
  csdid_es_parttime,
  height = 5,
  width = 7,
  units = "in"
)

# NOTE: Could later combine this little table with tables of other results.
simple_parttime <- aggte(cs_parttime, type = "simple")

tib_parttime <- tibble(
  `Estimate` = simple_parttime$overall.att,
  `Std. Error` = simple_parttime$overall.se,
  `95% CI` = sprintf(
    "[%.3f, %.3f]",
    simple_parttime$overall.att - 1.96*simple_parttime$overall.se,
    simple_parttime$overall.att + 1.96*simple_parttime$overall.se
  )
)

tab_parttime <- kable(
  tib_parttime,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  align = c("ccc"),
  caption = "Overall Effect - Part-Time Postings",
  label = "csdid_tab_parttime_fab"
) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the overall effect of NCA bans on the number of part-time postings across all treated states.",
    threeparttable = TRUE,
    escape = FALSE
  ) 

writeLines(
  tab_parttime,
  "output/tables/csdid_tab_parttime_fab.tex"
)

rm(cs_parttime, csdid_cohort_parttime, es_parttime, csdid_es_parttime,
   tib_parttime, tab_parttime, simple_parttime)
gc()


# --- Parttime (Share) --- 

cs_parttime_share <- att_gt(
  yname = "share_parttime",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_parttime_share)
csdid_cohort_parttime_share <- ggdid(
  cs_parttime_share, 
  xlab = "Year",
  ylab = "Part-Time Postings (Share)",
  title = " ", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cohort_parttime_share_fab.pdf",
  csdid_cohort_parttime_share,
  height = 8,
  width = 8,
  units = "in"
)

es_parttime_share <- aggte(
  cs_parttime_share,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_parttime_share)
csdid_es_parttime_share <- ggdid(
  es_parttime_share, 
  xlab = "Event Time",
  ylab = "Part-Time Postings (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_parttime_share_fab.pdf",
  csdid_es_parttime_share,
  height = 5, 
  width = 7,
  units = "in"
)

# NOTE: Could later combine this little table with tables of other results.
simple_parttime_share <- aggte(cs_parttime_share, type = "simple")

tib_parttime_share <- tibble(
  `Estimate` = simple_parttime_share$overall.att,
  `Std. Error` = simple_parttime_share$overall.se,
  `95% CI` = sprintf(
    "[%.3f, %.3f]",
    simple_parttime_share$overall.att - 1.96*simple_parttime_share$overall.se,
    simple_parttime_share$overall.att + 1.96*simple_parttime_share$overall.se
  )
)

tab_parttime_share <- kable(
  tib_parttime_share,
  format = "latex",
  booktabs = TRUE,
  digits = 3,
  align = c("ccc"),
  caption = "Overall Effect - Part-Time Postings (Share)",
  label = "csdid_tab_parttime_share_fab"
) %>%
  kable_styling(latex_options = c("hold_position")) %>%
  footnote(
    general_title = "",
    fixed_small_size = TRUE,
    general = "\\\\footnotesize \\\\textit{Notes:} This table reports the overall effect of NCA bans on the share of postings that are part-time across all treated states.",
    threeparttable = TRUE,
    escape = FALSE
  ) 

writeLines(
  tab_parttime_share,
  "output/tables/csdid_tab_parttime_share_fab.tex"
)

rm(cs_parttime_share, 
   csdid_cohort_parttime_share,
   es_parttime_share,
   csdid_es_parttime_share,
   tib_parttime_share,
   tab_parttime_share,
   simple_parttime_share)
gc()


# -------------------- CS DID ANALYSIS (CONDITIONAL - SOC) ---------------------
# --- Total Postings --- 
cs_tot_post_cond <- att_gt(
  yname = "total_postings_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ frac_college,
  panel = FALSE, # makes this repeated cross-sections
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_tot_post_cond)
csdid_cond_cohort_tot_post <- ggdid(
  cs_tot_post_cond, 
  xlab = "Year", 
  ylab = "Total Postings", 
  title = "",
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_tot_post_fab.pdf",
  csdid_cond_cohort_tot_post,
  width = 8,
  height = 8,
  units = "in"
)

es_cond_tot_post <- aggte(
  cs_tot_post_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_tot_post)
csdid_es_cond_tot_post <- ggdid(
  es_cond_tot_post, 
  xlab = "Event Time", 
  ylab = "Total Postings",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_tot_post_fab.pdf",
  csdid_es_cond_tot_post,
  width = 7,
  height = 5,
  units = "in"
)

rm(cs_tot_post_cond, csdid_cond_cohort_tot_post, es_cond_tot_post, csdid_es_cond_tot_post)
gc()


# --- Any Experience --- 

cs_any_exp_cond <- att_gt(
  yname = "any_exp_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_any_exp_cond)
csdid_cond_cohort_exp <- ggdid(
  cs_any_exp_cond, 
  xlab = "Year",
  ylab = "Experience Required (Postings)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_exp_fab.pdf",
  csdid_cond_cohort_exp,
  width = 8,
  height = 8,
  units = "in"
)

es_cond_any_exp <- aggte(
  cs_any_exp_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_any_exp)
csdid_es_cond_exp <- ggdid(
  es_cond_any_exp, 
  xlab = "Event Time",
  ylab = "Experience Required (Postings)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_exp_fab.pdf",
  csdid_es_cond_exp,
  width = 7,
  height = 5,
  units = "in"
)

rm(cs_any_exp_cond, csdid_cond_cohort_exp, es_cond_any_exp, csdid_es_cond_exp)
gc()


# --- Share Experience --- 

cs_share_exp_cond <- att_gt(
  yname = "share_exp",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_share_exp_cond)
csdid_cond_cohort_exp_share <- ggdid(
  cs_share_exp_cond, 
  xlab = "Year",
  ylab = "Experience Required (Share)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_exp_share_fab.pdf",
  csdid_cond_cohort_exp_share,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_share_exp <- aggte(
  cs_share_exp_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_share_exp)
csdid_es_cond_exp_share <- ggdid(
  es_cond_share_exp, 
  xlab = "Event Time",
  ylab = "Experience Required (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_exp_share_fab.pdf",
  csdid_es_cond_exp_share,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_share_exp_cond, csdid_cond_cohort_exp_share, es_cond_share_exp, csdid_es_cond_exp_share)
gc()


# --- Average Experience --- 

cs_ave_exp_cond <- att_gt(
  yname = "ave_exp_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_ave_exp_cond)
csdid_cond_cohort_exp_ave <- ggdid(
  cs_ave_exp_cond, 
  xlab = "Year",
  ylab = "Experience Required (Average)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_exp_ave_fab.pdf",
  csdid_cond_cohort_exp_ave,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_ave_exp <- aggte(
  cs_ave_exp_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_ave_exp)
csdid_es_cond_exp_ave <- ggdid(
  es_cond_ave_exp, 
  xlab = "Event Time",
  ylab = "Experience Required (Average)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_exp_ave_fab.pdf",
  csdid_es_cond_exp_ave,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_ave_exp_cond, csdid_cond_cohort_exp_ave, es_cond_ave_exp, csdid_es_cond_exp_ave)
gc()


# --- Any Bachelor's Degree ---- 

cs_bachelor_cond <- att_gt(
  yname = "bachelor_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_bachelor_cond)
csdid_cond_cohort_bachelor <- ggdid(
  cs_bachelor_cond,
  xlab = "Year",
  ylab = "Bachelor's Required (Postings)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_bachelor_fab.pdf",
  csdid_cond_cohort_bachelor,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_bachelor <- aggte(
  cs_bachelor_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_bachelor)
csdid_es_cond_bachelor <- ggdid(
  es_cond_bachelor, 
  xlab = "Event Time",
  ylab = "Bachelor's Required (Postings)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_bachelor_fab.pdf",
  csdid_es_cond_bachelor,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_bachelor_cond, csdid_cond_cohort_bachelor, es_cond_bachelor, csdid_es_cond_bachelor)
gc()


# --- Share Bachelor's Degree ---- 

cs_bachelor_share_cond <- att_gt(
  yname = "share_bachelor",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_bachelor_share_cond)
csdid_cond_cohort_bachelor_share <- ggdid(
  cs_bachelor_share_cond, 
  xlab = "Year",
  ylab = "Bachelor's Required (Share)",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_bachelor_share_fab.pdf",
  csdid_cond_cohort_bachelor_share,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_bachelor_share <- aggte(
  cs_bachelor_share_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_bachelor_share)
csdid_es_cond_bachelor_share <- ggdid(
  es_cond_bachelor_share, 
  xlab = "Event Time",
  ylab = "Bachelor's Required (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_bachelor_share_fab.pdf",
  csdid_es_cond_bachelor_share,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_bachelor_share_cond, 
   csdid_cond_cohort_bachelor_share, 
   es_cond_bachelor_share, 
   csdid_es_cond_bachelor_share)
gc()


# --- Fulltime --- 

cs_fulltime_cond <- att_gt(
  yname = "fulltime_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_fulltime_cond)
csdid_cond_cohort_fulltime <- ggdid(
  cs_fulltime_cond, 
  xlab = "Year",
  ylab = "Full-Time Postings",
  title = "", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_fulltime_fab.pdf",
  csdid_cond_cohort_fulltime,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_fulltime <- aggte(
  cs_fulltime_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_fulltime)
csdid_es_cond_fulltime <- ggdid(
  es_cond_fulltime, 
  xlab = "Event Time",
  ylab = "Full-Time Postings",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_fulltime_fab.pdf",
  csdid_es_cond_fulltime,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_fulltime_cond, csdid_cond_cohort_fulltime, es_cond_fulltime, csdid_es_cond_fulltime)
gc()


# --- Fulltime (Share) --- 

cs_fulltime_share_cond <- att_gt(
  yname = "share_fulltime",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_fulltime_share_cond)
csdid_cond_cohort_fulltime_share <- ggdid(
  cs_fulltime_share_cond, 
  xlab = "Year",
  ylab = "Full-Time Postings (Share)",
  title = " ", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_fulltime_share_fab.pdf",
  csdid_cond_cohort_fulltime_share,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_fulltime_share <- aggte(
  cs_fulltime_share_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_fulltime_share)
csdid_es_cond_fulltime_share <- ggdid(
  es_cond_fulltime_share, 
  xlab = "Event Time",
  ylab = "Full-Time Postings (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_fulltime_share_fab.pdf",
  csdid_es_cond_fulltime_share,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_fulltime_share_cond, 
   csdid_cond_cohort_fulltime_share, 
   es_cond_fulltime_share, 
   csdid_es_cond_fulltime_share)
gc()


# --- Parttime --- 

cs_parttime_cond <- att_gt(
  yname = "parttime_firm",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_parttime_cond)
csdid_cond_cohort_parttime <- ggdid(
  cs_parttime_cond, 
  xlab = "Year",
  ylab = "Part-Time Postings",
  title = " ", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_parttime_fab.pdf",
  csdid_cond_cohort_parttime,
  height = 8,
  width = 8, 
  units = "in"
)

es_cond_parttime <- aggte(
  cs_parttime_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_parttime)
csdid_es_cond_parttime <- ggdid(
  es_cond_parttime, 
  xlab = "Event Time",
  ylab = "Part-Time Postings",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_parttime_fab.pdf",
  csdid_es_cond_parttime,
  height = 5,
  width = 7,
  units = "in"
)

rm(cs_parttime_cond, csdid_cond_cohort_parttime, es_cond_parttime, csdid_es_cond_parttime)
gc()


# --- Parttime (Share) --- 

cs_parttime_share_cond <- att_gt(
  yname = "share_parttime",
  tname = "year",
  idname = "panel_id",
  gname = "cohort",
  xformla = ~ factor(soc_4),
  panel = FALSE,
  data = df_firm,
  control_group = "nevertreated",
  est_method = "dr",
  bstrap = TRUE,
  cband = TRUE,
  clustervars = "state",
  base_period = "universal",
  anticipation = 0,
  pl = par_proc,
  cores = n_cores
)
summary(cs_parttime_share_cond)
csdid_cond_cohort_parttime_share <- ggdid(
  cs_parttime_share_cond, 
  xlab = "Year",
  ylab = "Part-Time Postings (Share)",
  title = " ", 
  grtitle = "Ban in"
)
ggsave(
  "output/figures/csdid_cond_cohort_parttime_share_fab.pdf",
  csdid_cond_cohort_parttime_share,
  height = 8,
  width = 8,
  units = "in"
)

es_cond_parttime_share <- aggte(
  cs_parttime_share_cond,
  type = "dynamic",
  cband = TRUE,
  balance_e = 2
)
summary(es_cond_parttime_share)
csdid_es_cond_parttime_share <- ggdid(
  es_cond_parttime_share, 
  xlab = "Event Time",
  ylab = "Part-Time Postings (Share)",
  title = " "
)
ggsave(
  "output/figures/csdid_es_cond_parttime_share_fab.pdf",
  csdid_es_cond_parttime_share,
  height = 5, 
  width = 7,
  units = "in"
)

rm(cs_parttime_share_cond, 
   csdid_cond_cohort_parttime_share,
   es_cond_parttime_share,
   csdid_es_cond_parttime_share)
gc()


