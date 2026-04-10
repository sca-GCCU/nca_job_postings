##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "6b_twfe_agg2_mn.R" 
# by: Sebastian C. Anastasi
# Date of this version: April 10, 2026
#
# Description: Generates TWFE estimates and tables, as well as corresponding 
# Event Study plots. 
#
# Dependencies: 
#
# Output: 
##############################################################################

# NOTE: When run in the cluster, I will need to update paths and (I believe)
# install the appropriate packages. 

rm(list = ls())

# Load path helper 
home <- path.expand("~")
proj_root <- file.path(home, "nca_job_postings")
programs_dir <- file.path(proj_root, "programs")
source(file.path(programs_dir, "0c_paths.R"))

library(dplyr)
library(ggplot2)
library(readr)
library(stringr)
library(tibble)
library(lubridate)
library(fixest)

# use all available cores assigned to the job/session
setFixest_nthreads(0)

outcome_var <- c("any_educ_share", "bachelor_share", "master_share", 
                 "doctorate_share", "any_exp_share", "ave_exp", "fulltime_share",
                 "internship_share")



# --- Load the data and prep the data ------------------------------------------

agg2_mn_analysis <- read_csv(file.path(data_analysis, "agg2_mn_analysis.csv"))

# Transform treatment indicator to numeric for tables
agg2_mn_analysis <- agg2_mn_analysis %>%
  mutate(
    treated_eff_full = as.numeric(treated_eff_full),
    treated_enact_full = as.numeric(treated_enact_full)
  )

# Grab MN ban date
ban_date <- agg2_mn_analysis %>%
  filter(ban_full == 1) %>%
  summarise(date = first(date_eff_full)) %>%
  pull(date)

# Create event time indicator for event studies 
agg2_mn_analysis <- agg2_mn_analysis %>%
  mutate(
    event_time = (year(date) - year(ban_date))*12 + (month(date) - month(ban_date))
  )

# Create truncated version for event studies
et_lb <- -36  # create event time lower bound

es_df <- agg2_mn_analysis %>%
  filter(event_time >= et_lb)

gc()



# --- TOTAL POSTINGS -----------------------------------------------------------

# --- Table ---  

# (i) Spec 1: TWFE 
twfe_total_postings_1 <- feols(
  total_postings ~ treated_eff_full | state + date,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_total_postings_1)


# (ii) Spec 2: Add Occupation FE
twfe_total_postings_2 <- feols(
  total_postings ~ treated_eff_full | state + date + soc_4,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_total_postings_2)


# ADDING NEW SPECIFICATION 3; IGNORING CONTROL VARIABLES FOR NOW

# (iii) Spec 3: Add Firm FE

twfe_total_postings_3 <- feols(
  total_postings ~ treated_eff_full | state + date + soc_4 + company,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_total_postings_3)


# Combine into table 
etable(
  twfe_total_postings_1, twfe_total_postings_2, twfe_total_postings_3, 
  tex = TRUE,
  tpt = TRUE,
  dict = c(
    total_postings = "Postings",
    treated_eff_full = "Treated $\\times$ Post",
    state = "State",
    date = "Month-Year",
    soc_4 = "Occupation",
    company = "Firm"
  ),
  style.tex = style.tex("aer"), # Removes weird "Model" thing and cleans up headings
  notes = c(
    "@\\footnotesize",
    "*Notes:* Standard errors are clustered at the state level.",
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.1"
  ),
  file = file.path(output_tables, "table_twfe_total_postings_agg2_mn.tex")
)

rm(twfe_total_postings_1, twfe_total_postings_2, twfe_total_postings_3)
gc()

# NOTE: Once I'm performing other methods too, I may need to figure out how to 
# combine all of these results in a different table. This is fine for now.






# --- Event Study ---

# (i) Spec 1: TWFE 
# Run event study specification 
es_total_postings_1 <- feols(
  total_postings ~ i(event_time, ban_full, -1) |
    state + date,
  data = es_df,
  cluster = ~state
)

summary(es_total_postings_1)

# create plotting dataframe 
es_total_postings_1_df <- tibble(
  term = names(es_total_postings_1$coefficients),
  estimate = as.numeric(es_total_postings_1$coefficients),
  std_error = as.numeric(es_total_postings_1$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_total_postings_1_df <- bind_rows(es_total_postings_1_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_total_postings_1 <- ggplot(es_total_postings_1_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Postings"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_total_postings_1

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_total_postings_1_agg2_mn.pdf"),
  es_plot_total_postings_1,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_total_postings_1, es_total_postings_1_df, es_plot_total_postings_1, ref_row)
gc()


# (ii) Spec 2: Add Occupation FE
# Run event study specification 
es_total_postings_2 <- feols(
  total_postings ~ i(event_time, ban_full, -1) |
    state + date + soc_4,
  data = es_df,
  cluster = ~state
)

summary(es_total_postings_2)

# create plotting dataframe 
es_total_postings_2_df <- tibble(
  term = names(es_total_postings_2$coefficients),
  estimate = as.numeric(es_total_postings_2$coefficients),
  std_error = as.numeric(es_total_postings_2$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_total_postings_2_df <- bind_rows(es_total_postings_2_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_total_postings_2 <- ggplot(es_total_postings_2_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Postings"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_total_postings_2

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_total_postings_2_agg2_mn.pdf"),
  es_plot_total_postings_2,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_total_postings_2, es_total_postings_2_df, es_plot_total_postings_2, ref_row)
gc()


# (iii) Spec 3: Add Firm FE 
# Run event study specification 
es_total_postings_3 <- feols(
  total_postings ~ i(event_time, ban_full, -1) |
    state + date + soc_4 + company,
  data = es_df,
  cluster = ~state
)

summary(es_total_postings_3)

# create plotting dataframe 
es_total_postings_3_df <- tibble(
  term = names(es_total_postings_3$coefficients),
  estimate = as.numeric(es_total_postings_3$coefficients),
  std_error = as.numeric(es_total_postings_3$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_total_postings_3_df <- bind_rows(es_total_postings_3_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_total_postings_3 <- ggplot(es_total_postings_3_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Postings"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_total_postings_3

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_total_postings_3_agg2_mn.pdf"),
  es_plot_total_postings_3,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_total_postings_3, es_total_postings_3_df, es_plot_total_postings_3, ref_row)
gc()




# --- ANY EDUCATION SHARE ------------------------------------------------------

# --- Table ---  

# (i) Spec 1: TWFE 
twfe_any_educ_share_1 <- feols(
  any_educ_share ~ treated_eff_full | state + date,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_any_educ_share_1)


# (ii) Spec 2: Add Occupation FE
twfe_any_educ_share_2 <- feols(
  any_educ_share ~ treated_eff_full | state + date + soc_4,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_any_educ_share_2)

# (iii) Spec 3: Add Firm FE

twfe_any_educ_share_3 <- feols(
  any_educ_share ~ treated_eff_full | state + date + soc_4 + company,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_any_educ_share_3)


# Combine into table 
etable(
  twfe_any_educ_share_1, twfe_any_educ_share_2, twfe_any_educ_share_3, 
  tex = TRUE,
  tpt = TRUE,
  dict = c(
    any_educ_share = "Any Education Requirement (Share)",
    treated_eff_full = "Treated $\\times$ Post",
    state = "State",
    date = "Month-Year",
    soc_4 = "Occupation",
    company = "Firm"
  ),
  style.tex = style.tex("aer"), # Removes weird "Model" thing and cleans up headings
  notes = c(
    "@\\footnotesize",
    "*Notes:* Standard errors are clustered at the state level.",
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.1"
  ),
  file = file.path(output_tables, "table_twfe_any_educ_share_agg2_mn.tex")
)

rm(twfe_any_educ_share_1, twfe_any_educ_share_2, twfe_any_educ_share_3)
gc()




# --- Event Study ---

# (i) Spec 1: TWFE 
# Run event study specification 
es_any_educ_share_1 <- feols(
  any_educ_share ~ i(event_time, ban_full, -1) |
    state + date,
  data = es_df,
  cluster = ~state
)

summary(es_any_educ_share_1)

# create plotting dataframe 
es_any_educ_share_1_df <- tibble(
  term = names(es_any_educ_share_1$coefficients),
  estimate = as.numeric(es_any_educ_share_1$coefficients),
  std_error = as.numeric(es_any_educ_share_1$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_any_educ_share_1_df <- bind_rows(es_any_educ_share_1_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_any_educ_share_1 <- ggplot(es_any_educ_share_1_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Any Education Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_any_educ_share_1

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_any_educ_share_1_agg2_mn.pdf"),
  es_plot_any_educ_share_1,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_any_educ_share_1, es_any_educ_share_1_df, es_plot_any_educ_share_1, ref_row)
gc()


# (ii) Spec 2: Add Occupation FE
# Run event study specification 
es_any_educ_share_2 <- feols(
  any_educ_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4,
  data = es_df,
  cluster = ~state
)

summary(es_any_educ_share_2)

# create plotting dataframe 
es_any_educ_share_2_df <- tibble(
  term = names(es_any_educ_share_2$coefficients),
  estimate = as.numeric(es_any_educ_share_2$coefficients),
  std_error = as.numeric(es_any_educ_share_2$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_any_educ_share_2_df <- bind_rows(es_any_educ_share_2_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_any_educ_share_2 <- ggplot(es_any_educ_share_2_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Any Education Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_any_educ_share_2

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_any_educ_share_2_agg2_mn.pdf"),
  es_plot_any_educ_share_2,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_any_educ_share_2, es_any_educ_share_2_df, es_plot_any_educ_share_2, ref_row)
gc()


# (iii) Spec 3: Add Firm FE 
# Run event study specification 
es_any_educ_share_3 <- feols(
  any_educ_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4 + company,
  data = es_df,
  cluster = ~state
)

summary(es_any_educ_share_3)

# create plotting dataframe 
es_any_educ_share_3_df <- tibble(
  term = names(es_any_educ_share_3$coefficients),
  estimate = as.numeric(es_any_educ_share_3$coefficients),
  std_error = as.numeric(es_any_educ_share_3$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_any_educ_share_3_df <- bind_rows(es_any_educ_share_3_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_any_educ_share_3 <- ggplot(es_any_educ_share_3_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Any Education Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_any_educ_share_3

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_any_educ_share_3_agg2_mn.pdf"),
  es_plot_any_educ_share_3,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_any_educ_share_3, es_any_educ_share_3_df, es_plot_any_educ_share_3, ref_row)
gc()





# --- BACHELOR'S SHARE ---------------------------------------------------------
# --- Table ---  

# (i) Spec 1: TWFE 
twfe_bachelor_share_1 <- feols(
  bachelor_share ~ treated_eff_full | state + date,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_bachelor_share_1)


# (ii) Spec 2: Add Occupation FE
twfe_bachelor_share_2 <- feols(
  bachelor_share ~ treated_eff_full | state + date + soc_4,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_bachelor_share_2)

# (iii) Spec 3: Add Firm FE

twfe_bachelor_share_3 <- feols(
  bachelor_share ~ treated_eff_full | state + date + soc_4 + company,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_bachelor_share_3)


# Combine into table 
etable(
  twfe_bachelor_share_1, twfe_bachelor_share_2, twfe_bachelor_share_3, 
  tex = TRUE,
  tpt = TRUE,
  dict = c(
    bachelor_share = "Bachelor's Requirement (Share)",
    treated_eff_full = "Treated $\\times$ Post",
    state = "State",
    date = "Month-Year",
    soc_4 = "Occupation",
    company = "Firm"
  ),
  style.tex = style.tex("aer"), # Removes weird "Model" thing and cleans up headings
  notes = c(
    "@\\footnotesize",
    "*Notes:* Standard errors are clustered at the state level.",
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.1"
  ),
  file = file.path(output_tables, "table_twfe_bachelor_share_agg2_mn.tex")
)

rm(twfe_bachelor_share_1, twfe_bachelor_share_2, twfe_bachelor_share_3)
gc()



# --- Event Study ---

# (i) Spec 1: TWFE 
# Run event study specification 
es_bachelor_share_1 <- feols(
  bachelor_share ~ i(event_time, ban_full, -1) |
    state + date,
  data = es_df,
  cluster = ~state
)

summary(es_bachelor_share_1)

# create plotting dataframe 
es_bachelor_share_1_df <- tibble(
  term = names(es_bachelor_share_1$coefficients),
  estimate = as.numeric(es_bachelor_share_1$coefficients),
  std_error = as.numeric(es_bachelor_share_1$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_bachelor_share_1_df <- bind_rows(es_bachelor_share_1_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_bachelor_share_1 <- ggplot(es_bachelor_share_1_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Bachelor's Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_bachelor_share_1

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_bachelor_share_1_agg2_mn.pdf"),
  es_plot_bachelor_share_1,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_bachelor_share_1, es_bachelor_share_1_df, es_plot_bachelor_share_1, ref_row)
gc()


# (ii) Spec 2: Add Occupation FE
# Run event study specification 
es_bachelor_share_2 <- feols(
  bachelor_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4,
  data = es_df,
  cluster = ~state
)

summary(es_bachelor_share_2)

# create plotting dataframe 
es_bachelor_share_2_df <- tibble(
  term = names(es_bachelor_share_2$coefficients),
  estimate = as.numeric(es_bachelor_share_2$coefficients),
  std_error = as.numeric(es_bachelor_share_2$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_bachelor_share_2_df <- bind_rows(es_bachelor_share_2_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_bachelor_share_2 <- ggplot(es_bachelor_share_2_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Bachelor's Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_bachelor_share_2

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_bachelor_share_2_agg2_mn.pdf"),
  es_plot_bachelor_share_2,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_bachelor_share_2, es_bachelor_share_2_df, es_plot_bachelor_share_2, ref_row)
gc()


# (iii) Spec 3: Add Firm FE 
# Run event study specification 
es_bachelor_share_3 <- feols(
  bachelor_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4 + company,
  data = es_df,
  cluster = ~state
)

summary(es_bachelor_share_3)

# create plotting dataframe 
es_bachelor_share_3_df <- tibble(
  term = names(es_bachelor_share_3$coefficients),
  estimate = as.numeric(es_bachelor_share_3$coefficients),
  std_error = as.numeric(es_bachelor_share_3$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_bachelor_share_3_df <- bind_rows(es_bachelor_share_3_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_bachelor_share_3 <- ggplot(es_bachelor_share_3_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Bachelor's Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_bachelor_share_3

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_bachelor_share_3_agg2_mn.pdf"),
  es_plot_bachelor_share_3,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_bachelor_share_3, es_bachelor_share_3_df, es_plot_bachelor_share_3, ref_row)
gc()





# --- MASTER'S SHARE -----------------------------------------------------------
# --- Table ---  

# (i) Spec 1: TWFE 
twfe_master_share_1 <- feols(
  master_share ~ treated_eff_full | state + date,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_master_share_1)


# (ii) Spec 2: Add Occupation FE
twfe_master_share_2 <- feols(
  master_share ~ treated_eff_full | state + date + soc_4,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_master_share_2)

# (iii) Spec 3: Add Firm FE

twfe_master_share_3 <- feols(
  master_share ~ treated_eff_full | state + date + soc_4 + company,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_master_share_3)


# Combine into table 
etable(
  twfe_master_share_1, twfe_master_share_2, twfe_master_share_3, 
  tex = TRUE,
  tpt = TRUE,
  dict = c(
    master_share = "Master's Requirement (Share)",
    treated_eff_full = "Treated $\\times$ Post",
    state = "State",
    date = "Month-Year",
    soc_4 = "Occupation",
    company = "Firm"
  ),
  style.tex = style.tex("aer"), # Removes weird "Model" thing and cleans up headings
  notes = c(
    "@\\footnotesize",
    "*Notes:* Standard errors are clustered at the state level.",
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.1"
  ),
  file = file.path(output_tables, "table_twfe_master_share_agg2_mn.tex")
)

rm(twfe_master_share_1, twfe_master_share_2, twfe_master_share_3)
gc()





# --- Event Study ---

# (i) Spec 1: TWFE 
# Run event study specification 
es_master_share_1 <- feols(
  master_share ~ i(event_time, ban_full, -1) |
    state + date,
  data = es_df,
  cluster = ~state
)

summary(es_master_share_1)

# create plotting dataframe 
es_master_share_1_df <- tibble(
  term = names(es_master_share_1$coefficients),
  estimate = as.numeric(es_master_share_1$coefficients),
  std_error = as.numeric(es_master_share_1$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_master_share_1_df <- bind_rows(es_master_share_1_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_master_share_1 <- ggplot(es_master_share_1_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Master's Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_master_share_1

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_master_share_1_agg2_mn.pdf"),
  es_plot_master_share_1,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_master_share_1, es_master_share_1_df, es_plot_master_share_1, ref_row)
gc()


# (ii) Spec 2: Add Occupation FE
# Run event study specification 
es_master_share_2 <- feols(
  master_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4,
  data = es_df,
  cluster = ~state
)

summary(es_master_share_2)

# create plotting dataframe 
es_master_share_2_df <- tibble(
  term = names(es_master_share_2$coefficients),
  estimate = as.numeric(es_master_share_2$coefficients),
  std_error = as.numeric(es_master_share_2$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_master_share_2_df <- bind_rows(es_master_share_2_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_master_share_2 <- ggplot(es_master_share_2_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Master's Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_master_share_2

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_master_share_2_agg2_mn.pdf"),
  es_plot_master_share_2,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_master_share_2, es_master_share_2_df, es_plot_master_share_2, ref_row)
gc()


# (iii) Spec 3: Add Firm FE 
# Run event study specification 
es_master_share_3 <- feols(
  master_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4 + company,
  data = es_df,
  cluster = ~state
)

summary(es_master_share_3)

# create plotting dataframe 
es_master_share_3_df <- tibble(
  term = names(es_master_share_3$coefficients),
  estimate = as.numeric(es_master_share_3$coefficients),
  std_error = as.numeric(es_master_share_3$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_master_share_3_df <- bind_rows(es_master_share_3_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_master_share_3 <- ggplot(es_master_share_3_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Master's Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_master_share_3

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_master_share_3_agg2_mn.pdf"),
  es_plot_master_share_3,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_master_share_3, es_master_share_3_df, es_plot_master_share_3, ref_row)
gc()




# --- DOCTORATE SHARE ----------------------------------------------------------
# --- Table ---  

# (i) Spec 1: TWFE 
twfe_doctorate_share_1 <- feols(
  doctorate_share ~ treated_eff_full | state + date,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_doctorate_share_1)


# (ii) Spec 2: Add Occupation FE
twfe_doctorate_share_2 <- feols(
  doctorate_share ~ treated_eff_full | state + date + soc_4,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_doctorate_share_2)


# (iii) Spec 3: Add Firm FE

twfe_doctorate_share_3 <- feols(
  doctorate_share ~ treated_eff_full | state + date + soc_4 + company,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_doctorate_share_3)


# Combine into table 
etable(
  twfe_doctorate_share_1, twfe_doctorate_share_2, twfe_doctorate_share_3, 
  tex = TRUE,
  tpt = TRUE,
  dict = c(
    doctorate_share = "Doctorate Requirement (Share)",
    treated_eff_full = "Treated $\\times$ Post",
    state = "State",
    date = "Month-Year",
    soc_4 = "Occupation",
    company = "Firm"
  ),
  style.tex = style.tex("aer"), # Removes weird "Model" thing and cleans up headings
  notes = c(
    "@\\footnotesize",
    "*Notes:* Standard errors are clustered at the state level.",
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.1"
  ),
  file = file.path(output_tables, "table_twfe_doctorate_share_agg2_mn.tex")
)

rm(twfe_doctorate_share_1, twfe_doctorate_share_2, twfe_doctorate_share_3)
gc()





# --- Event Study ---

# (i) Spec 1: TWFE 
# Run event study specification 
es_doctorate_share_1 <- feols(
  doctorate_share ~ i(event_time, ban_full, -1) |
    state + date,
  data = es_df,
  cluster = ~state
)

summary(es_doctorate_share_1)

# create plotting dataframe 
es_doctorate_share_1_df <- tibble(
  term = names(es_doctorate_share_1$coefficients),
  estimate = as.numeric(es_doctorate_share_1$coefficients),
  std_error = as.numeric(es_doctorate_share_1$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_doctorate_share_1_df <- bind_rows(es_doctorate_share_1_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_doctorate_share_1 <- ggplot(es_doctorate_share_1_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Doctorate Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_doctorate_share_1

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_doctorate_share_1_agg2_mn.pdf"),
  es_plot_doctorate_share_1,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_doctorate_share_1, es_doctorate_share_1_df, es_plot_doctorate_share_1, ref_row)
gc()


# (ii) Spec 2: Add Occupation FE
# Run event study specification 
es_doctorate_share_2 <- feols(
  doctorate_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4,
  data = es_df,
  cluster = ~state
)

summary(es_doctorate_share_2)

# create plotting dataframe 
es_doctorate_share_2_df <- tibble(
  term = names(es_doctorate_share_2$coefficients),
  estimate = as.numeric(es_doctorate_share_2$coefficients),
  std_error = as.numeric(es_doctorate_share_2$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_doctorate_share_2_df <- bind_rows(es_doctorate_share_2_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_doctorate_share_2 <- ggplot(es_doctorate_share_2_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Doctorate Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_doctorate_share_2

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_doctorate_share_2_agg2_mn.pdf"),
  es_plot_doctorate_share_2,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_doctorate_share_2, es_doctorate_share_2_df, es_plot_doctorate_share_2, ref_row)
gc()



# (iii) Spec 3: Add Firm FE 
# Run event study specification 
es_doctorate_share_3 <- feols(
  doctorate_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4 + company,
  data = es_df,
  cluster = ~state
)

summary(es_doctorate_share_3)

# create plotting dataframe 
es_doctorate_share_3_df <- tibble(
  term = names(es_doctorate_share_3$coefficients),
  estimate = as.numeric(es_doctorate_share_3$coefficients),
  std_error = as.numeric(es_doctorate_share_3$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_doctorate_share_3_df <- bind_rows(es_doctorate_share_3_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_doctorate_share_3 <- ggplot(es_doctorate_share_3_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Doctorate Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_doctorate_share_3

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_doctorate_share_3_agg2_mn.pdf"),
  es_plot_doctorate_share_3,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_doctorate_share_3, es_doctorate_share_3_df, es_plot_doctorate_share_3, ref_row)
gc()



# --- ANY EXPERIENCE SHARE -----------------------------------------------------
# --- Table ---  

# (i) Spec 1: TWFE 
twfe_any_exp_share_1 <- feols(
  any_exp_share ~ treated_eff_full | state + date,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_any_exp_share_1)


# (ii) Spec 2: Add Occupation FE
twfe_any_exp_share_2 <- feols(
  any_exp_share ~ treated_eff_full | state + date + soc_4,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_any_exp_share_2)

# (iii) Spec 3: Add Firm FE

twfe_any_exp_share_3 <- feols(
  any_exp_share ~ treated_eff_full | state + date + soc_4 + company,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_any_exp_share_3)


# Combine into table 
etable(
  twfe_any_exp_share_1, twfe_any_exp_share_2, twfe_any_exp_share_3, 
  tex = TRUE,
  tpt = TRUE,
  dict = c(
    any_exp_share = "Any Experience Requirement (Share)",
    treated_eff_full = "Treated $\\times$ Post",
    state = "State",
    date = "Month-Year",
    soc_4 = "Occupation",
    company = "Firm"
  ),
  style.tex = style.tex("aer"), # Removes weird "Model" thing and cleans up headings
  notes = c(
    "@\\footnotesize",
    "*Notes:* Standard errors are clustered at the state level.",
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.1"
  ),
  file = file.path(output_tables, "table_twfe_any_exp_share_agg2_mn.tex")
)

rm(twfe_any_exp_share_1, twfe_any_exp_share_2, twfe_any_exp_share_3)
gc()





# --- Event Study ---

# (i) Spec 1: TWFE 
# Run event study specification 
es_any_exp_share_1 <- feols(
  any_exp_share ~ i(event_time, ban_full, -1) |
    state + date,
  data = es_df,
  cluster = ~state
)

summary(es_any_exp_share_1)

# create plotting dataframe 
es_any_exp_share_1_df <- tibble(
  term = names(es_any_exp_share_1$coefficients),
  estimate = as.numeric(es_any_exp_share_1$coefficients),
  std_error = as.numeric(es_any_exp_share_1$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_any_exp_share_1_df <- bind_rows(es_any_exp_share_1_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_any_exp_share_1 <- ggplot(es_any_exp_share_1_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Any Experience Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_any_exp_share_1

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_any_exp_share_1_agg2_mn.pdf"),
  es_plot_any_exp_share_1,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_any_exp_share_1, es_any_exp_share_1_df, es_plot_any_exp_share_1, ref_row)
gc()


# (ii) Spec 2: Add Occupation FE
# Run event study specification 
es_any_exp_share_2 <- feols(
  any_exp_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4,
  data = es_df,
  cluster = ~state
)

summary(es_any_exp_share_2)

# create plotting dataframe 
es_any_exp_share_2_df <- tibble(
  term = names(es_any_exp_share_2$coefficients),
  estimate = as.numeric(es_any_exp_share_2$coefficients),
  std_error = as.numeric(es_any_exp_share_2$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_any_exp_share_2_df <- bind_rows(es_any_exp_share_2_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_any_exp_share_2 <- ggplot(es_any_exp_share_2_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Any Experience Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_any_exp_share_2

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_any_exp_share_2_agg2_mn.pdf"),
  es_plot_any_exp_share_2,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_any_exp_share_2, es_any_exp_share_2_df, es_plot_any_exp_share_2, ref_row)
gc()



# (iii) Spec 3: Add Firm FE 
# Run event study specification 
es_any_exp_share_3 <- feols(
  any_exp_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4 + company,
  data = es_df,
  cluster = ~state
)

summary(es_any_exp_share_3)

# create plotting dataframe 
es_any_exp_share_3_df <- tibble(
  term = names(es_any_exp_share_3$coefficients),
  estimate = as.numeric(es_any_exp_share_3$coefficients),
  std_error = as.numeric(es_any_exp_share_3$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_any_exp_share_3_df <- bind_rows(es_any_exp_share_3_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_any_exp_share_3 <- ggplot(es_any_exp_share_3_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Any Experience Requirement (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_any_exp_share_3

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_any_exp_share_3_agg2_mn.pdf"),
  es_plot_any_exp_share_3,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_any_exp_share_3, es_any_exp_share_3_df, es_plot_any_exp_share_3, ref_row)
gc()




# --- AVERAGE EXPERIENCE -------------------------------------------------------
# --- Table ---  

# (i) Spec 1: TWFE 
twfe_ave_exp_1 <- feols(
  ave_exp ~ treated_eff_full | state + date,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_ave_exp_1)


# (ii) Spec 2: Add Occupation FE
twfe_ave_exp_2 <- feols(
  ave_exp ~ treated_eff_full | state + date + soc_4,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_ave_exp_2)

# (iii) Spec 3: Add Firm FE

twfe_ave_exp_3 <- feols(
  ave_exp ~ treated_eff_full | state + date + soc_4 + company,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_ave_exp_3)


# Combine into table 
etable(
  twfe_ave_exp_1, twfe_ave_exp_2, twfe_ave_exp_3, 
  tex = TRUE,
  tpt = TRUE,
  dict = c(
    ave_exp = "Average Experience",
    treated_eff_full = "Treated $\\times$ Post",
    state = "State",
    date = "Month-Year",
    soc_4 = "Occupation",
    company = "Firm"
  ),
  style.tex = style.tex("aer"), # Removes weird "Model" thing and cleans up headings
  notes = c(
    "@\\footnotesize",
    "*Notes:* Standard errors are clustered at the state level.",
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.1"
  ),
  file = file.path(output_tables, "table_twfe_ave_exp_agg2_mn.tex")
)

rm(twfe_ave_exp_1, twfe_ave_exp_2, twfe_ave_exp_3)
gc()





# --- Event Study ---

# (i) Spec 1: TWFE 
# Run event study specification 
es_ave_exp_1 <- feols(
  ave_exp ~ i(event_time, ban_full, -1) |
    state + date,
  data = es_df,
  cluster = ~state
)

summary(es_ave_exp_1)

# create plotting dataframe 
es_ave_exp_1_df <- tibble(
  term = names(es_ave_exp_1$coefficients),
  estimate = as.numeric(es_ave_exp_1$coefficients),
  std_error = as.numeric(es_ave_exp_1$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_ave_exp_1_df <- bind_rows(es_ave_exp_1_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_ave_exp_1 <- ggplot(es_ave_exp_1_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Average Experience"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_ave_exp_1

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_ave_exp_1_agg2_mn.pdf"),
  es_plot_ave_exp_1,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_ave_exp_1, es_ave_exp_1_df, es_plot_ave_exp_1, ref_row)
gc()


# (ii) Spec 2: Add Occupation FE
# Run event study specification 
es_ave_exp_2 <- feols(
  ave_exp ~ i(event_time, ban_full, -1) |
    state + date + soc_4,
  data = es_df,
  cluster = ~state
)

summary(es_ave_exp_2)

# create plotting dataframe 
es_ave_exp_2_df <- tibble(
  term = names(es_ave_exp_2$coefficients),
  estimate = as.numeric(es_ave_exp_2$coefficients),
  std_error = as.numeric(es_ave_exp_2$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_ave_exp_2_df <- bind_rows(es_ave_exp_2_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_ave_exp_2 <- ggplot(es_ave_exp_2_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Average Experience"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_ave_exp_2

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_ave_exp_2_agg2_mn.pdf"),
  es_plot_ave_exp_2,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_ave_exp_2, es_ave_exp_2_df, es_plot_ave_exp_2, ref_row)
gc()



# (iii) Spec 3: Add Firm FE 
# Run event study specification 
es_ave_exp_3 <- feols(
  ave_exp ~ i(event_time, ban_full, -1) |
    state + date + soc_4 + company,
  data = es_df,
  cluster = ~state
)

summary(es_ave_exp_3)

# create plotting dataframe 
es_ave_exp_3_df <- tibble(
  term = names(es_ave_exp_3$coefficients),
  estimate = as.numeric(es_ave_exp_3$coefficients),
  std_error = as.numeric(es_ave_exp_3$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_ave_exp_3_df <- bind_rows(es_ave_exp_3_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_ave_exp_3 <- ggplot(es_ave_exp_3_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Average Experience"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_ave_exp_3

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_ave_exp_3_agg2_mn.pdf"),
  es_plot_ave_exp_3,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_ave_exp_3, es_ave_exp_3_df, es_plot_ave_exp_3, ref_row)
gc()






# --- FULL-TIME SHARE ----------------------------------------------------------
# --- Table ---  

# (i) Spec 1: TWFE 
twfe_fulltime_share_1 <- feols(
  fulltime_share ~ treated_eff_full | state + date,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_fulltime_share_1)


# (ii) Spec 2: Add Occupation FE
twfe_fulltime_share_2 <- feols(
  fulltime_share ~ treated_eff_full | state + date + soc_4,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_fulltime_share_2)

# (iii) Spec 3: Add Firm FE

twfe_fulltime_share_3 <- feols(
  fulltime_share ~ treated_eff_full | state + date + soc_4 + company,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_fulltime_share_3)


# Combine into table 
etable(
  twfe_fulltime_share_1, twfe_fulltime_share_2, twfe_fulltime_share_3, 
  tex = TRUE,
  tpt = TRUE,
  dict = c(
    fulltime_share = "Full-Time Postings (Share)",
    treated_eff_full = "Treated $\\times$ Post",
    state = "State",
    date = "Month-Year",
    soc_4 = "Occupation",
    company = "Firm"
  ),
  style.tex = style.tex("aer"), # Removes weird "Model" thing and cleans up headings
  notes = c(
    "@\\footnotesize",
    "*Notes:* Standard errors are clustered at the state level.",
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.1"
  ),
  file = file.path(output_tables, "table_twfe_fulltime_share_agg2_mn.tex")
)

rm(twfe_fulltime_share_1, twfe_fulltime_share_2, twfe_fulltime_share_3)
gc()






# --- Event Study ---

# (i) Spec 1: TWFE 
# Run event study specification 
es_fulltime_share_1 <- feols(
  fulltime_share ~ i(event_time, ban_full, -1) |
    state + date,
  data = es_df,
  cluster = ~state
)

summary(es_fulltime_share_1)

# create plotting dataframe 
es_fulltime_share_1_df <- tibble(
  term = names(es_fulltime_share_1$coefficients),
  estimate = as.numeric(es_fulltime_share_1$coefficients),
  std_error = as.numeric(es_fulltime_share_1$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_fulltime_share_1_df <- bind_rows(es_fulltime_share_1_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_fulltime_share_1 <- ggplot(es_fulltime_share_1_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Full-Time Postings (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_fulltime_share_1

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_fulltime_share_1_agg2_mn.pdf"),
  es_plot_fulltime_share_1,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_fulltime_share_1, es_fulltime_share_1_df, es_plot_fulltime_share_1, ref_row)
gc()


# (ii) Spec 2: Add Occupation FE
# Run event study specification 
es_fulltime_share_2 <- feols(
  fulltime_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4,
  data = es_df,
  cluster = ~state
)

summary(es_fulltime_share_2)

# create plotting dataframe 
es_fulltime_share_2_df <- tibble(
  term = names(es_fulltime_share_2$coefficients),
  estimate = as.numeric(es_fulltime_share_2$coefficients),
  std_error = as.numeric(es_fulltime_share_2$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_fulltime_share_2_df <- bind_rows(es_fulltime_share_2_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_fulltime_share_2 <- ggplot(es_fulltime_share_2_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Full-Time Postings (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_fulltime_share_2

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_fulltime_share_2_agg2_mn.pdf"),
  es_plot_fulltime_share_2,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_fulltime_share_2, es_fulltime_share_2_df, es_plot_fulltime_share_2, ref_row)
gc()



# (iii) Spec 3: Add Firm FE 
# Run event study specification 
es_fulltime_share_3 <- feols(
  fulltime_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4 + company,
  data = es_df,
  cluster = ~state
)

summary(es_fulltime_share_3)

# create plotting dataframe 
es_fulltime_share_3_df <- tibble(
  term = names(es_fulltime_share_3$coefficients),
  estimate = as.numeric(es_fulltime_share_3$coefficients),
  std_error = as.numeric(es_fulltime_share_3$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_fulltime_share_3_df <- bind_rows(es_fulltime_share_3_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_fulltime_share_3 <- ggplot(es_fulltime_share_3_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Full-Time Postings (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_fulltime_share_3

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_fulltime_share_3_agg2_mn.pdf"),
  es_plot_fulltime_share_3,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_fulltime_share_3, es_fulltime_share_3_df, es_plot_fulltime_share_3, ref_row)
gc()




# --- INTERNSHIP SHARE ---------------------------------------------------------
# --- Table ---  

# (i) Spec 1: TWFE 
twfe_internship_share_1 <- feols(
  internship_share ~ treated_eff_full | state + date,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_internship_share_1)


# (ii) Spec 2: Add Occupation FE
twfe_internship_share_2 <- feols(
  internship_share ~ treated_eff_full | state + date + soc_4,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_internship_share_2)

# (iii) Spec 3: Add Firm FE

twfe_internship_share_3 <- feols(
  internship_share ~ treated_eff_full | state + date + soc_4 + company,
  data = agg2_mn_analysis,
  cluster = ~state
)

summary(twfe_internship_share_3)


# Combine into table 
etable(
  twfe_internship_share_1, twfe_internship_share_2, twfe_internship_share_3, 
  tex = TRUE,
  tpt = TRUE,
  dict = c(
    internship_share = "Internship Postings (Share)",
    treated_eff_full = "Treated $\\times$ Post",
    state = "State",
    date = "Month-Year",
    soc_4 = "Occupation",
    company = "Firm"
  ),
  style.tex = style.tex("aer"), # Removes weird "Model" thing and cleans up headings
  notes = c(
    "@\\footnotesize",
    "*Notes:* Standard errors are clustered at the state level.",
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.1"
  ),
  file = file.path(output_tables, "table_twfe_internship_share_agg2_mn.tex")
)

rm(twfe_internship_share_1, twfe_internship_share_2, twfe_internship_share_3)
gc()





# --- Event Study ---

# (i) Spec 1: TWFE 
# Run event study specification 
es_internship_share_1 <- feols(
  internship_share ~ i(event_time, ban_full, -1) |
    state + date,
  data = es_df,
  cluster = ~state
)

summary(es_internship_share_1)

# create plotting dataframe 
es_internship_share_1_df <- tibble(
  term = names(es_internship_share_1$coefficients),
  estimate = as.numeric(es_internship_share_1$coefficients),
  std_error = as.numeric(es_internship_share_1$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_internship_share_1_df <- bind_rows(es_internship_share_1_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_internship_share_1 <- ggplot(es_internship_share_1_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Internship Postings (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_internship_share_1

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_internship_share_1_agg2_mn.pdf"),
  es_plot_internship_share_1,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_internship_share_1, es_internship_share_1_df, es_plot_internship_share_1, ref_row)
gc()


# (ii) Spec 2: Add Occupation FE
# Run event study specification 
es_internship_share_2 <- feols(
  internship_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4,
  data = es_df,
  cluster = ~state
)

summary(es_internship_share_2)

# create plotting dataframe 
es_internship_share_2_df <- tibble(
  term = names(es_internship_share_2$coefficients),
  estimate = as.numeric(es_internship_share_2$coefficients),
  std_error = as.numeric(es_internship_share_2$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_internship_share_2_df <- bind_rows(es_internship_share_2_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_internship_share_2 <- ggplot(es_internship_share_2_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Internship Postings (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_internship_share_2

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_internship_share_2_agg2_mn.pdf"),
  es_plot_internship_share_2,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_internship_share_2, es_internship_share_2_df, es_plot_internship_share_2, ref_row)
gc()



# (iii) Spec 3: Add Firm FE 
# Run event study specification 
es_internship_share_3 <- feols(
  internship_share ~ i(event_time, ban_full, -1) |
    state + date + soc_4 + company,
  data = es_df,
  cluster = ~state
)

summary(es_internship_share_3)

# create plotting dataframe 
es_internship_share_3_df <- tibble(
  term = names(es_internship_share_3$coefficients),
  estimate = as.numeric(es_internship_share_3$coefficients),
  std_error = as.numeric(es_internship_share_3$se)
) %>%
  mutate(
    event_time = as.numeric(str_extract(term, "-?\\d+")),
    ci_low = estimate - 1.96 * std_error, # 95% CIs 
    ci_high = estimate + 1.96 * std_error
  )

ref_row <- tibble( #adding back in an entry for the omitted period 
  term = "Reference",
  estimate = 0, 
  std_error = NA_real_,
  event_time = -1,
  ci_low = NA_real_,
  ci_high = NA_real_
)

es_internship_share_3_df <- bind_rows(es_internship_share_3_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_internship_share_3 <- ggplot(es_internship_share_3_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Internship Postings (Share)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_internship_share_3

# Save the event study plot 
ggsave(
  file.path(output_figures, "es_internship_share_3_agg2_mn.pdf"),
  es_plot_internship_share_3,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(es_internship_share_3, es_internship_share_3_df, es_plot_internship_share_3, ref_row)
gc()




