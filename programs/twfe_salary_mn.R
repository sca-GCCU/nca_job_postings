##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "twfe_salary_mn.R" 
# by: Sebastian C. Anastasi
# Date of this version: April 1, 2026
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

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

library(fixest)
library(tidyverse)



# --- Load the data and prep the data ------------------------------------------

salary_mn_analysis <- read_csv("data/analysis-data/salary_mn_analysis.csv")

#outcome_var <- c("real_salary", "real_salary_from", "real_salary_to")

# NOTE: 13,819 observations are missing salary information. I thought we  
# restricted to observations with non-missing salary information, so I'm confused.
# 
# salary_mn_analysis %>%
#   filter(is.na(real_salary_to)) %>%
#   select(real_salary, real_salary_from, real_salary_to)

# Transform treatment indicator to numeric for tables
salary_mn_analysis <- salary_mn_analysis %>%
  mutate(
    treated_eff_full = as.numeric(treated_eff_full),
    treated_enact_full = as.numeric(treated_enact_full)
  )

# Grab MN ban date
ban_date <- salary_mn_analysis %>%
  filter(ban_full == 1) %>%
  summarise(date = first(date_eff_full)) %>%
  pull(date)

# Create event time indicator for event studies 
salary_mn_analysis <- salary_mn_analysis %>%
  mutate(
    event_time = (year(date) - year(ban_date))*12 + (month(date) - month(ban_date))
  )

# Create truncated version for event studies
et_lb <- -36  # create event time lower bound

es_df <- salary_mn_analysis %>%
  filter(event_time >= et_lb)



# --- Salary (Real) ------------------------------------------------------------

# --- Table ---  

# (i) Spec 1: TWFE 
twfe_real_salary_1 <- feols(
  real_salary ~ treated_eff_full | state + date,
  data = salary_mn_analysis,
  cluster = ~state
)

summary(twfe_real_salary_1)


# (ii) Spec 2: Add Occupation FE
twfe_real_salary_2 <- feols(
  real_salary ~ treated_eff_full | state + date + soc_4,
  data = salary_mn_analysis,
  cluster = ~state
)

summary(twfe_real_salary_2)


# ADDING NEW SPECIFICATION 3; IGNORING CONTROL VARIABLES FOR NOW

# (iii) Spec 3: Add Firm FE

twfe_real_salary_3 <- feols(
  real_salary ~ treated_eff_full | state + date + soc_4 + company,
  data = salary_mn_analysis,
  cluster = ~state
)

summary(twfe_real_salary_3)


# Combine into table 
etable(
  twfe_real_salary_1, twfe_real_salary_2, twfe_real_salary_3, 
  tex = TRUE,
  tpt = TRUE,
  dict = c(
    real_salary = "Salary (Real)",
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
  file = "output/tables/table_twfe_real_salary_salary_mn.tex"
)

# NOTE: Once I'm performing other methods too, I may need to figure out how to 
# combine all of these results in a different table. This is fine for now.






# --- Event Study ---

# (i) Spec 1: TWFE 
# Run event study specification 
es_real_salary_1 <- feols(
  real_salary ~ i(event_time, ban_full, -1) |
    state + date,
  data = es_df,
  cluster = ~state
)

summary(es_real_salary_1)

# create plotting dataframe 
es_real_salary_1_df <- tibble(
  term = names(es_real_salary_1$coefficients),
  estimate = as.numeric(es_real_salary_1$coefficients),
  std_error = as.numeric(es_real_salary_1$se)
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

es_real_salary_1_df <- bind_rows(es_real_salary_1_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_real_salary_1 <- ggplot(es_real_salary_1_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Salary (Real)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_real_salary_1

# Save the event study plot 
ggsave(
  "output/figures/es_real_salary_1_salary_mn.pdf",
  es_plot_real_salary_1,
  width = 7,
  height = 4.5,
  units = "in"
)


# (ii) Spec 2: Add Occupation FE
# Run event study specification 
es_real_salary_2 <- feols(
  real_salary ~ i(event_time, ban_full, -1) |
    state + date + soc_4,
  data = es_df,
  cluster = ~state
)

summary(es_real_salary_2)

# create plotting dataframe 
es_real_salary_2_df <- tibble(
  term = names(es_real_salary_2$coefficients),
  estimate = as.numeric(es_real_salary_2$coefficients),
  std_error = as.numeric(es_real_salary_2$se)
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

es_real_salary_2_df <- bind_rows(es_real_salary_2_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_real_salary_2 <- ggplot(es_real_salary_2_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Salary (Real)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_real_salary_2

# Save the event study plot 
ggsave(
  "output/figures/es_real_salary_2_salary_mn.pdf",
  es_plot_real_salary_2,
  width = 7,
  height = 4.5,
  units = "in"
)


# (iii) Spec 3: Add Firm FE 
# Run event study specification 
es_real_salary_3 <- feols(
  real_salary ~ i(event_time, ban_full, -1) |
    state + date + soc_4 + company,
  data = es_df,
  cluster = ~state
)

summary(es_real_salary_3)

# create plotting dataframe 
es_real_salary_3_df <- tibble(
  term = names(es_real_salary_3$coefficients),
  estimate = as.numeric(es_real_salary_3$coefficients),
  std_error = as.numeric(es_real_salary_3$se)
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

es_real_salary_3_df <- bind_rows(es_real_salary_3_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_real_salary_3 <- ggplot(es_real_salary_3_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Salary (Real)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_real_salary_3

# Save the event study plot 
ggsave(
  "output/figures/es_real_salary_3_salary_mn.pdf",
  es_plot_real_salary_3,
  width = 7,
  height = 4.5,
  units = "in"
)






# --- REAL SALARY FROM ---------------------------------------------------------

# --- Table ---  

# (i) Spec 1: TWFE 
twfe_real_salary_from_1 <- feols(
  real_salary_from ~ treated_eff_full | state + date,
  data = salary_mn_analysis,
  cluster = ~state
)

summary(twfe_real_salary_from_1)


# (ii) Spec 2: Add Occupation FE
twfe_real_salary_from_2 <- feols(
  real_salary_from ~ treated_eff_full | state + date + soc_4,
  data = salary_mn_analysis,
  cluster = ~state
)

summary(twfe_real_salary_from_2)


# ADDING NEW SPECIFICATION 3; IGNORING CONTROL VARIABLES FOR NOW

# (iii) Spec 3: Add Firm FE

twfe_real_salary_from_3 <- feols(
  real_salary_from ~ treated_eff_full | state + date + soc_4 + company,
  data = salary_mn_analysis,
  cluster = ~state
)

summary(twfe_real_salary_from_3)


# Combine into table 
etable(
  twfe_real_salary_from_1, twfe_real_salary_from_2, twfe_real_salary_from_3, 
  tex = TRUE,
  tpt = TRUE,
  dict = c(
    real_salary_from = "Salary Lower Threshold (Real)",
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
  file = "output/tables/table_twfe_real_salary_from_salary_mn.tex"
)

# NOTE: Once I'm performing other methods too, I may need to figure out how to 
# combine all of these results in a different table. This is fine for now.






# --- Event Study ---

# (i) Spec 1: TWFE 
# Run event study specification 
es_real_salary_from_1 <- feols(
  real_salary_from ~ i(event_time, ban_full, -1) |
    state + date,
  data = es_df,
  cluster = ~state
)

summary(es_real_salary_from_1)

# create plotting dataframe 
es_real_salary_from_1_df <- tibble(
  term = names(es_real_salary_from_1$coefficients),
  estimate = as.numeric(es_real_salary_from_1$coefficients),
  std_error = as.numeric(es_real_salary_from_1$se)
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

es_real_salary_from_1_df <- bind_rows(es_real_salary_from_1_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_real_salary_from_1 <- ggplot(es_real_salary_from_1_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Salary Lower Threshold (Real)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_real_salary_from_1

# Save the event study plot 
ggsave(
  "output/figures/es_real_salary_from_1_salary_mn.pdf",
  es_plot_real_salary_from_1,
  width = 7,
  height = 4.5,
  units = "in"
)


# (ii) Spec 2: Add Occupation FE
# Run event study specification 
es_real_salary_from_2 <- feols(
  real_salary_from ~ i(event_time, ban_full, -1) |
    state + date + soc_4,
  data = es_df,
  cluster = ~state
)

summary(es_real_salary_from_2)

# create plotting dataframe 
es_real_salary_from_2_df <- tibble(
  term = names(es_real_salary_from_2$coefficients),
  estimate = as.numeric(es_real_salary_from_2$coefficients),
  std_error = as.numeric(es_real_salary_from_2$se)
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

es_real_salary_from_2_df <- bind_rows(es_real_salary_from_2_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_real_salary_from_2 <- ggplot(es_real_salary_from_2_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Salary Lower Threshold (Real)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_real_salary_from_2

# Save the event study plot 
ggsave(
  "output/figures/es_real_salary_from_2_salary_mn.pdf",
  es_plot_real_salary_from_2,
  width = 7,
  height = 4.5,
  units = "in"
)


# (iii) Spec 3: Add Firm FE 
# Run event study specification 
es_real_salary_from_3 <- feols(
  real_salary_from ~ i(event_time, ban_full, -1) |
    state + date + soc_4 + company,
  data = es_df,
  cluster = ~state
)

summary(es_real_salary_from_3)

# create plotting dataframe 
es_real_salary_from_3_df <- tibble(
  term = names(es_real_salary_from_3$coefficients),
  estimate = as.numeric(es_real_salary_from_3$coefficients),
  std_error = as.numeric(es_real_salary_from_3$se)
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

es_real_salary_from_3_df <- bind_rows(es_real_salary_from_3_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_real_salary_from_3 <- ggplot(es_real_salary_from_3_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Salary Lower Threshold (Real)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_real_salary_from_3

# Save the event study plot 
ggsave(
  "output/figures/es_real_salary_from_3_salary_mn.pdf",
  es_plot_real_salary_from_3,
  width = 7,
  height = 4.5,
  units = "in"
)






# --- REAL SALARY TO -----------------------------------------------------------

# --- Table ---  

# (i) Spec 1: TWFE 
twfe_real_salary_to_1 <- feols(
  real_salary_to ~ treated_eff_full | state + date,
  data = salary_mn_analysis,
  cluster = ~state
)

summary(twfe_real_salary_to_1)


# (ii) Spec 2: Add Occupation FE
twfe_real_salary_to_2 <- feols(
  real_salary_to ~ treated_eff_full | state + date + soc_4,
  data = salary_mn_analysis,
  cluster = ~state
)

summary(twfe_real_salary_to_2)


# ADDING NEW SPECIFICATION 3; IGNORING CONTROL VARIABLES FOR NOW

# (iii) Spec 3: Add Firm FE

twfe_real_salary_to_3 <- feols(
  real_salary_to ~ treated_eff_full | state + date + soc_4 + company,
  data = salary_mn_analysis,
  cluster = ~state
)

summary(twfe_real_salary_to_3)


# Combine into table 
etable(
  twfe_real_salary_to_1, twfe_real_salary_to_2, twfe_real_salary_to_3, 
  tex = TRUE,
  tpt = TRUE,
  dict = c(
    real_salary_to = "Salary Upper Threshold (Real)",
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
  file = "output/tables/table_twfe_real_salary_to_salary_mn.tex"
)

# NOTE: Once I'm performing other methods too, I may need to figure out how to 
# combine all of these results in a different table. This is fine for now.






# --- Event Study ---

# (i) Spec 1: TWFE 
# Run event study specification 
es_real_salary_to_1 <- feols(
  real_salary_to ~ i(event_time, ban_full, -1) |
    state + date,
  data = es_df,
  cluster = ~state
)

summary(es_real_salary_to_1)

# create plotting dataframe 
es_real_salary_to_1_df <- tibble(
  term = names(es_real_salary_to_1$coefficients),
  estimate = as.numeric(es_real_salary_to_1$coefficients),
  std_error = as.numeric(es_real_salary_to_1$se)
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

es_real_salary_to_1_df <- bind_rows(es_real_salary_to_1_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_real_salary_to_1 <- ggplot(es_real_salary_to_1_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Salary Upper Threshold (Real)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_real_salary_to_1

# Save the event study plot 
ggsave(
  "output/figures/es_real_salary_to_1_salary_mn.pdf",
  es_plot_real_salary_to_1,
  width = 7,
  height = 4.5,
  units = "in"
)


# (ii) Spec 2: Add Occupation FE
# Run event study specification 
es_real_salary_to_2 <- feols(
  real_salary_to ~ i(event_time, ban_full, -1) |
    state + date + soc_4,
  data = es_df,
  cluster = ~state
)

summary(es_real_salary_to_2)

# create plotting dataframe 
es_real_salary_to_2_df <- tibble(
  term = names(es_real_salary_to_2$coefficients),
  estimate = as.numeric(es_real_salary_to_2$coefficients),
  std_error = as.numeric(es_real_salary_to_2$se)
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

es_real_salary_to_2_df <- bind_rows(es_real_salary_to_2_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_real_salary_to_2 <- ggplot(es_real_salary_to_2_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Salary Upper Threshold (Real)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_real_salary_to_2

# Save the event study plot 
ggsave(
  "output/figures/es_real_salary_to_2_salary_mn.pdf",
  es_plot_real_salary_to_2,
  width = 7,
  height = 4.5,
  units = "in"
)


# (iii) Spec 3: Add Firm FE 
# Run event study specification 
es_real_salary_to_3 <- feols(
  real_salary_to ~ i(event_time, ban_full, -1) |
    state + date + soc_4 + company,
  data = es_df,
  cluster = ~state
)

summary(es_real_salary_to_3)

# create plotting dataframe 
es_real_salary_to_3_df <- tibble(
  term = names(es_real_salary_to_3$coefficients),
  estimate = as.numeric(es_real_salary_to_3$coefficients),
  std_error = as.numeric(es_real_salary_to_3$se)
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

es_real_salary_to_3_df <- bind_rows(es_real_salary_to_3_df, ref_row) %>%
  arrange(event_time)

# Create plot with ggplot 
es_plot_real_salary_to_3 <- ggplot(es_real_salary_to_3_df, aes(x = event_time, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + 
  geom_vline(xintercept = -1, linetype = "dotted", color = "black") +
  geom_point() + 
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.4) + 
  scale_x_continuous(breaks = seq(-36, 19, by = 3)) + 
  labs(
    x = "Months Since Ban",
    y = "Coefficient - Salary Upper Threshold (Real)"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black")
  )

es_plot_real_salary_to_3

# Save the event study plot 
ggsave(
  "output/figures/es_real_salary_to_3_salary_mn.pdf",
  es_plot_real_salary_to_3,
  width = 7,
  height = 4.5,
  units = "in"
)



# ------------------------------------------------------------------------------
# --- CONDITIONING ON EXPERIENCE REQUIREMENTS ----------------------------------
# ------------------------------------------------------------------------------




