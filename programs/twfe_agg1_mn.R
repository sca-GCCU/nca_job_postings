##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "twfe_agg1_mn.R" 
# by: Sebastian C. Anastasi
# Date of this version: March 25, 2026
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

outcome_var <- c("any_educ_share", "bachelor_share", "master_share", 
                 "doctorate_share", "any_exp_share", "ave_exp", "fulltime_share",
                 "internship_share")



# 1. Load the data and prep the data

agg1_mn_analysis <- read_csv("data/analysis-data/agg1_mn_analysis.csv")

# Transform treatment indicator to numeric for tables
agg1_mn_analysis <- agg1_mn_analysis %>%
  mutate(
    treated_eff_full = as.numeric(treated_eff_full),
    treated_enact_full = as.numeric(treated_enact_full)
  )

# Grab MN ban date
ban_date <- agg1_mn_analysis %>%
  filter(ban_full == 1) %>%
  summarise(date = first(date_eff_full)) %>%
  pull(date)

# Create event time indicator for event studies 
agg1_mn_analysis <- agg1_mn_analysis %>%
  mutate(
    event_time = (year(date) - year(ban_date))*12 + (month(date) - month(ban_date))
  )

# Create truncated version for event studies
et_lb <- -36  # create event time lower bound

es_df <- agg1_mn_analysis %>%
  filter(event_time >= et_lb)


# 2. TWFE estimation

# --- A. Any Education Share ---

# --- Table ---  

# (i) Spec 1: TWFE 
twfe_any_educ_share_1 <- feols(
  any_educ_share ~ treated_eff_full | state + date,
  data = agg1_mn_analysis,
  cluster = ~state
)

summary(twfe_any_educ_share_1)


# (ii) Spec 2: Add Occupation FE
twfe_any_educ_share_2 <- feols(
  any_educ_share ~ treated_eff_full | state + date + soc_4,
  data = agg1_mn_analysis,
  cluster = ~state
)

summary(twfe_any_educ_share_2)

# NOTE: Some singletons are currently being removed, but I think that's just 
# because of the small dataset right now. 


# (iii) Spec 3: Add Demographic Controls 
twfe_any_educ_share_3 <- feols(
  any_educ_share ~ treated_eff_full + # treatment indicator
    frac_male + frac_black + frac_college + mean_age | # demographic controls
    state + date + soc_4, # other FE
  data = agg1_mn_analysis,
  cluster = ~state
)

summary(twfe_any_educ_share_3)

# NOTE: Currently dropping all of my controls because of collinearity.


# (iv) Spec 4: Add Economic Controls 
twfe_any_educ_share_4 <- feols(
  any_educ_share ~ treated_eff_full + # treatment indicator
    frac_male + frac_black + frac_college + mean_age + # demographic controls
    unemp_rate + real_income + real_hpi | # economic controls 
    state + date + soc_4, # other FE
  data = agg1_mn_analysis,
  cluster = ~state
)

summary(twfe_any_educ_share_4)

# NOTE: Currently dropping all of my controls because of collinearity.


# Combine into table 
etable(
  twfe_any_educ_share_1, twfe_any_educ_share_2, twfe_any_educ_share_3,
  twfe_any_educ_share_4, 
  tex = TRUE,
  tpt = TRUE,
  dict = c(
    any_educ_share = "Any Education (Share)",
    treated_eff_full = "Treated $\\times$ Post",
    state = "State",
    date = "Month",
    soc_4 = "Occupation"
  ),
  style.tex = style.tex("aer"), # Removes weird "Model" thing and cleans up headings
  notes = c(
    "@\\footnotesize",
    "*Notes:* Standard errors are clustered at the state level.",
    "*** p$<$0.01, ** p$<$0.05, * p$<$0.1"
  ),
  file = "output/tables/table_twfe_any_share.tex"
)

# NOTE: MAKE OTHER TABLES MATCH THIS ONE.





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
    y = "Coefficient - Any Education (Share)"
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
  "output/figures/es_any_educ_share_1_agg1_mn.pdf",
  es_plot_any_educ_share_1,
  width = 7,
  height = 4.5,
  units = "in"
)


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
    y = "Coefficient - Any Education (Share)"
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
  "output/figures/es_any_educ_share_2_agg1_mn.pdf",
  es_plot_any_educ_share_2,
  width = 7,
  height = 4.5,
  units = "in"
)



# (iii) Spec 3: Add Demographic Controls 
# Run event study specification 
es_any_educ_share_3 <- feols(
  any_educ_share ~ i(event_time, ban_full, -1) + 
    frac_male + frac_black + frac_college + mean_age | # demographic controls
    state + date + soc_4,
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
    y = "Coefficient - Any Education (Share)"
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
  "output/figures/es_any_educ_share_3_agg1_mn.pdf",
  es_plot_any_educ_share_3,
  width = 7,
  height = 4.5,
  units = "in"
)






# --- B. Bachelor Share ---
# --- Table ---  

# (i) Spec 1: TWFE 
twfe_bachelor_share_1 <- feols(
  bachelor_share ~ treated_eff_full | state + date,
  data = agg1_mn_analysis,
  cluster = ~state
)

summary(twfe_bachelor_share_1)


# (ii) Spec 2: Add Occupation FE
twfe_bachelor_share_2 <- feols(
  bachelor_share ~ treated_eff_full | state + date + soc_4,
  data = agg1_mn_analysis,
  cluster = ~state
)

summary(twfe_bachelor_share_2)

# NOTE: Some singletons are currently being removed, but I think that's just 
# because of the small dataset right now. 


# (iii) Spec 3: Add Demographic Controls 
twfe_bachelor_share_3 <- feols(
  bachelor_share ~ treated_eff_full + # treatment indicator
    frac_male + frac_black + frac_college + mean_age | # demographic controls
    state + date + soc_4, # other FE
  data = agg1_mn_analysis,
  cluster = ~state
)

summary(twfe_bachelor_share_3)

# NOTE: Currently dropping all of my controls because of collinearity.


# (iv) Spec 4: Add Economic Controls 
twfe_bachelor_share_4 <- feols(
  bachelor_share ~ treated_eff_full + # treatment indicator
    frac_male + frac_black + frac_college + mean_age + # demographic controls
    unemp_rate + real_income + real_hpi | # economic controls 
    state + date + soc_4, # other FE
  data = agg1_mn_analysis,
  cluster = ~state
)

summary(twfe_bachelor_share_4)

# NOTE: Currently dropping all of my controls because of collinearity.


# Combine into table 
etable(twfe_bachelor_share_1, twfe_bachelor_share_2, twfe_bachelor_share_3,
       twfe_bachelor_share_4, tex = TRUE)

# NOTE: NEED TO RELABEL AND CLEAN STUFF UP.




# --- Event Study ---

# (i) Spec 1: TWFE 

# (ii) Spec 2: Add Occupation FE

# (iii) Spec 3: Add Demographic Controls




# --- C. Master Share ---
# --- Table ---  

# (i) Spec 1: TWFE 

# (ii) Spec 2: Add Occupation FE

# (iii) Spec 3: Add Demographic Controls 


# --- Event Study ---

# (i) Spec 1: TWFE 

# (ii) Spec 2: Add Occupation FE

# (iii) Spec 3: Add Demographic Controls




# --- D. Doctorate Share ---
# --- Table ---  

# (i) Spec 1: TWFE 

# (ii) Spec 2: Add Occupation FE

# (iii) Spec 3: Add Demographic Controls 


# --- Event Study ---

# (i) Spec 1: TWFE 

# (ii) Spec 2: Add Occupation FE

# (iii) Spec 3: Add Demographic Controls




# --- E. Any Experience Share ---
# --- Table ---  

# (i) Spec 1: TWFE 

# (ii) Spec 2: Add Occupation FE

# (iii) Spec 3: Add Demographic Controls 


# --- Event Study ---

# (i) Spec 1: TWFE 

# (ii) Spec 2: Add Occupation FE

# (iii) Spec 3: Add Demographic Controls




# --- F. Average Experience ---
# --- Table ---  

# (i) Spec 1: TWFE 

# (ii) Spec 2: Add Occupation FE

# (iii) Spec 3: Add Demographic Controls 


# --- Event Study ---

# (i) Spec 1: TWFE 

# (ii) Spec 2: Add Occupation FE

# (iii) Spec 3: Add Demographic Controls




# --- G. Full-Time Share ---
# --- Table ---  

# (i) Spec 1: TWFE 

# (ii) Spec 2: Add Occupation FE

# (iii) Spec 3: Add Demographic Controls 


# --- Event Study ---

# (i) Spec 1: TWFE 

# (ii) Spec 2: Add Occupation FE

# (iii) Spec 3: Add Demographic Controls




# --- H. Internship Share ---
# --- Table ---  

# (i) Spec 1: TWFE 

# (ii) Spec 2: Add Occupation FE

# (iii) Spec 3: Add Demographic Controls 


# --- Event Study ---

# (i) Spec 1: TWFE 

# (ii) Spec 2: Add Occupation FE

# (iii) Spec 3: Add Demographic Controls
















