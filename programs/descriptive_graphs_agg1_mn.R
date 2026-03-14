##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "descriptive_graphs_agg1_mn" 
# by: Sebastian C. Anastasi
# Date of this version: March 6, 2026
#
# Description: Creates occupation-state-month level descriptive.
#
# Dependencies: 
#
# Output: 
##############################################################################

# NOTE: When run in the cluster, I will need to update paths and (I believe)
# install the appropriate packages. 

rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")

library(tidyverse)
library(lubridate)
library(scales)

# 1. Load analysis data 
agg1_mn_analysis <- read_csv("data/analysis-data/agg1_mn_analysis.csv")


# 2. Create plots of raw means by treatment status 

# --- Total Postings ---
# Create means - ANNUAL  
tot_ann <- agg1_mn_analysis %>%
  group_by(ban_full, state, year) %>% # collapse by month and find yearly postings
  summarise(
    yearly_total_postings = sum(total_postings, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ban_full, year) %>% # collapse by state and find mean yearly postings
  summarise(
    mean_yearly_total_postings = mean(yearly_total_postings, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(
    year != 2025
  )

years_vec <- seq(from = 2010, to = 2024, by = 1) # TRY MONTHS LATER

p_amean_tot_post <- ggplot(tot_ann, aes(x = year, 
                                             y = mean_yearly_total_postings, 
                                             color = group, 
                                             group = group)) + 
  geom_line(size = 1.2) +
  labs(
    x = "Year",
    y = "Total Postings - Annual",
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = years_vec) +
  scale_y_continuous(
    breaks = seq(0, max(tot_ann$mean_yearly_total_postings, na.rm = TRUE), by = 1000)
  ) +
  scale_color_grey() + 
  theme_minimal() + 
  theme(
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_tot_post

ggsave(
  "output/figures/plot_amean_tot_post_agg1_mn.pdf",
  p_amean_tot_post,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  tot_ann,
  p_amean_tot_post
)



# Create means - MONTHLY 
tot_mon <- agg1_mn_analysis %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_total_postings = mean(total_postings, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  )

treat_date_value <- make_date(2023, 7, 1) # save MN ban date 

date_breaks_vec <- seq(
  from = min(tot_mon$date),
  to = max(tot_mon$date),
  by = "6 months"
)

p_mmean_tot_post <- ggplot(
  tot_mon,
  aes(x = date, y = mean_total_postings, color = group, group = group)
) + 
  geom_line(size = 1.2) +
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Total Postings - Monthly",
    color = ""
  ) +
  scale_color_grey() +
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  scale_y_continuous(
    breaks = seq(0, max(tot_mon$mean_total_postings, na.rm = TRUE), by = 100)
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) 

p_mmean_tot_post

ggsave(
  "output/figures/plot_mmean_tot_post_agg1_mn.pdf",
  p_mmean_tot_post,
  width = 7,
  height = 4,
  units = "in"
)

rm(
  tot_mon,
  p_mmean_tot_post
)



# --- Any Education ---  
# Count - ANNUAL 
any_ed_ann <- agg1_mn_analysis %>% 
  group_by(ban_full, state, year) %>% # collapse month, preserve states
  summarise(
    yearly_any_ed = sum(any_educ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ban_full, year) %>% 
  summarise(
    mean_yearly_any_ed = mean(yearly_any_ed, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(year != 2025)
  

p_amean_any_ed <- ggplot(any_ed_ann, aes(x = year, 
                                        y = mean_yearly_any_ed, 
                                        color = group, 
                                        group = group)) + 
  geom_line(size = 1.2) +
  labs(
    x = "Year",
    y = "Any Education Requirement - Annual",
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = years_vec) +
  scale_y_continuous(
    breaks = seq(0, max(any_ed_ann$mean_yearly_any_ed, na.rm = TRUE), by = 1000)
  ) +
  scale_color_grey() + 
  theme_minimal() + 
  theme(
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_any_ed

ggsave(
  "output/figures/plot_amean_any_ed_agg1_mn.pdf",
  p_amean_any_ed,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  any_ed_ann,
  p_amean_any_ed
)



# Count - MONTHLY 
any_ed_mon <- agg1_mn_analysis %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_any_educ = mean(any_educ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_any_ed <- ggplot(
  any_ed_mon,
  aes(x = date, y = mean_any_educ, color = group, group = group)
) + 
  geom_line(size = 1.2) +
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Any Education Requirement - Monthly",
    color = ""
  ) +
  scale_color_grey() +
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  scale_y_continuous(
    breaks = seq(0, max(any_ed_mon$mean_any_educ, na.rm = TRUE), by = 50)
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) 

p_mmean_any_ed

ggsave(
  "output/figures/plot_mmean_any_ed_agg1_mn.pdf",
  p_mmean_any_ed,
  width = 7,
  height = 4,
  units = "in"
)

rm(
  any_ed_mon,
  p_mmean_any_ed
)


# Share - ANNUAL 

share_any_ed_ann <- agg1_mn_analysis %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_any_educ_share_yr = mean(any_educ_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  )

p_amean_any_ed_share <- ggplot(share_any_ed_ann, aes(x = year, 
                                         y = mean_any_educ_share_yr, 
                                         color = group, 
                                         group = group)) + 
  geom_line(size = 1.2) +
  labs(
    x = "Year",
    y = "Any Education Requirement (Share) - Annual",
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = years_vec) +
  #scale_y_continuous(
  #  breaks = seq(0, max(share_any_ed_ann$mean_any_educ_share_yr, na.rm = TRUE), by = 1000)
  #) +
  scale_color_grey() + 
  theme_minimal() + 
  theme(
    legend.position = c(0.1, 0.1),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_any_ed_share

ggsave(
  "output/figures/plot_amean_any_ed_share_agg1_mn.pdf",
  p_amean_any_ed_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_any_ed_ann,
  p_amean_any_ed_share
)


# Share - MONTHLY 

share_any_ed_mon <- agg1_mn_analysis %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_any_educ_share = mean(any_educ_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_any_ed_share <- ggplot(
  share_any_ed_mon,
  aes(x = date, y = mean_any_educ_share, color = group, group = group)
) + 
geom_line(size = 1.2) +
geom_vline(
  xintercept = treat_date_value,
  linetype = "dashed",
  color = "black"
) +
labs(
  x = "Year-Month",
  y = "Any Education Requirement (Share) - Monthly",
  color = ""
) +
scale_color_grey() +
scale_x_date(
  breaks = date_breaks_vec,
  labels = label_date("%Y-%m")
) +
#scale_y_continuous(
#  breaks = seq(0, max(share_any_ed_mon$mean_any_educ_share, na.rm = TRUE), by = 50)
#) + 
theme_minimal() + 
theme(
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
  legend.position = c(0.1, 0.1),
  legend.background = element_blank(),
  panel.grid = element_blank(),
  axis.line = element_line(color = "black")
) 

p_mmean_any_ed_share

ggsave(
  "output/figures/plot_mmean_any_ed_agg1_mn.pdf",
  p_mmean_any_ed_share,
  width = 7,
  height = 4,
  units = "in"
)

rm(
  share_any_ed_mon,
  p_mmean_any_ed_share
)


# ------------------- RESUME HERE ----------------------------------------------


# Bachelor's 
# Count
# Share



# Master's
# Count
# Share



# Doctorate 
# Count
# Share



# Any Experience 
# Count
# Share



# Average Experience



# Full Time
# Count
# Share



# Part Time 
# Count
# Share



# Internship
# Count
# Share



# Average Salary 






