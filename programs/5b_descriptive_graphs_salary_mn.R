##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "descriptive_graphs_salary_mn" 
# by: Sebastian C. Anastasi
# Date of this version: April 6, 2026
#
# Description: Creates postings-level descriptive graphs of salary offers.
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
library(lubridate)
library(scales)

# Load data 
salary_df <- read_csv(file.path(data_analysis, "salary_mn_analysis.csv"))


# --- Define Vectors for Plotting --- 
years_vec <- seq(from = 2010, to = 2024, by = 1) # TRY MONTHS LATER

treat_date_value <- make_date(2023, 7, 1) # save MN ban date 

date_breaks_vec <- seq(
  from = min(salary_df$date),
  to = max(salary_df$date),
  by = "6 months"
)


# NOTE: Key variables real_salary, real_salary_from, real_salary_to, pay period (?)

# --- ANNUAL - UNCONDITIONAL ---
# Create aggregation dataframe 
salary_agg <- salary_df %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_real_salary = mean(real_salary, na.rm = TRUE),
    mean_real_salary_from = mean(real_salary_from, na.rm = TRUE),
    mean_real_salary_to = mean(real_salary_to, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(year != 2025)

p_amean_rsalary <- ggplot(
  salary_agg,
  aes(
    x = year,
    y = mean_real_salary,
    color = group
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Real Salary - Annual",
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = years_vec) +
  scale_y_continuous(
    labels = comma
  ) +
  scale_color_grey() + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

p_amean_rsalary

ggsave(
  file.path(output_figures, "plot_amean_rsalary_mn.pdf"),
  p_amean_rsalary,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  salary_agg,
  p_amean_rsalary
)


# --- ANNUAL - EXP ---  
salary_agg_exp <- salary_df %>%
  filter(any_exp == 1) %>% # has experience requirement
  group_by(ban_full, year) %>%
  summarise(
    mean_real_salary = mean(real_salary, na.rm = TRUE),
    mean_real_salary_from = mean(real_salary_from, na.rm = TRUE),
    mean_real_salary_to = mean(real_salary_to, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(year != 2025)

p_amean_rsalary_exp <- ggplot(
  salary_agg_exp,
  aes(
    x = year,
    y = mean_real_salary,
    color = group
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Real Salary (Experience) - Annual",
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = years_vec) +
  scale_y_continuous(
    labels = comma
  ) +
  scale_color_grey() + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

p_amean_rsalary_exp

ggsave(
  file.path(output_figures, "plot_amean_rsalary_exp_mn.pdf"),
  p_amean_rsalary_exp,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  salary_agg_exp,
  p_amean_rsalary_exp
)



# --- ANNUAL - NO EXP ---  
salary_agg_noexp <- salary_df %>%
  filter(any_exp == 0) %>% # has experience requirement
  group_by(ban_full, year) %>%
  summarise(
    mean_real_salary = mean(real_salary, na.rm = TRUE),
    mean_real_salary_from = mean(real_salary_from, na.rm = TRUE),
    mean_real_salary_to = mean(real_salary_to, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(year != 2025)

p_amean_rsalary_noexp <- ggplot(
  salary_agg_noexp,
  aes(
    x = year,
    y = mean_real_salary,
    color = group
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Real Salary (No Experience) - Annual",
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = years_vec) +
  scale_y_continuous(
    labels = comma
  ) +
  scale_color_grey() + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  )

p_amean_rsalary_noexp

ggsave(
  file.path(output_figures, "plot_amean_rsalary_noexp_mn.pdf"),
  p_amean_rsalary_noexp,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  salary_agg_noexp,
  p_amean_rsalary_noexp
)






# For dropping Feb 2025, since we didn't have the full month. 
date_cut_off <- make_date(2025, 2, 1)

# --- MONTHLY - UNCONDITIONAL --- 
# Create aggregation dataframe 
salary_magg <- salary_df %>%
  filter(date != date_cut_off) %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_real_salary = mean(real_salary, na.rm = TRUE),
    mean_real_salary_from = mean(real_salary_from, na.rm = TRUE),
    mean_real_salary_to = mean(real_salary_to, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  ) 

p_mmean_rsalary <- ggplot(
  salary_magg,
  aes(x = date, y = mean_real_salary, color = group, group = group)
) + 
  geom_line(linewidth = 0.9) +
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Real Salary - Monthly",
    color = ""
  ) +
  scale_color_grey() +
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  scale_y_continuous(
    labels = comma
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) 

p_mmean_rsalary

ggsave(
  file.path(output_figures, "plot_mmean_rsalary_mn.pdf"),
  p_mmean_rsalary,
  width = 11,
  height = 5,
  units = "in"
)

rm(
  salary_magg,
  p_mmean_rsalary
)

# --- MONTHLY - EXP --- 
# Create aggregation dataframe 
salary_magg_exp <- salary_df %>%
  filter(date != date_cut_off) %>%
  filter(any_exp == 1) %>% # has an experience requirement
  group_by(ban_full, date) %>%
  summarise(
    mean_real_salary = mean(real_salary, na.rm = TRUE),
    mean_real_salary_from = mean(real_salary_from, na.rm = TRUE),
    mean_real_salary_to = mean(real_salary_to, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  ) 

p_mmean_rsalary_exp <- ggplot(
  salary_magg_exp,
  aes(x = date, y = mean_real_salary, color = group, group = group)
) + 
  geom_line(linewidth = 0.9) +
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Real Salary (Experience) - Monthly",
    color = ""
  ) +
  scale_color_grey() +
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  scale_y_continuous(
    labels = comma
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) 

p_mmean_rsalary_exp

ggsave(
  file.path(output_figures, "plot_mmean_rsalary_exp_mn.pdf"),
  p_mmean_rsalary_exp,
  width = 11,
  height = 5,
  units = "in"
)

rm(
  salary_magg_exp,
  p_mmean_rsalary_exp
)


# --- MONTHLY - NO EXP --- 
# Create aggregation dataframe 
salary_magg_noexp <- salary_df %>%
  filter(date != date_cut_off) %>%
  filter(any_exp == 0) %>% # has an experience requirement
  group_by(ban_full, date) %>%
  summarise(
    mean_real_salary = mean(real_salary, na.rm = TRUE),
    mean_real_salary_from = mean(real_salary_from, na.rm = TRUE),
    mean_real_salary_to = mean(real_salary_to, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  ) 

p_mmean_rsalary_noexp <- ggplot(
  salary_magg_noexp,
  aes(x = date, y = mean_real_salary, color = group, group = group)
) + 
  geom_line(linewidth = 0.9) +
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Real Salary (No Experience) - Monthly",
    color = ""
  ) +
  scale_color_grey() +
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  scale_y_continuous(
    labels = comma
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) 

p_mmean_rsalary_noexp

ggsave(
  file.path(output_figures, "plot_mmean_rsalary_noexp_mn.pdf"),
  p_mmean_rsalary_noexp,
  width = 11,
  height = 5,
  units = "in"
)

rm(
  salary_magg_noexp,
  p_mmean_rsalary_noexp
)

