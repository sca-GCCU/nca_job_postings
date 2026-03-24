##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "descriptive_graphs_agg1_mn" 
# by: Sebastian C. Anastasi
# Date of this version: March 16, 2026
#
# Description: Creates occupation-state-month level descriptive graphs.
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
# --- Define Vectors for Plotting --- 
years_vec <- seq(from = 2010, to = 2024, by = 1) # TRY MONTHS LATER

treat_date_value <- make_date(2023, 7, 1) # save MN ban date 

date_breaks_vec <- seq(
  from = min(agg1_mn_analysis$date),
  to = max(agg1_mn_analysis$date),
  by = "6 months"
)


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

p_amean_tot_post <- ggplot(tot_ann, aes(x = year, 
                                             y = mean_yearly_total_postings, 
                                             color = group, 
                                             group = group)) + 
  geom_line(linewidth = 1.2) +
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
  group_by(ban_full, state, date) %>%
  summarise(
    sum_total_postings = sum(total_postings), # add up totals within state
    .groups = "drop"
  ) %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_total_postings = mean(sum_total_postings, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_tot_post <- ggplot(
  tot_mon,
  aes(x = date, y = mean_total_postings, color = group, group = group)
) + 
  geom_line(linewidth = 1.2) +
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
    breaks = seq(0, max(tot_mon$mean_total_postings, na.rm = TRUE), by = 500)
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
  height = 4.5,
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
  geom_line(linewidth = 1.2) +
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
  group_by(ban_full, state, date) %>%
  summarise(
    sum_any_educ = sum(any_educ),
    .groups = "drop"
  ) %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_any_educ = mean(sum_any_educ, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_any_ed <- ggplot(
  any_ed_mon,
  aes(x = date, y = mean_any_educ, color = group, group = group)
) + 
  geom_line(linewidth = 1.2) +
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
    breaks = seq(0, max(any_ed_mon$mean_any_educ, na.rm = TRUE), by = 500)
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
  height = 4.5,
  units = "in"
)

rm(
  any_ed_mon,
  p_mmean_any_ed
)


# Share (Any Ed) - ANNUAL 
share_any_ed_ann <- agg1_mn_analysis %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_any_educ_share_yr = mean(any_educ_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(year != 2025)

p_amean_any_ed_share <- ggplot(share_any_ed_ann, aes(x = year, 
                                         y = mean_any_educ_share_yr, 
                                         color = group, 
                                         group = group)) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Any Education Requirement (Share) - Annual",
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = years_vec) +
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


# Share (Any Ed) - MONTHLY 
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
geom_line(linewidth = 1.2) +
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
  "output/figures/plot_mmean_any_ed_share_agg1_mn.pdf",
  p_mmean_any_ed_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_any_ed_mon,
  p_mmean_any_ed_share
)



# --- Bachelor's --- 
# Count (Bachelor's) - ANNUAL
bachelors_ann <- agg1_mn_analysis %>%
  group_by(ban_full, state, year) %>%
  summarise(
    bachelor = sum(bachelor),
    .groups = "drop"
  ) %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_bachelor = mean(bachelor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States") # if_else is dplyr version of ifelse
  ) %>%
  filter(year != 2025)

p_amean_bachelor <- ggplot(
  bachelors_ann,
  aes(
    x = year, 
    y = mean_bachelor, 
    color = group
  )
) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Bachelor's Requirement - Annual",
    color = ""
  ) + 
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") + 
  scale_x_continuous(breaks = years_vec) + 
  scale_y_continuous(breaks = seq(0, max(bachelors_ann$mean_bachelor), by = 1000)) + 
  scale_color_grey() + 
  theme_minimal() + # has to be called before other theme() adjustments 
  theme(
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_bachelor

ggsave(
  "output/figures/plot_amean_bachelor_agg1_mn.pdf",
  p_amean_bachelor,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  bachelors_ann,
  p_amean_bachelor
)


# Count (Bachelor's) - MONTHLY 
bachelors_mon <- agg1_mn_analysis %>%
  group_by(ban_full, state, date) %>%
  summarise(
    sum_bachelor = sum(bachelor),
    .groups = "drop"
  ) %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_bachelor = mean(sum_bachelor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_bachelor <- ggplot(
  bachelors_mon, 
  aes(
    x = date,
    y = mean_bachelor,
    colour = group
  )
) +
  geom_line(linewidth = 1.2) + 
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Bachelor's Requirement - Monthly ",
    color = ""
  ) + 
  scale_color_grey() + 
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  scale_y_continuous(
    breaks = seq(0, max(bachelors_mon$mean_bachelor), by = 100)
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_mmean_bachelor

ggsave(
  "output/figures/plot_mmean_bachelor_agg1_mn.pdf",
  p_mmean_bachelor,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  bachelors_mon,
  p_mmean_bachelor
)


# Share (Bachelor's) - ANNUAL 
share_bachelors_ann <- agg1_mn_analysis %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_bachelor_share_yr = mean(bachelor_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(year != 2025)

p_amean_bachelor_share <- ggplot(share_bachelors_ann, aes(x = year, 
                                                     y = mean_bachelor_share_yr, 
                                                     color = group, 
                                                     group = group)) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Bachelor's Requirement (Share) - Annual",
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = years_vec) +
  scale_color_grey() + 
  theme_minimal() + 
  theme(
    legend.position = c(0.1, 0.1),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_bachelor_share

ggsave(
  "output/figures/plot_amean_bachelor_share_agg1_mn.pdf",
  p_amean_bachelor_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_bachelors_ann,
  p_amean_bachelor_share
)


# Share (Bachelor's) - MONTHLY 
share_bachelor_mon <- agg1_mn_analysis %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_bachelor_share = mean(bachelor_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_bachelor_share <- ggplot(
  share_bachelor_mon,
  aes(x = date, y = mean_bachelor_share, color = group, group = group)
) + 
  geom_line(linewidth = 1.2) +
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Bachelor's Requirement (Share) - Monthly",
    color = ""
  ) +
  scale_color_grey() +
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.1, 0.1),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) 

p_mmean_bachelor_share

ggsave(
  "output/figures/plot_mmean_bachelor_share_agg1_mn.pdf",
  p_mmean_bachelor_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_bachelor_mon,
  p_mmean_bachelor_share
)



# --- Master's --- 
# Count (Master's) - ANNUAL
master_ann <- agg1_mn_analysis %>%
  group_by(ban_full, state, year) %>%
  summarise(
    master = sum(master),
    .groups = "drop"
  ) %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_master = mean(master, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States") # if_else is dplyr version of ifelse
  ) %>%
  filter(year != 2025)

p_amean_master <- ggplot(
  master_ann,
  aes(
    x = year, 
    y = mean_master, 
    color = group
  )
) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Master's Requirement - Annual",
    color = ""
  ) + 
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") + 
  scale_x_continuous(breaks = years_vec) + 
  scale_y_continuous(breaks = seq(0, max(master_ann$mean_master), by = 50)) + 
  scale_color_grey() + 
  theme_minimal() + # has to be called before other theme() adjustments 
  theme(
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_master

ggsave(
  "output/figures/plot_amean_master_agg1_mn.pdf",
  p_amean_master,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  master_ann,
  p_amean_master
)


# Count (Master's) - MONTHLY 
master_mon <- agg1_mn_analysis %>%
  group_by(ban_full, state, date) %>%
  summarise(
    master = sum(master),
    .groups = "drop"
  ) %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_master = mean(master, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_master <- ggplot(
  master_mon, 
  aes(
    x = date,
    y = mean_master,
    colour = group
  )
) +
  geom_line(linewidth = 1.2) + 
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Master's Requirement - Monthly ",
    color = ""
  ) + 
  scale_color_grey() + 
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  scale_y_continuous(
    breaks = seq(0, max(master_mon$mean_master), by = 25)
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_mmean_master

ggsave(
  "output/figures/plot_mmean_master_agg1_mn.pdf",
  p_mmean_master,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  master_mon,
  p_mmean_master
)


# Share (Master's) - ANNUAL 
share_master_ann <- agg1_mn_analysis %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_master_share_yr = mean(master_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(year != 2025)

p_amean_master_share <- ggplot(share_master_ann, aes(x = year, 
                                                          y = mean_master_share_yr, 
                                                          color = group, 
                                                          group = group)) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Master's Requirement (Share) - Annual",
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = years_vec) +
  scale_color_grey() + 
  theme_minimal() + 
  theme(
    legend.position = c(0.1, 0.1),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_master_share

ggsave(
  "output/figures/plot_amean_master_share_agg1_mn.pdf",
  p_amean_master_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_master_ann,
  p_amean_master_share
)


# Share (Master's) - MONTHLY 
share_master_mon <- agg1_mn_analysis %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_master_share = mean(master_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_master_share <- ggplot(
  share_master_mon,
  aes(x = date, y = mean_master_share, color = group, group = group)
) + 
  geom_line(linewidth = 1.2) +
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Master's Requirement (Share) - Monthly",
    color = ""
  ) +
  scale_color_grey() +
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.1, 0.9),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) 

p_mmean_master_share

ggsave(
  "output/figures/plot_mmean_master_share_agg1_mn.pdf",
  p_mmean_master_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_master_mon,
  p_mmean_master_share
)





# --- Doctorate --- 
# Count (Doctorate) - ANNUAL
doctorate_ann <- agg1_mn_analysis %>%
  group_by(ban_full, state, year) %>%
  summarise(
    doctorate = sum(doctorate),
    .groups = "drop"
  ) %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_doctorate = mean(doctorate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States") # if_else is dplyr version of ifelse
  ) %>%
  filter(year != 2025)

p_amean_doctorate <- ggplot(
  doctorate_ann,
  aes(
    x = year, 
    y = mean_doctorate, 
    color = group
  )
) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Doctorate Requirement - Annual",
    color = ""
  ) + 
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") + 
  scale_x_continuous(breaks = years_vec) + 
  scale_y_continuous(breaks = seq(0, max(doctorate_ann$mean_doctorate), by = 50)) + 
  scale_color_grey() + 
  theme_minimal() + # has to be called before other theme() adjustments 
  theme(
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_doctorate

ggsave(
  "output/figures/plot_amean_doctorate_agg1_mn.pdf",
  p_amean_doctorate,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  doctorate_ann,
  p_amean_doctorate
)


# Count (Doctorate) - MONTHLY 
doctorate_mon <- agg1_mn_analysis %>%
  group_by(ban_full, state, date) %>%
  summarise(
    doctorate = sum(doctorate),
    .groups = "drop"
  ) %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_doctorate = mean(doctorate, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_doctorate <- ggplot(
  doctorate_mon, 
  aes(
    x = date,
    y = mean_doctorate,
    colour = group
  )
) +
  geom_line(linewidth = 1.2) + 
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Doctorate Requirement - Monthly ",
    color = ""
  ) + 
  scale_color_grey() + 
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  scale_y_continuous(
    breaks = seq(0, max(doctorate_mon$mean_doctorate), by = 25)
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_mmean_doctorate

ggsave(
  "output/figures/plot_mmean_doctorate_agg1_mn.pdf",
  p_mmean_doctorate,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  doctorate_mon,
  p_mmean_doctorate
)


# Share (Doctorate) - ANNUAL 
share_doctorate_ann <- agg1_mn_analysis %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_doctorate_share_yr = mean(doctorate_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(year != 2025)

p_amean_doctorate_share <- ggplot(share_doctorate_ann, aes(x = year, 
                                                     y = mean_doctorate_share_yr, 
                                                     color = group, 
                                                     group = group)) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Doctorate Requirement (Share) - Annual",
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = years_vec) +
  scale_color_grey() + 
  theme_minimal() + 
  theme(
    legend.position = c(0.1, 0.1),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_doctorate_share

ggsave(
  "output/figures/plot_amean_doctorate_share_agg1_mn.pdf",
  p_amean_doctorate_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_doctorate_ann,
  p_amean_doctorate_share
)


# Share (Doctorate) - MONTHLY 
share_doctorate_mon <- agg1_mn_analysis %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_doctorate_share = mean(doctorate_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_doctorate_share <- ggplot(
  share_doctorate_mon,
  aes(x = date, y = mean_doctorate_share, color = group, group = group)
) + 
  geom_line(linewidth = 1.2) +
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Doctorate Requirement (Share) - Monthly",
    color = ""
  ) +
  scale_color_grey() +
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.1, 0.9),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) 

p_mmean_doctorate_share

ggsave(
  "output/figures/plot_mmean_doctorate_share_agg1_mn.pdf",
  p_mmean_doctorate_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_doctorate_mon,
  p_mmean_doctorate_share
)




# --- Any Experience --- 
# Count (Any Exp) - ANNUAL
any_exp_ann <- agg1_mn_analysis %>%
  group_by(ban_full, state, year) %>%
  summarise(
    any_exp = sum(any_exp),
    .groups = "drop"
  ) %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_any_exp = mean(any_exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States") # if_else is dplyr version of ifelse
  ) %>%
  filter(year != 2025)

p_amean_any_exp <- ggplot(
  any_exp_ann,
  aes(
    x = year, 
    y = mean_any_exp, 
    color = group
  )
) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Any Experience Requirement - Annual",
    color = ""
  ) + 
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") + 
  scale_x_continuous(breaks = years_vec) + 
  scale_y_continuous(breaks = seq(0, max(any_exp_ann$mean_any_exp), by = 500)) + 
  scale_color_grey() + 
  theme_minimal() + # has to be called before other theme() adjustments 
  theme(
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_any_exp

ggsave(
  "output/figures/plot_amean_any_exp_agg1_mn.pdf",
  p_amean_any_exp,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  any_exp_ann,
  p_amean_any_exp
)


# Count (Any Exp) - MONTHLY 
any_exp_mon <- agg1_mn_analysis %>%
  group_by(ban_full, state, date) %>%
  summarise(
    any_exp = sum(any_exp),
    .groups = "drop"
  ) %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_any_exp = mean(any_exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_any_exp <- ggplot(
  any_exp_mon, 
  aes(
    x = date,
    y = mean_any_exp,
    colour = group
  )
) +
  geom_line(linewidth = 1.2) + 
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Any Experience Requirement - Monthly ",
    color = ""
  ) + 
  scale_color_grey() + 
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  scale_y_continuous(
    breaks = seq(0, max(any_exp_mon$mean_any_exp), by = 200)
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_mmean_any_exp

ggsave(
  "output/figures/plot_mmean_any_exp_agg1_mn.pdf",
  p_mmean_any_exp,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  any_exp_mon,
  p_mmean_any_exp
)


# Share (Any Exp) - ANNUAL 
share_any_exp_ann <- agg1_mn_analysis %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_any_exp_share_yr = mean(any_exp_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(year != 2025)

p_amean_any_exp_share <- ggplot(share_any_exp_ann, aes(x = year, 
                                                           y = mean_any_exp_share_yr, 
                                                           color = group, 
                                                           group = group)) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Any Experience Requirement (Share) - Annual",
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = years_vec) +
  scale_color_grey() + 
  theme_minimal() + 
  theme(
    legend.position = c(0.1, 0.1),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_any_exp_share

ggsave(
  "output/figures/plot_amean_any_exp_share_agg1_mn.pdf",
  p_amean_any_exp_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_any_exp_ann,
  p_amean_any_exp_share
)

# Share (Any Exp) - MONTHLY 
share_any_exp_mon <- agg1_mn_analysis %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_any_exp_share = mean(any_exp_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_any_exp_share <- ggplot(
  share_any_exp_mon,
  aes(x = date, y = mean_any_exp_share, color = group, group = group)
) + 
  geom_line(linewidth = 1.2) +
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Any Experience Requirement (Share) - Monthly",
    color = ""
  ) +
  scale_color_grey() +
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.1, 0.1),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) 

p_mmean_any_exp_share

ggsave(
  "output/figures/plot_mmean_any_exp_share_agg1_mn.pdf",
  p_mmean_any_exp_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_any_exp_mon,
  p_mmean_any_exp_share
)


# --- Average Experience --- 
# (Ave Exp) ANNUAL 
ave_exp_ann <- agg1_mn_analysis %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_ave_exp_yr = mean(ave_exp, na.rm = TRUE), # conditional on any requirement
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(year != 2025)

p_amean_ave_exp <- ggplot(
  ave_exp_ann,
  aes(
    x = year, 
    y = mean_ave_exp_yr, 
    color = group
  )
) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Average Experience Requirement - Annual",
    color = ""
  ) + 
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") + 
  scale_x_continuous(breaks = years_vec) + 
  scale_color_grey() + 
  theme_minimal() + # has to be called before other theme() adjustments 
  theme(
    legend.position = c(0.1, 0.1),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_ave_exp

ggsave(
  "output/figures/plot_amean_ave_exp_agg1_mn.pdf",
  p_amean_ave_exp,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  ave_exp_ann,
  p_amean_ave_exp
)

# (Ave Exp) MONTHLY 
ave_exp_mon <- agg1_mn_analysis %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_ave_exp = mean(ave_exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_ave_exp <- ggplot(
    ave_exp_mon,
    aes(x = date, y = mean_ave_exp, color = group, group = group)
  ) + 
  geom_line(linewidth = 1.2) +
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Average Experience Requirement - Monthly",
    color = ""
  ) +
  scale_color_grey() +
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.1, 0.1),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) 

p_mmean_ave_exp

ggsave(
  "output/figures/plot_mmean_ave_exp_agg1_mn.pdf",
  p_mmean_ave_exp,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  ave_exp_mon,
  p_mmean_ave_exp
)



# --- Full Time --- 
# Count (FT) - ANNUAL
ft_ann <- agg1_mn_analysis %>%
  group_by(ban_full, state, year) %>%
  summarise(
    fulltime = sum(fulltime),
    .groups = "drop"
  ) %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_ft = mean(fulltime, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States") # if_else is dplyr version of ifelse
  ) %>%
  filter(year != 2025)

p_amean_ft <- ggplot(
  ft_ann,
  aes(
    x = year, 
    y = mean_ft, 
    color = group
  )
) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Full-Time Postings - Annual",
    color = ""
  ) + 
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") + 
  scale_x_continuous(breaks = years_vec) + 
  scale_y_continuous(breaks = seq(0, max(ft_ann$mean_ft), by = 1000)) + 
  scale_color_grey() + 
  theme_minimal() + # has to be called before other theme() adjustments 
  theme(
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_ft

ggsave(
  "output/figures/plot_amean_ft_agg1_mn.pdf",
  p_amean_ft,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  ft_ann,
  p_amean_ft
)


# Count (FT) - MONTHLY 
ft_mon <- agg1_mn_analysis %>%
  group_by(ban_full, state, date) %>%
  summarise(
    fulltime = sum(fulltime),
    .groups = "drop"
  ) %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_ft = mean(fulltime, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_ft <- ggplot(
  ft_mon, 
  aes(
    x = date,
    y = mean_ft,
    colour = group
  )
) +
  geom_line(linewidth = 1.2) + 
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Full-Time Postings - Monthly ",
    color = ""
  ) + 
  scale_color_grey() + 
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  scale_y_continuous(
    breaks = seq(0, max(ft_mon$mean_ft), by = 200)
  ) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_mmean_ft

ggsave(
  "output/figures/plot_mmean_ft_agg1_mn.pdf",
  p_mmean_ft,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  ft_mon,
  p_mmean_ft
)


# Share (FT) - ANNUAL 
share_ft_ann <- agg1_mn_analysis %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_ft_share_yr = mean(fulltime_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(year != 2025)

p_amean_ft_share <- ggplot(share_ft_ann, aes(x = year, 
                                                       y = mean_ft_share_yr, 
                                                       color = group, 
                                                       group = group)) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Full-Time Postings (Share) - Annual",
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = years_vec) +
  scale_color_grey() + 
  theme_minimal() + 
  theme(
    legend.position = c(0.1, 0.1),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_ft_share

ggsave(
  "output/figures/plot_amean_ft_share_agg1_mn.pdf",
  p_amean_ft_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_ft_ann,
  p_amean_ft_share
)


# Share (FT) - MONTHLY 
share_ft_mon <- agg1_mn_analysis %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_ft_share = mean(fulltime_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_ft_share <- ggplot(
  share_ft_mon,
  aes(x = date, y = mean_ft_share, color = group, group = group)
) + 
  geom_line(linewidth = 1.2) +
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Full-Time Postings (Share) - Monthly",
    color = ""
  ) +
  scale_color_grey() +
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.1, 0.1),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) 

p_mmean_ft_share

ggsave(
  "output/figures/plot_mmean_ft_share_agg1_mn.pdf",
  p_mmean_ft_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_ft_mon,
  p_mmean_ft_share
)


# NOTE: COULD DO PART-TIME LISTINGS AS WELL, BUT I'M SKIPPING THEM FOR NOW.

# --- Part Time ---  
# Count (PT) - ANNUAL

# Count (PT) - MONTHLY 

# Share (PT) - ANNUAL 

# Share (PT) - MONTHLY 



# --- Internship --- 
# Count (Intern) - ANNUAL
intern_ann <- agg1_mn_analysis %>%
  group_by(ban_full, state, year) %>%
  summarise(
    internship = sum(internship),
    .groups = "drop"
  ) %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_intern = mean(internship, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States") # if_else is dplyr version of ifelse
  ) %>%
  filter(year != 2025)

p_amean_intern <- ggplot(
  intern_ann,
  aes(
    x = year, 
    y = mean_intern, 
    color = group
  )
) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Internship Postings - Annual",
    color = ""
  ) + 
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") + 
  scale_x_continuous(breaks = years_vec) + 
  scale_color_grey() + 
  theme_minimal() + # has to be called before other theme() adjustments 
  theme(
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_intern

ggsave(
  "output/figures/plot_amean_intern_agg1_mn.pdf",
  p_amean_intern,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  intern_ann,
  p_amean_intern
)


# Count (Intern) - MONTHLY 
intern_mon <- agg1_mn_analysis %>%
  group_by(ban_full, state, date) %>%
  summarise(
    internship = sum(internship),
    .groups = "drop"
  ) %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_intern = mean(internship, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = if_else(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_intern <- ggplot(
  intern_mon, 
  aes(
    x = date,
    y = mean_intern,
    colour = group
  )
) +
  geom_line(linewidth = 1.2) + 
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Internship Postings - Monthly ",
    color = ""
  ) + 
  scale_color_grey() + 
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.15, 0.95),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_mmean_intern

ggsave(
  "output/figures/plot_mmean_intern_agg1_mn.pdf",
  p_mmean_intern,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  intern_mon,
  p_mmean_intern
)


# Share (Intern) - ANNUAL 
share_intern_ann <- agg1_mn_analysis %>%
  group_by(ban_full, year) %>%
  summarise(
    mean_intern_share_yr = mean(internship_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(year != 2025)

p_amean_intern_share <- ggplot(share_intern_ann, aes(x = year, 
                                             y = mean_intern_share_yr, 
                                             color = group, 
                                             group = group)) + 
  geom_line(linewidth = 1.2) +
  labs(
    x = "Year",
    y = "Internship Postings (Share) - Annual",
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") +
  scale_x_continuous(breaks = years_vec) +
  scale_color_grey() + 
  theme_minimal() + 
  theme(
    legend.position = c(0.1, 0.9),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  )

p_amean_intern_share

ggsave(
  "output/figures/plot_amean_intern_share_agg1_mn.pdf",
  p_amean_intern_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_intern_ann,
  p_amean_intern_share
)


# Share (Intern) - MONTHLY 
share_intern_mon <- agg1_mn_analysis %>%
  group_by(ban_full, date) %>%
  summarise(
    mean_intern_share = mean(internship_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  )

p_mmean_intern_share <- ggplot(
  share_intern_mon,
  aes(x = date, y = mean_intern_share, color = group, group = group)
) + 
  geom_line(linewidth = 1.2) +
  geom_vline(
    xintercept = treat_date_value,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    x = "Year-Month",
    y = "Internship Postings (Share) - Monthly",
    color = ""
  ) +
  scale_color_grey() +
  scale_x_date(
    breaks = date_breaks_vec,
    labels = label_date("%Y-%m")
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    legend.position = c(0.1, 0.9),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black")
  ) 

p_mmean_intern_share

ggsave(
  "output/figures/plot_mmean_intern_share_agg1_mn.pdf",
  p_mmean_intern_share,
  width = 7,
  height = 4.5,
  units = "in"
)

rm(
  share_intern_mon,
  p_mmean_intern_share
)










