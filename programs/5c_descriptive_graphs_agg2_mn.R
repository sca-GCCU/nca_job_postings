##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "descriptive_graphs_agg2_mn.R" 
# by: Sebastian C. Anastasi
# Date of this version: July 17, 2026
#
# Description: Creates occupation-state-month level descriptive graphs.
#
# Dependencies: 
#
# Output: 
##############################################################################

# NOTE: Not really using this for descriptive graphs at the moment. Using 5a's
# descriptive graphs instead. 
  
rm(list = ls())
gc()

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings")
#setwd("/home/scanast/nca_job_postings") # for cluster run

# --- Load packages ---
library(dplyr)
library(ggplot2)
library(readr)
library(lubridate)
library(scales)
library(stringr)

# Load the data 
agg2 <- read_csv("data/analysis-data/agg2_mn_analysis.csv")

# ---- Create 2-Digit NAICS for Plotting ---- 

agg2 <- agg2 %>%
  mutate(
    naics2 = str_sub(as.character(naics4), 1,2)
  ) %>%
  relocate(naics2, .after = naics4_name)


# ---- Total Number of Listings ---- 
# NOTES:
# (1) Group by treatment status, industry, state, year
# (2) Group by treatment status, industry, year (collapse state)
# (3) Facet plot by treatment status 

tot_ann <- agg2 %>%
  group_by(ban_full, naics2, state, year) %>% # collapse month and find yearly postings
  summarise(
    yearly_total_postings = sum(total_postings, na.rm = TRUE), # sum-up postings within each cell
    .groups = "drop"
  ) %>%
  group_by(ban_full, naics2, year) %>% # collapse state and find mean STATE-level yearly postings by treatment status
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
                                        color = naics2,
                                        group = naics2)) + 
  geom_line(linewidth = 0.9) + 
  geom_point(size = 2) + 
  labs(
    x = "Year",
    y = "Mean Annual Total Postings",
    color = ""
  ) + 
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) + 
  facet_grid(group ~ .)

p_amean_tot_post


# ---- Listing with Any Experience Requirement ----
any_exp_ann <- agg2 %>%
  group_by(ban_full, naics2, state, year) %>%
  summarise(
    yearly_any_exp_postings = sum(any_exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(ban_full, naics2, year) %>%
  summarise(
    mean_yearly_any_exp_postings = mean(yearly_any_exp_postings, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(
    year != 2025
  )

p_amean_any_exp <- ggplot(any_exp_ann, aes(x = year,
                                           y = mean_yearly_any_exp_postings,
                                           color = naics2,
                                           group = naics2)) + 
  geom_line(linewidth = 0.9) + 
  geom_point(size = 2) + 
  labs(
    x = "Year",
    y = "Mean Annual Any-Experience Postings",
    color = ""
  ) + 
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) + 
  facet_grid(group ~ .)

p_amean_any_exp


# ---- Fraction with Any Experience Requirement ----
# NOTE: This is the share variable defined at the firm-by-occ cell.
share_exp_ann <- agg2 %>%
  group_by(ban_full, naics2, year) %>%
  summarise(
    yearly_share_exp = mean(any_exp_share, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(
    year != 2025
  )

p_amean_share_exp <- ggplot(share_exp_ann, aes(x = year,
                                               y = yearly_share_exp, 
                                               color = naics2, 
                                               group = naics2)) + 
  geom_line(linewidth = 0.9) + 
  geom_point(size = 2) + 
  labs(
    x = "Year", 
    y = "Mean Annual Experience Share",
    color = ""
  ) + 
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) + 
  facet_grid(group ~ .)

p_amean_share_exp

# ---- Average Experience Requirement ----
ave_exp_ann <- agg2 %>%
  group_by(ban_full, naics2, year) %>%
  summarise(
    yearly_ave_exp = mean(ave_exp, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    group = ifelse(ban_full == 1, "Minnesota", "Control States")
  ) %>%
  filter(
    year != 2025
  )

p_amean_ave_exp <- ggplot(ave_exp_ann, aes(x = year,
                                           y = yearly_ave_exp,
                                           color = naics2,
                                           group = naics2)) +
  geom_line(linewidth = 0.9) + 
  geom_point(size = 2) + 
  labs(
    x = "Year",
    y = "Mean Average Experience", 
    color = ""
  ) +
  geom_vline(xintercept = 2023, linetype = "dashed", color = "black") + 
  theme_minimal() + 
  theme(
    legend.position = "bottom",
    legend.background = element_blank(),
    panel.grid = element_blank(),
    axis.line =  element_line(color = "black"),
    axis.ticks = element_line(color = "black")
  ) + 
  facet_grid(group ~ .)

p_amean_ave_exp





