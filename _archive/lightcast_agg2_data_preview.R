rm(list = ls())

setwd("C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/data")

library(data.table)
peek <- fread("anastasi_data_2010_2025_agg2.csv", nrows = 10)
head(peek)
