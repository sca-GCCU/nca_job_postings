##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "0b_packages.R" 
# by: Sebastian C. Anastasi
# Date of this version: April 6, 2026
#
# Description: Installs all required packages.
#
# Dependencies:
#
# Output: 
##############################################################################

packages <- c("tidyverse", "lubridate", "fixest", "stargazer", "kableExtra", 
              "scales", "readxl", "haven")

installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

if(length(to_install) > 0){
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

