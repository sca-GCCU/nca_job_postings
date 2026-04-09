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

# NOTE 1: Error when trying to install tidyverse. It needed ragg, which wasn't 
# installed,and may not be able to be installed on the Cluster... So, I'm just  
# going to install and load individual packages that I need.

# NOTE 2: It takes a while for packages to install on RStudio, so don't re-run
# this entire script. IN FUTURE, selectively install the necessary packages.  

# NOTE 3: It's fine to leave that "repos" argument. It just says from where to 
# download packages. It specifically routes you to the nearest CRAN server.

packages <- c("lubridate", "fixest", "stargazer", "kableExtra", 
              "scales", "readxl", "haven")

installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

if(length(to_install) > 0){
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

