##############################################################################
# Project Title: "Noncompete Bans and Early-Career Workers"
# Project Collaborators: Sebastian C. Anastasi and Vitor Melo 
#
# R Script: "0c_paths.R" 
# by: Sebastian C. Anastasi
# Date of this version: April 6, 2026
#
# Description: Defines all project directories in one place.
#
# Dependencies:
#
# Output: 
##############################################################################

# 1. Identify home directory (robust to use on Cluster)
home <- path.expand("~")

# 2. Define project root
proj_root <- file.path(home, "nca_job_postings")


# 3. Data folders
data_dir        <- file.path(proj_root, "data")
data_raw        <- file.path(data_dir, "raw-data")
data_clean      <- file.path(data_dir, "clean-data")
data_analysis   <- file.path(data_dir, "analysis-data")

# 4. Output folders
output_dir      <- file.path(proj_root, "output")
output_figures  <- file.path(output_dir, "figures")
output_tables   <- file.path(output_dir, "tables")
output_other    <- file.path(output_dir, "other")

# 5. Logs
log_dir         <- file.path(proj_root, "logs")

# 6. Code
programs_dir    <- file.path(proj_root, "programs")

# 7. Create missing folders automatically
dir.create(data_analysis, showWarnings = FALSE, recursive = TRUE)
dir.create(data_clean,    showWarnings = FALSE, recursive = TRUE)

dir.create(output_dir,     showWarnings = FALSE, recursive = TRUE)
dir.create(output_figures, showWarnings = FALSE, recursive = TRUE)
dir.create(output_tables,  showWarnings = FALSE, recursive = TRUE)
dir.create(output_other,   showWarnings = FALSE, recursive = TRUE)

dir.create(log_dir,        showWarnings = FALSE, recursive = TRUE)

