cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "nca_acs_pretreat_means_banwise.log", replace 

clear all 
set more off // stops Stata from pausing output every screenful

* LOAD DATA --------------------------------------------------------------------

use nca_acs_soc, clear 
compress // shrinks variable storage types to smallest lossless type 


* OUTPUT DIRECTORIES -----------------------------------------------------------
local root "C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/results_acs_full/twfe"
local tabdir "`root'/tables_twfe"
local sterdir "`root'/estimates_twfe"

cap mkdir "`tabdir'"
cap mkdir "`sterdir'"

local hw_ban_year 2008 2020 2022
local lw_ban_year 2017 2018 2019 2020 2021

local age_var age early_career mid_career late_career
local age_group early_career mid_career late_career

* Pre-treatment means for HW ban cohorts  
foreach y of local hw_ban_year {
	foreach v of local age_var {
		di "*** pre-treatment mean of `v' for `y' HW ban cohort"
		mean `v' [aw=perwt] ///
			if ((year_eff_ban == `y' & hw == 1) | year_eff_ban == 0)
	}
	foreach a of local age_group {
		di "*** pre-treatment mean real income for age group = `a' for `y' HW ban cohort"
		mean incwage_r [aw=perwt] ///
			if (((year_eff_ban == `y' & hw == 1) | year_eff_ban == 0) & `a' == 1)
	}
	di "*** pre-treatment mean real income for `y' HW ban cohort (unstratified)"
	mean incwage_r [aw=perwt] ///
			if ((year_eff_ban == `y' & hw == 1) | year_eff_ban == 0)
}

* Pre-treatment means for LW ban cohorts  

foreach y of local lw_ban_year {
	foreach v of local age_var {
		di "*** pre-treatment mean of `v' for `y' LW ban cohort"
		mean `v' [aw=perwt] ///
			if ((year_eff_ban == `y' & hw == 0) | year_eff_ban == 0)
	}
	foreach a of local age_group {
		di "*** pre-treatment mean real income for age group = `a' for `y' LW ban cohort"
		mean incwage_r [aw=perwt] ///
			if (((year_eff_ban == `y' & hw == 0) | year_eff_ban == 0) & `a' == 1)
	}
	di "*** pre-treatment mean real income for `y' LW ban cohort (unstratified)"
	mean incwage_r [aw=perwt] ///
			if ((year_eff_ban == `y' & hw == 0) | year_eff_ban == 0)
}




log close 