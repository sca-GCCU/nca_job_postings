cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "nca_acs_twfe_1pct.log", replace 

clear all 

* LOAD DATA --------------------------------------------------------------------

use nca_acs_soc_1pct, clear 


* TWFE ESTIMATES ---------------------------------------------------------------

* NOTE: Ask Drukker about whether I should use fw for how many obs are in each
* cross-section or if I should use perwt. Also whether I should use aweights or
* fweights.

* (I) HW 2008 ------------------------------------------------------------------

* (1) Age Estimates

local age_vars age early_career mid_career late_career 

foreach v in `age_vars' {
	didregress (`v') (treated_eff) ///
		if (year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban) [aweight=perwt], ///
		group(statefip) time(year) vce(cluster statefip)
}

* (2) Wage Estimates 

* (a) All Age Groups

didregress (incwage_real) (treated_eff) ///
	if (year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban) [aweight=perwt], ///
	group(statefip) time(year) vce(cluster statefip)
	
* NOTE: Equivalent results from the below reghdfe. Use this approach for
* consistency with ES results? hdidregress and didregress let you do more stuff.
reghdfe incwage_real treated_eff ///
	if (year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban) [aweight=perwt], ///
	absorb(statefip year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "***Real income results for `a'***"
	didregress (incwage_real) (treated_eff) ///
	if ((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) & `a' == 1 ///
	[aweight=perwt], group(statefip) time(year) vce(cluster statefip)
}


* (II) HW 2020 -----------------------------------------------------------------

* (1) Age Estimates

local age_vars age early_career mid_career late_career 

foreach v in `age_vars' {
	didregress (`v') (treated_eff) ///
		if (year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban) [aweight=perwt], ///
		group(statefip) time(year) vce(cluster statefip)
}

* (2) Wage Estimates 

* (a) All Age Groups

didregress (incwage_real) (treated_eff) ///
	if (year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban) [aweight=perwt], ///
	group(statefip) time(year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "***Real income results for `a'***"
	didregress (incwage_real) (treated_eff) ///
	if ((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) & `a' == 1 ///
	[aweight=perwt], group(statefip) time(year) vce(cluster statefip)
}


* (III) HW 2022 ----------------------------------------------------------------

* (1) Age Estimates 

local age_vars age early_career mid_career late_career 

foreach v in `age_vars' {
	didregress (`v') (treated_eff) ///
		if (year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban) [aweight=perwt], ///
		group(statefip) time(year) vce(cluster statefip)
}

* (2) Wage Estimates 

* (a) All Age Groups

didregress (incwage_real) (treated_eff) ///
	if (year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban) [aweight=perwt], ///
	group(statefip) time(year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "***Real income results for `a'***"
	didregress (incwage_real) (treated_eff) ///
	if ((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) & `a' == 1 ///
	[aweight=perwt], group(statefip) time(year) vce(cluster statefip)
}


* (IV) LW 2017 -----------------------------------------------------------------

* (1) Age Estimates 

local age_vars age early_career mid_career late_career 

foreach v in `age_vars' {
	didregress (`v') (treated_eff) ///
		if (year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban) [aweight=perwt], ///
		group(statefip) time(year) vce(cluster statefip)
}

* (2) Wage Estimates 

* (a) All Age Groups

didregress (incwage_real) (treated_eff) ///
	if (year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban) [aweight=perwt], ///
	group(statefip) time(year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "***Real income results for `a'***"
	didregress (incwage_real) (treated_eff) ///
	if ((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) & `a' == 1 ///
	[aweight=perwt], group(statefip) time(year) vce(cluster statefip)
}



* (V) LW 2018 ------------------------------------------------------------------

* (1) Age Estimates 

local age_vars age early_career mid_career late_career 

foreach v in `age_vars' {
	didregress (`v') (treated_eff) ///
		if (year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban) [aweight=perwt], ///
		group(statefip) time(year) vce(cluster statefip)
}

* (2) Wage Estimates 

* (a) All Age Groups

didregress (incwage_real) (treated_eff) ///
	if (year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban) [aweight=perwt], ///
	group(statefip) time(year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "***Real income results for `a'***"
	didregress (incwage_real) (treated_eff) ///
	if ((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) & `a' == 1 ///
	[aweight=perwt], group(statefip) time(year) vce(cluster statefip)
}



* (VI) LW 2019 -----------------------------------------------------------------

* (1) Age Estimates 

local age_vars age early_career mid_career late_career 

foreach v in `age_vars' {
	didregress (`v') (treated_eff) ///
		if (year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban) [aweight=perwt], ///
		group(statefip) time(year) vce(cluster statefip)
}

* (2) Wage Estimates 

* (a) All Age Groups

didregress (incwage_real) (treated_eff) ///
	if (year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban) [aweight=perwt], ///
	group(statefip) time(year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "***Real income results for `a'***"
	didregress (incwage_real) (treated_eff) ///
	if ((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) & `a' == 1 ///
	[aweight=perwt], group(statefip) time(year) vce(cluster statefip)
}



* (VII) LW 2020 ----------------------------------------------------------------

* (1) Age Estimates 

local age_vars age early_career mid_career late_career 

foreach v in `age_vars' {
	didregress (`v') (treated_eff) ///
		if (year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban) [aweight=perwt], ///
		group(statefip) time(year) vce(cluster statefip)
}

* (2) Wage Estimates 

* (a) All Age Groups

didregress (incwage_real) (treated_eff) ///
	if (year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban) [aweight=perwt], ///
	group(statefip) time(year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "***Real income results for `a'***"
	didregress (incwage_real) (treated_eff) ///
	if ((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) & `a' == 1 ///
	[aweight=perwt], group(statefip) time(year) vce(cluster statefip)
}



* (VIII) LW 2021 ---------------------------------------------------------------

* (1) Age Estimates 

local age_vars age early_career mid_career late_career 

foreach v in `age_vars' {
	didregress (`v') (treated_eff) ///
		if (year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban) [aweight=perwt], ///
		group(statefip) time(year) vce(cluster statefip)
}

* (2) Wage Estimates 

* (a) All Age Groups

didregress (incwage_real) (treated_eff) ///
	if (year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban) [aweight=perwt], ///
	group(statefip) time(year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "***Real income results for `a'***"
	didregress (incwage_real) (treated_eff) ///
	if ((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) & `a' == 1 ///
	[aweight=perwt], group(statefip) time(year) vce(cluster statefip)
}





log close 