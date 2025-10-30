cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "nca_acs_twfe_es_1pct.log", replace 

clear all 

* LOAD DATA --------------------------------------------------------------------

use nca_acs_soc_1pct, clear 

* TWFE EVENT STUDY ESTIMATES ---------------------------------------------------

* (1) UNCONDITIONAL 

* create clean dataset; NOTE: I'll have to do this for each can cohort
keep if (year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)

* create leads and lags 
gen time_to_treat = year - year_eff_ban
replace time_to_treat = 0 if year_eff_ban == 0

* determining treat & control
gen treat = (year_eff_ban == 2008 & hw_ban == 1)

* shift to avoid negatives 
summ time_to_treat
g shifted_ttt = time_to_treat - r(min)
summ shifted_ttt if time_to_treat == -1
local true_neg1 = r(mean)

reghdfe incwage_real ib`true_neg1'.shifted_ttt [aweight=perwt], ///
	absorb(statefip year) vce(cluster statefip)

* pull coefficients and SEs
gen coef = . 
gen se = . 

levelsof shifted_ttt, l(times)

foreach t in `times' {
	replace coef = _b[`t'.shifted_ttt] if shifted_ttt == `t'
	replace se = _se[`t'.shifted_ttt] if shifted_ttt == `t'
}

* make CIs
gen ci_top = coef + 1.96*se 
gen ci_bottom = coef - 1.96*se

* switch back to time_to_treat to get original timing 
keep time_to_treat coef se ci_*
duplicates drop

sort time_to_treat

* create the picture 
summ ci_top 
local top_range = r(max)
summ ci_bottom
local bottom_range = r(min)

twoway (sc coef time_to_treat, mcolor(navy) msymbol(O)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)) ///
	(function y = 0, range(time_to_treat) lcolor(gray)) ///
	(function y = 0, range(`bottom_range' `top_range') horiz lcolor(gray) ///
		lpattern(dash)), ///
	legend(off) ytitle("Coefficient") xtitle("Event Time") ///
	title("Earnings - High-Wage Ban (2008)")


***NOTES: 
	* CREATE FOR ALL OTHER OUTCOME VARIABLES
	* NEED TO USE SOTABLE TO ADJUST FOR SIMULTANEOUS INFERENCE 



log close 
