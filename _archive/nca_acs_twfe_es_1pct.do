cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "nca_acs_twfe_es_1pct.log", replace 

clear all 


* HW 2008 - Data Prep ----------------------------------------------------------

use nca_acs_soc_1pct, clear 

* keep treated cohort + never treated
keep if (year_eff_ban == 2008 & hw_ban == 1) | year_eff_ban == 0

* event time: controls in baseline -1
gen time_to_treat = year - year_eff_ban
replace time_to_treat = -1 if year_eff_ban == 0

* shift and pick base
summ time_to_treat
local tmin = r(min)
gen shifted_ttt = time_to_treat - `tmin'
summ shifted_ttt if time_to_treat == -1
local true_neg1 = r(mean)

* --- Output folder setup ---
local outdir "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\results_acs_1pct\twfe\figures_twfe"
cap mkdir "`outdir'"


* HW 2008 | AGE OUTCOMES -------------------------------------------------------

*outcomes
local age_vars age early_career mid_career late_career

* --- Spec 1: Unconditional --- 

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2008 - Unconditional: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2008_spec1_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 2: + Occupation FE ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2008 - Occupation FE: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2008_spec2_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 3: + Occupation FE + Lagged Controls ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2008 - Occupation FE + Covariates: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2008_spec3_`v'.png", replace width(1600)
		
	restore
}



* HW 2008 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------

* age groups 
local age_groups early_career mid_career late_career

* --- Spec 1: Unconditional --- 

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("HW Ban 2008 - Unconditional: Real Income")
	
* --- Save figure ---
graph export "`outdir'\hw2008_spec1_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2008 - Unconditional: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2008_spec1_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 2: + Occupation FE ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("HW Ban 2008 - Occupation FE: Real Income")
	
* --- Save figure ---
graph export "`outdir'\hw2008_spec2_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2008 - Occupation FE: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2008_spec2_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 3: + Occupation FE + Lagged Controls ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt /// 
	c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("HW Ban 2008 - Occupation FE + Covariates: Real Income")
	
* --- Save figure ---
graph export "`outdir'\hw2008_spec3_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2008 - Occupation FE + Covariates: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2008_spec3_incwage_`a'.png", replace width(1600)
		
	restore
}



********************************************************************************

* HW 2020 - Data Prep ----------------------------------------------------------

use nca_acs_soc_1pct, clear 

* keep treated cohort + never treated
keep if (year_eff_ban == 2020 & hw_ban == 1) | year_eff_ban == 0

* event time: controls in baseline -1
gen time_to_treat = year - year_eff_ban
replace time_to_treat = -1 if year_eff_ban == 0

* shift and pick base
summ time_to_treat
local tmin = r(min)
gen shifted_ttt = time_to_treat - `tmin'
summ shifted_ttt if time_to_treat == -1
local true_neg1 = r(mean)

* --- Output folder setup ---
local outdir "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\results_acs_1pct\twfe\figures_twfe"
cap mkdir "`outdir'"


* HW 2020 | AGE OUTCOMES -------------------------------------------------------

*outcomes
local age_vars age early_career mid_career late_career

* --- Spec 1: Unconditional --- 

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2020 - Unconditional: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2020_spec1_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 2: + Occupation FE ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2020 - Occupation FE: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2020_spec2_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 3: + Occupation FE + Lagged Controls ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2020 - Occupation FE + Covariates: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2020_spec3_`v'.png", replace width(1600)
		
	restore
}



* HW 2020 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------

* age groups 
local age_groups early_career mid_career late_career

* --- Spec 1: Unconditional --- 

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("HW Ban 2020 - Unconditional: Real Income")
	
* --- Save figure ---
graph export "`outdir'\hw2020_spec1_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2020 - Unconditional: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2020_spec1_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 2: + Occupation FE ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("HW Ban 2020 - Occupation FE: Real Income")
	
* --- Save figure ---
graph export "`outdir'\hw2020_spec2_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2020 - Occupation FE: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2020_spec2_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 3: + Occupation FE + Lagged Controls ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt /// 
	c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("HW Ban 2020 - Occupation FE + Covariates: Real Income")
	
* --- Save figure ---
graph export "`outdir'\hw2020_spec3_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2020 - Occupation FE + Covariates: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2020_spec3_incwage_`a'.png", replace width(1600)
		
	restore
}




********************************************************************************

* HW 2022 - Data Prep ----------------------------------------------------------

use nca_acs_soc_1pct, clear 

* keep treated cohort + never treated
keep if (year_eff_ban == 2022 & hw_ban == 1) | year_eff_ban == 0

* event time: controls in baseline -1
gen time_to_treat = year - year_eff_ban
replace time_to_treat = -1 if year_eff_ban == 0

* shift and pick base
summ time_to_treat
local tmin = r(min)
gen shifted_ttt = time_to_treat - `tmin'
summ shifted_ttt if time_to_treat == -1
local true_neg1 = r(mean)

* --- Output folder setup ---
local outdir "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\results_acs_1pct\twfe\figures_twfe"
cap mkdir "`outdir'"


* HW 2022 | AGE OUTCOMES -------------------------------------------------------

*outcomes
local age_vars age early_career mid_career late_career

* --- Spec 1: Unconditional --- 

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2022 - Unconditional: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2022_spec1_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 2: + Occupation FE ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2022 - Occupation FE: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2022_spec2_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 3: + Occupation FE + Lagged Controls ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2022 - Occupation FE + Covariates: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2022_spec3_`v'.png", replace width(1600)
		
	restore
}



* HW 2022 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------

* age groups 
local age_groups early_career mid_career late_career

* --- Spec 1: Unconditional --- 

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("HW Ban 2022 - Unconditional: Real Income")
	
* --- Save figure ---
graph export "`outdir'\hw2022_spec1_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2022 - Unconditional: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2022_spec1_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 2: + Occupation FE ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("HW Ban 2022 - Occupation FE: Real Income")
	
* --- Save figure ---
graph export "`outdir'\hw2022_spec2_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2022 - Occupation FE: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2022_spec2_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 3: + Occupation FE + Lagged Controls ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt /// 
	c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("HW Ban 2022 - Occupation FE + Covariates: Real Income")
	
* --- Save figure ---
graph export "`outdir'\hw2022_spec3_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("HW Ban 2022 - Occupation FE + Covariates: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\hw2022_spec3_incwage_`a'.png", replace width(1600)
		
	restore
}









********************************************************************************
* LW 2017 - Data Prep ----------------------------------------------------------

use nca_acs_soc_1pct, clear 

* keep treated cohort + never treated
keep if (year_eff_ban == 2017 & hw_ban == 0) | year_eff_ban == 0

* event time: controls in baseline -1
gen time_to_treat = year - year_eff_ban
replace time_to_treat = -1 if year_eff_ban == 0

* shift and pick base
summ time_to_treat
local tmin = r(min)
gen shifted_ttt = time_to_treat - `tmin'
summ shifted_ttt if time_to_treat == -1
local true_neg1 = r(mean)

* --- Output folder setup ---
local outdir "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\results_acs_1pct\twfe\figures_twfe"
cap mkdir "`outdir'"


* LW 2017 | AGE OUTCOMES -------------------------------------------------------

*outcomes
local age_vars age early_career mid_career late_career

* --- Spec 1: Unconditional --- 

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2017 - Unconditional: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2017_spec1_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 2: + Occupation FE ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2017 - Occupation FE: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2017_spec2_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 3: + Occupation FE + Lagged Controls ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2017 - Occupation FE + Covariates: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2017_spec3_`v'.png", replace width(1600)
		
	restore
}



* LW 2017 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------

* age groups 
local age_groups early_career mid_career late_career

* --- Spec 1: Unconditional --- 

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2017 - Unconditional: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2017_spec1_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2017 - Unconditional: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2017_spec1_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 2: + Occupation FE ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2017 - Occupation FE: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2017_spec2_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2017 - Occupation FE: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2017_spec2_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 3: + Occupation FE + Lagged Controls ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt /// 
	c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2017 - Occupation FE + Covariates: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2017_spec3_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2017 - Occupation FE + Covariates: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2017_spec3_incwage_`a'.png", replace width(1600)
		
	restore
}



********************************************************************************
* LW 2018 - Data Prep ----------------------------------------------------------

use nca_acs_soc_1pct, clear 

* keep treated cohort + never treated
keep if (year_eff_ban == 2018 & hw_ban == 0) | year_eff_ban == 0

* event time: controls in baseline -1
gen time_to_treat = year - year_eff_ban
replace time_to_treat = -1 if year_eff_ban == 0

* shift and pick base
summ time_to_treat
local tmin = r(min)
gen shifted_ttt = time_to_treat - `tmin'
summ shifted_ttt if time_to_treat == -1
local true_neg1 = r(mean)

* --- Output folder setup ---
local outdir "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\results_acs_1pct\twfe\figures_twfe"
cap mkdir "`outdir'"


* LW 2018 | AGE OUTCOMES -------------------------------------------------------

*outcomes
local age_vars age early_career mid_career late_career

* --- Spec 1: Unconditional --- 

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2018 - Unconditional: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2018_spec1_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 2: + Occupation FE ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2018 - Occupation FE: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2018_spec2_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 3: + Occupation FE + Lagged Controls ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2018 - Occupation FE + Covariates: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2018_spec3_`v'.png", replace width(1600)
		
	restore
}



* LW 2018 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------

* age groups 
local age_groups early_career mid_career late_career

* --- Spec 1: Unconditional --- 

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2018 - Unconditional: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2018_spec1_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2018 - Unconditional: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2018_spec1_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 2: + Occupation FE ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2018 - Occupation FE: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2018_spec2_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2018 - Occupation FE: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2018_spec2_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 3: + Occupation FE + Lagged Controls ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt /// 
	c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2018 - Occupation FE + Covariates: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2018_spec3_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2018 - Occupation FE + Covariates: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2018_spec3_incwage_`a'.png", replace width(1600)
		
	restore
}






********************************************************************************
* LW 2019 - Data Prep ----------------------------------------------------------

use nca_acs_soc_1pct, clear 

* keep treated cohort + never treated
keep if (year_eff_ban == 2019 & hw_ban == 0) | year_eff_ban == 0

* event time: controls in baseline -1
gen time_to_treat = year - year_eff_ban
replace time_to_treat = -1 if year_eff_ban == 0

* shift and pick base
summ time_to_treat
local tmin = r(min)
gen shifted_ttt = time_to_treat - `tmin'
summ shifted_ttt if time_to_treat == -1
local true_neg1 = r(mean)

* --- Output folder setup ---
local outdir "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\results_acs_1pct\twfe\figures_twfe"
cap mkdir "`outdir'"


* LW 2019 | AGE OUTCOMES -------------------------------------------------------

*outcomes
local age_vars age early_career mid_career late_career

* --- Spec 1: Unconditional --- 

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2019 - Unconditional: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2019_spec1_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 2: + Occupation FE ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2019 - Occupation FE: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2019_spec2_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 3: + Occupation FE + Lagged Controls ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2019 - Occupation FE + Covariates: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2019_spec3_`v'.png", replace width(1600)
		
	restore
}



* LW 2019 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------

* age groups 
local age_groups early_career mid_career late_career

* --- Spec 1: Unconditional --- 

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2019 - Unconditional: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2019_spec1_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2019 - Unconditional: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2019_spec1_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 2: + Occupation FE ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2019 - Occupation FE: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2019_spec2_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2019 - Occupation FE: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2019_spec2_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 3: + Occupation FE + Lagged Controls ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt /// 
	c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2019 - Occupation FE + Covariates: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2019_spec3_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2019 - Occupation FE + Covariates: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2019_spec3_incwage_`a'.png", replace width(1600)
		
	restore
}





********************************************************************************
* LW 2020 - Data Prep ----------------------------------------------------------

use nca_acs_soc_1pct, clear 

* keep treated cohort + never treated
keep if (year_eff_ban == 2020 & hw_ban == 0) | year_eff_ban == 0

* event time: controls in baseline -1
gen time_to_treat = year - year_eff_ban
replace time_to_treat = -1 if year_eff_ban == 0

* shift and pick base
summ time_to_treat
local tmin = r(min)
gen shifted_ttt = time_to_treat - `tmin'
summ shifted_ttt if time_to_treat == -1
local true_neg1 = r(mean)

* --- Output folder setup ---
local outdir "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\results_acs_1pct\twfe\figures_twfe"
cap mkdir "`outdir'"


* LW 2020 | AGE OUTCOMES -------------------------------------------------------

*outcomes
local age_vars age early_career mid_career late_career

* --- Spec 1: Unconditional --- 

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2020 - Unconditional: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2020_spec1_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 2: + Occupation FE ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2020 - Occupation FE: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2020_spec2_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 3: + Occupation FE + Lagged Controls ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2020 - Occupation FE + Covariates: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2020_spec3_`v'.png", replace width(1600)
		
	restore
}



* LW 2020 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------

* age groups 
local age_groups early_career mid_career late_career

* --- Spec 1: Unconditional --- 

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2020 - Unconditional: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2020_spec1_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2020 - Unconditional: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2020_spec1_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 2: + Occupation FE ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2020 - Occupation FE: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2020_spec2_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2020 - Occupation FE: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2020_spec2_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 3: + Occupation FE + Lagged Controls ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt /// 
	c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2020 - Occupation FE + Covariates: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2020_spec3_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2020 - Occupation FE + Covariates: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2020_spec3_incwage_`a'.png", replace width(1600)
		
	restore
}







********************************************************************************
* LW 2021 - Data Prep ----------------------------------------------------------

use nca_acs_soc_1pct, clear 

* keep treated cohort + never treated
keep if (year_eff_ban == 2021 & hw_ban == 0) | year_eff_ban == 0

* event time: controls in baseline -1
gen time_to_treat = year - year_eff_ban
replace time_to_treat = -1 if year_eff_ban == 0

* shift and pick base
summ time_to_treat
local tmin = r(min)
gen shifted_ttt = time_to_treat - `tmin'
summ shifted_ttt if time_to_treat == -1
local true_neg1 = r(mean)

* --- Output folder setup ---
local outdir "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\results_acs_1pct\twfe\figures_twfe"
cap mkdir "`outdir'"


* LW 2021 | AGE OUTCOMES -------------------------------------------------------

*outcomes
local age_vars age early_career mid_career late_career

* --- Spec 1: Unconditional --- 

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2021 - Unconditional: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2021_spec1_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 2: + Occupation FE ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2021 - Occupation FE: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2021_spec2_`v'.png", replace width(1600)
		
	restore
}


* --- Spec 3: + Occupation FE + Lagged Controls ---

foreach v of local age_vars {
	* regression
	reghdfe `v' ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2021 - Occupation FE + Covariates: `v'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2021_spec3_`v'.png", replace width(1600)
		
	restore
}



* LW 2021 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------

* age groups 
local age_groups early_career mid_career late_career

* --- Spec 1: Unconditional --- 

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2021 - Unconditional: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2021_spec1_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2021 - Unconditional: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2021_spec1_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 2: + Occupation FE ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2021 - Occupation FE: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2021_spec2_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2021 - Occupation FE: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2021_spec2_incwage_`a'.png", replace width(1600)
		
	restore
}



* --- Spec 3: + Occupation FE + Lagged Controls ---

* All Groups 

* regression
reghdfe incwage_r ib`true_neg1'.shifted_ttt /// 
	c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 [pweight=perwt], ///
	absorb(statefip year occ1990) vce(cluster statefip)

* simultaneous inference targets (no hard-coding)
quietly levelsof shifted_ttt if e(sample), local(et_levels)
local kept_levels : list et_levels - true_neg1
local pnames
foreach k of local kept_levels {
	local pnames `pnames' `k'.shifted_ttt
}
sotable, pnames(`pnames')

* build plotting dataset (fixed add-one-row)
preserve
clear
local K : word count `pnames'
set obs `K'
matrix sim = r(results)'
svmat sim, names(col)
gen shifted_ttt = .
local i = 1
foreach nm of local pnames {
	local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
	replace shifted_ttt = `et' in `i'
	local ++i
}
set obs `= _N + 1'
replace shifted_ttt = `true_neg1' in L
replace b   = 0 in L
capture replace se  = . in L
capture replace low = . in L
capture replace up  = . in L

gen time_to_treat = shifted_ttt + `tmin'
rename b coef
rename se se_adj
rename low ci_bottom
rename up ci_top

twoway ///
	(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
	(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
	xtitle("Event Time") ytitle("Coefficient") ///
	xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
	legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
		   position(6) ring(1) col(1) region(lstyle(none))) ///
	title("LW Ban 2021 - Occupation FE + Covariates: Real Income")
	
* --- Save figure ---
graph export "`outdir'\lw2021_spec3_incwage.png", replace width(1600)
	
restore


* Stratified

foreach a of local age_groups {
	* regression
	reghdfe incwage_r ib`true_neg1'.shifted_ttt ///
		c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if `a' == 1 [pweight=perwt], ///
		absorb(statefip year occ1990) vce(cluster statefip)

	* simultaneous inference targets (no hard-coding)
	quietly levelsof shifted_ttt if e(sample), local(et_levels)
	local kept_levels : list et_levels - true_neg1
	local pnames
	foreach k of local kept_levels {
		local pnames `pnames' `k'.shifted_ttt
	}
	sotable, pnames(`pnames')

	* build plotting dataset (fixed add-one-row)
	preserve
	clear
	local K : word count `pnames'
	set obs `K'
	matrix sim = r(results)'
	svmat sim, names(col)
	gen shifted_ttt = .
	local i = 1
	foreach nm of local pnames {
		local et = real(substr("`nm'", 1, strpos("`nm'", ".") - 1))
		replace shifted_ttt = `et' in `i'
		local ++i
	}
	set obs `= _N + 1'
	replace shifted_ttt = `true_neg1' in L
	replace b   = 0 in L
	capture replace se  = . in L
	capture replace low = . in L
	capture replace up  = . in L

	gen time_to_treat = shifted_ttt + `tmin'
	rename b coef
	rename se se_adj
	rename low ci_bottom
	rename up ci_top

	twoway ///
		(scatter coef time_to_treat, msymbol(O) mcolor(navy)) ///
		(rcap ci_top ci_bottom time_to_treat, lcolor(navy)), ///
		xtitle("Event Time") ytitle("Coefficient") ///
		xline(0, lpattern(dash) lcolor(gs8)) yline(0, lpattern(dash) lcolor(gs8)) ///
		legend(order(1 "Coefficient" 2 "Simultaneous 95% CI") ///
			   position(6) ring(1) col(1) region(lstyle(none))) ///
		title("LW Ban 2021 - Occupation FE + Covariates: Real Income for `a'")
		
	* --- Save figure ---
    graph export "`outdir'\lw2021_spec3_incwage_`a'.png", replace width(1600)
		
	restore
}











log close 
