cd "C:\Users\scana\OneDrive\Documents\nca_job_postings\data"

log using "nca_acs_analysis.log", replace 

* NAIVE TWFE ESTIMATOR 

	* Repeated cross sectional: so use didregress.
	* Weight by perwt, which is a frequency weight. 

use "nca_acs.dta", clear 

local depvar age young_adult older_adult no_high_school high_school ///
	some_college college

foreach v in `depvar' {
	display "Estimating for outcome: `v'"
	didregress (`v') (treated_eff) [fw=perwt], group(statefip) time(year)
}
	* The above was throwing error messages.

didregress (age) (treated_eff), group(statefip) time(year) // wrong sign 

didregress (college) (treated_eff), group(statefip) time(year) // small

didregress (college occ) (treated_eff), group(statefip) time(year) // smaller 

	* Try for subset of states

gen treated_2019 = year_eff_ban == 2019 
gen control = ever_treated == 0
keep if treated_2019 | control

didreg (age) (treated_eff), group(statefip) time(year) // positive, large
	
didreg (college) (treated_eff), group(statefip) time(year) // positive, large  

didreg (young_adult) (treated_eff), group(statefip) time(year)

didreg (older_adult) (treated_eff), group(statefip) time(year)

didreg (no_high_school) (treated_eff), group(statefip) time(year)

didreg (high_school) (treated_eff), group(statefip) time(year)

didreg (some_college) (treated_eff), group(statefip) time(year)

	* Reset data 

use "nca_acs.dta", clear 

use "nca_laws_gks.dta", clear 

tab state if ban == 1 

	* I think "ban" can work as my ever treated variable I created. 


	

log close 