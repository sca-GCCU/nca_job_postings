cd "C:\Users\scana\OneDrive\Documents\nca_job_postings\data"

log using "acs_nca_analysis.log", replace 

* MERGE ACS AND NONCOMPETE BAN DATA 
clear all 

use "state_year.dta", clear 
merge m:1 statefip using "nca_laws_gks.dta"
drop _merge 

	* Generating treatment variables
gen treated_eff = (year >= year_eff_ban) if ban == 1
gen treated_enact = (year >= year_enact_ban) if ban == 1

	* Drop full-ban states 
drop if full_ban == 1

	* Save new panel dataset 
save nca_laws_panel, replace 
use nca_laws_panel, clear 

	* Merge nca laws dataset with ACS dataset 
use "acs_2001-23.dta", clear 
merge m:1 statefip year using "nca_laws_panel.dta"

	* Check for what is missing: Ends up being the full-ban states.
tabulate statefip if _merge == 1, label

	* Drop full-ban observations
drop if _merge == 1
drop _merge 

	* Modify the treatment indicators so that missing values are 0. 
replace treated_eff = 0 if missing(treated_eff)
replace treated_enact = 0 if missing(treated_enact)

	* Save new panel dataset 
save nca_acs_panel, replace 
use nca_acs_panel, clear 


* MERGE ACS AND NONCOMPETE BAN DATA (SOC code version)
clear all 

use acs_2001-23_v2, clear 
merge m:1 statefip year using "nca_laws_panel.dta"
tabulate statefip if _merge == 1, label 
drop if _merge == 1
drop _merge 

replace treated_eff = 0 if missing(treated_eff)
replace treated_enact = 0 if missing(treated_enact)

save nca_acs_p2, replace 
use nca_acs_p2, clear 

	
*************************
* (PROBLEMS) MERGE COVARIATES

use covariates, clear 
drop v1
drop edu*
replace unemployment = unemployment/100
collapse (mean) unemployment white black hispanic poverty hpi_nsa hpi_sa, by(state year)
save covariates_acs, replace
use covariates_acs, clear 

	* Problems I've noticed with this data: First, it only includes 46 states
		* and does not include the District of Columbia. Second, it only
		* goes from 2010 to 2014. 
	* Get the covariates yourself. Just need the three from Clemens et al. 
*************************

	
* CREATE SUM STATS
clear all 

use nca_acs_p2, clear 






log close 