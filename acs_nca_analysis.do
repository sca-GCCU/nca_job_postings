cd "C:\Users\scana\OneDrive\Documents\nca_job_postings\data"

log using "acs_nca_analysis.log", replace 



* MERGE ACS AND NONCOMPETE BAN DATA 
clear all 

	* Create panel of treatment 
use "state_year.dta", clear 
merge m:1 statefip using "nca_laws_gks.dta"
drop _merge 

	* Generating treatment variables
gen treated_eff = (year >= year_eff_ban) if ban == 1
gen treated_enact = (year >= year_enact_ban) if ban == 1
label variable treated_eff "Date-effective treatment indicator"
label variable treated_enact "Date-enacted treatment indicator"

	* Save new panel dataset 
save nca_laws_panel, replace 
use nca_laws_panel, clear 


	* Merge nca laws dataset with ACS dataset 
use "acs_2001-23.dta", clear 
merge m:1 statefip year using "nca_laws_panel.dta"

drop _merge

	* Drop full ban states and variables 
	
drop if full_ban == 1 
drop full_ban year_full_ban month_full_ban

	* Modify the treatment indicators so that missing values are 0. 
replace treated_eff = 0 if missing(treated_eff)
replace treated_enact = 0 if missing(treated_enact)

	* Save new panel dataset 
save "nca_acs_no_controls.dta", replace 
use "nca_acs_no_controls.dta", clear 



* MERGE ACS AND NONCOMPETE BAN DATA (SOC code version)
clear all 

use "acs_2001-23_v2.dta", clear 
merge m:1 statefip year using "nca_laws_panel.dta"

drop _merge 

drop if full_ban == 1
drop full_ban year_full_ban month_full_ban

replace treated_eff = 0 if missing(treated_eff)
replace treated_enact = 0 if missing(treated_enact)

save "nca_acs_soc_no_controls.dta", replace 
use "nca_acs_soc_no_controls.dta", clear 

	* Might be irrelevant since I can't seem to match this up with the OES
	* occupation codes properly. Could try merging again, and only use the 
	* successful matches. 
		
		
		
* PREP COVARIATES

	* BLS employment 
clear all

//import data 
drop if statefip == 51000 //Dropping NYC 
drop if state == "Los Angeles County"

collapse (mean) employment_nsa employment_sa, by (statefip year)

drop if year < 2001 | year > 2023

save "bls_emp.dta", replace 

	* BEA per capita personal income 
clear all 

//import data 
local oldnames C D E F G H I J K L M N O P Q R S T U V W X Y
di "`oldnames'"

local stubnames
forvalues i = 2001(1)2023 {
	local stubnames `stubnames' income_pcap`i'
}
di "`stubnames'"

rename (`oldnames') (`stubnames')

reshape long income_pcap, i(statefip state) j(year)

replace statefip = statefip/1000

save "bea_inc.dta", replace 

	* FHFA Housing Price Index (HPI)
clear all 

//import data 
local oldnames v1 v2 v3 v4
local newnames state_abb year quarter hpi 
rename (`oldnames') (`newnames')

save "fhfa_hpi1.dta", replace 

clear all 

//import data 
save "abb_to_fip.dta", replace 

use "fhfa_hpi1.dta", clear
merge m:1 state_abb using "abb_to_fip.dta"

drop _merge 

collapse (mean) hpi, by (statefip year)

drop if year < 2001 | year > 2023

save "fhfa_hpi2.dta", replace 



* MERGE COVARIATES WITH NON-SOC ACS DATA
clear all 

use "nca_acs_no_controls.dta", clear 

merge m:1 statefip year using "bls_emp.dta"

drop if _merge == 2 //dropping full-ban values  
drop _merge 

merge m:1 statefip year using "bea_inc.dta"

drop if _merge == 2 //dropping full-ban values  
drop _merge 

merge m:1 statefip year using "fhfa_hpi2.dta"

drop if _merge == 2 //dropping full-ban values  
drop _merge 

save "nca_acs.dta", replace 


	
* CREATE SUM STATS
clear all 

use nca_acs_p2, clear 






log close 