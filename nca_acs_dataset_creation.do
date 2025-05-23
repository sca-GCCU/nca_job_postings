cd "C:\Users\scana\OneDrive\Documents\nca_job_postings\data"

log using "nca_acs_dataset.log", replace 



* MERGE ACS AND NONCOMPETE BAN DATA 
clear all 

	* Create panel of treatment 
use "state_year.dta", clear 
merge m:1 statefip using "nca_laws_gks.dta"
drop _merge 

	* Generating treatment variables
gen treated_eff = (year >= year_eff_ban) if ban == 1
gen treated_enact = (year >= year_enact_ban) if ban == 1

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

	* Relabel some variables 
label variable ban "treatment group indicator"
label variable year_enact_ban "year ban enacted"
label variable month_enact_ban "month ban enacted"
label variable year_eff_ban "year ban effective"
label variable month_eff_ban "month ban effective"
label variable hw_ban "high-wage ban indicator"
label variable ban_coverage "coverage description"
label variable multi_leg "multiple legislation indicator"
label variable multi_leg_year "year of additional legislation"
label variable treated_eff "date-effective treatment indicator"
label variable treated_enact "date-enacted treatment indicator"

	* Save new panel dataset 
save "nca_acs_no_controls.dta", replace 



* MERGE ACS AND NONCOMPETE BAN DATA (SOC code version)
clear all 

use "acs_2001-23_v2.dta", clear 
merge m:1 statefip year using "nca_laws_panel.dta"

drop _merge 

drop if full_ban == 1
drop full_ban year_full_ban month_full_ban

replace treated_eff = 0 if missing(treated_eff)
replace treated_enact = 0 if missing(treated_enact)

label variable ban "treatment group indicator" 
	// Add labels for values of ban at some point. 
label variable year_enact_ban "year ban enacted"
label variable month_enact_ban "month ban enacted"
label variable year_eff_ban "year ban effective"
label variable month_eff_ban "month ban effective"
label variable hw_ban "high-wage ban indicator"
label variable ban_coverage "coverage description"
label variable multi_leg "multiple legislation indicator"
label variable multi_leg_year "year of additional legislation"
label variable treated_eff "date-effective treatment indicator"
label variable treated_enact "date-enacted treatment indicator"

save "nca_acs_soc_no_controls.dta", replace 

	* Might be irrelevant since I can't seem to match this up with the OES
	* occupation codes properly. Could try merging again, and only use the 
	* successful matches. 
		
		
		
* PREP COVARIATES

	* (1) BLS employment 
clear all

//import data 
drop if statefip == 51000 //Dropping NYC 
drop if state == "Los Angeles County"

collapse (mean) employment_nsa employment_sa, by (statefip year)

drop if year < 2001 | year > 2023

save "bls_emp.dta", replace 

	* (2) BEA per capita personal income 
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

	* (3) FHFA Housing Price Index (HPI)
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

	* Label covariates
label variable employment_nsa "not-seasonally-adjusted employment rate"
label variable employment_sa "seasonally-adjusted employment rate"
label variable income_pcap "income per-capita"
label variable hpi "housing price index"

save "nca_acs.dta", replace 



* DROP ANYONE WHO IS NOT EMPLOYED

keep if empstat == 1

save "nca_acs.dta", replace 
	
	
	
* CREATING SOME RELEVANT VARIABLES
clear all 

use "nca_acs.dta", clear 

	* Create high-school/non-college aged adult indicator 
gen young_adult = (age >= 16 & age <= 21)
label variable young_adult "non-college aged indicator"

	* Create early-career aged adult indicator 
gen earlyc_adult = (age > 21 & age <= 34)
label variable earlyc_adult "early-career aged adult indicator"

	* Create mid-late-career aged adult indicator 
gen mlc_adult = (age > 34 & age < 50)
label variable mlc_adult "mid- to late-career aged adult indicator"

	* Create near-retirement adult indicator 
gen older_adult = (age >= 50 & age <= 64)
label variable older_adult "near-retirement aged adult indicator"

	* Create no high school degree indicator 
gen no_high_school = inrange(educd, 0, 61)
label variable no_high_school "no high school indicator"

	* Create high school degree indicator
gen high_school = inrange(educd, 62, 64)
label variable high_school "high school degree indicator"

	* Create some college indicator 
gen some_college = inrange(educd, 65, 100)
label variable some_college "some college indicator" 

	* Create a bachelor's degree or higher indicator 
gen college = inrange(educd, 101, 116)
label variable college "bachelor's degree or higher indicator"

	* Create an indicator specifically for black
gen black = (race == 2)
label variable black "black indicator"

save "nca_acs.dta", replace 




log close 