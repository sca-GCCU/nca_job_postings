cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "nca_acs_dataset.log", replace 

clear all 

* PREP TREATMENT PANEL ---------------------------------------------------------

	* Create panel of treatment 
use "state_year.dta", clear 
merge m:1 statefip using "nca_laws_gks.dta"
drop _merge 

	* Generating treatment variables
gen treated_eff = (year >= year_eff_ban) if ban == 1
gen treated_enact = (year >= year_enact_ban) if ban == 1

	* Modify the treatment indicators so that missing values are 0. 
replace treated_eff = 0 if missing(treated_eff)
replace treated_enact = 0 if missing(treated_enact)

	* Recoding the year_eff and year_enact variables to work with csdid 
replace year_eff_ban = 0 if missing(year_eff_ban) // recode gvar properly
replace year_enact_ban = 0 if missing(year_enact_ban) 

	* Save new panel dataset 
save nca_laws_panel, replace 


* CPI-U DATA -------------------------------------------------------------------

//IMPORT THE DTA: "cpi_u_clean.xlsx"

label variable cpi_annual "annual cpi"

gen cpi_base = .
qui summarize cpi_annual if year == 2020
replace cpi_base = r(mean)
label variable cpi_base "base year cpi"

gen cpi_deflator = cpi_base/cpi_annual
label variable cpi_deflator "cpi deflator"

save "cpi_u_deflator.dta", replace 


* PREP COVARIATES --------------------------------------------------------------

* (1) BLS employment 
clear all

//IMPORT THE DATA: "bls_employment_2025.xlsx"

drop if state == "New York city" 
drop if state == "Los Angeles County"

collapse (mean) employment_sa, by (statefip year) // monthly to yearly data

drop if year < 2001 | year > 2023

* creating lagged variable 
sort statefip year
xtset statefip year

gen employment_sa_l1 = L1.employment_sa

save "bls_emp.dta", replace 


* (2) BEA per capita personal income 
clear all 

	//IMPORT THE DATA: "bea_income_cleaned.xlsx"

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

* transforming to real values 
use "bea_inc.dta", clear
merge m:1 year using "cpi_u_deflator.dta"
keep if _merge == 3
drop _merge 

gen inc_pcap_r = income_pcap * cpi_deflator

* creating lagged variable 
sort statefip year
xtset statefip year

gen inc_pcap_r_l1 = L1.inc_pcap_r

drop cpi_*

save "bea_inc.dta", replace 



* (3) FHFA Housing Price Index (HPI)
clear all 

//IMPORT THE DATA: "hpi_po_state.xlsx"

drop index_nsa Warning

rename index_sa hpi 

save "hpi1.dta", replace 

*local oldnames v1 v2 v3 v4
*local newnames state_abb year quarter hpi 
*rename (`oldnames') (`newnames')

*save "fhfa_hpi1.dta", replace 

clear all 

*NOTE: Created "abb_to_fip.dta" 

use "hpi1.dta", clear
merge m:1 state_abb using "abb_to_fip.dta"

drop _merge 

collapse (mean) hpi, by (statefip year)

drop if year < 2001 | year > 2023

save "hpi2.dta", replace 

* transforming to real values 
use "hpi2.dta", clear 
merge m:1 year using "cpi_u_deflator.dta"
keep if _merge == 3
drop _merge

gen hpi_r = hpi * cpi_deflator

* creating lagged variable 
sort statefip year
xtset statefip year

gen hpi_r_l1 = L1.hpi_r

drop cpi_*

save "hpi2.dta", replace 



* CREATE SOC DATASET -----------------------------------------------------------

* (1) Merge ACS data (that has SOC codes) with Treatment Panel 
clear all 

use "acs_2001-23_v3.dta", clear 
merge m:1 statefip year using "nca_laws_panel.dta"

drop _merge 

drop if full_ban == 1
drop full_ban year_full_ban month_full_ban

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

	
* (2) Merge in Covariates 
clear all 

use "nca_acs_soc_no_controls.dta", clear 

merge m:1 statefip year using "bls_emp.dta"

drop if _merge == 2 //dropping full-ban values 
drop _merge 

merge m:1 statefip year using "bea_inc.dta"

drop if _merge == 2 //dropping full-ban values  
drop _merge 

merge m:1 statefip year using "hpi2.dta"

drop if _merge == 2 //dropping full-ban values  
drop _merge 

* Label covariates
label variable employment_sa "seasonally-adjusted employment rate"
label variable employment_sa_l1 "lagged seasonally-adjusted employment rate"
label variable income_pcap "income per-capita"
label variable inc_pcap_r "real income per-capita"
label variable inc_pcap_r_l1 "lagged real income per-capita"
label variable hpi "housing price index"
label variable hpi_r "real housing price index"
label variable hpi_r_l1 "lagged real housing price index"

save "nca_acs_soc.dta", replace 

* OBSERVATIONS AT THIS POINT: 54,272,701


* RESTRICT TO THOSE EMPLOYED AND WORKING FOR WAGES -----------------------------
* Note: Drop those not employed, self-employed, or reporting an incwage == 0
use "nca_acs_soc.dta", clear 

keep if empstat == 1 // keep employed; drop not employed 
					 // (29,254,534 observations deleted)

keep if classwkr == 2 // keep works for wages; drop self-employed or unkown
					  // (2,569,053 observations deleted)

drop if incwage == 0 // drop anyone who reports incwage == 0 
					 // (6,034 observations deleted)

save "nca_acs_soc.dta", replace 

* OBSERVATIONS AT THIS POINT: 22,443,080 


* CONVERTING NOMINAL WAGES TO REAL USING CPI-U ---------------------------------

clear all 

use "nca_acs_soc.dta", clear 

merge m:1 year using "cpi_u_deflator.dta"

drop if _merge == 2 //dropping years outside of range 
drop _merge 

drop cpi_annual cpi_base //only keep the deflator itself

* Make the actual adjustments 
gen incwage_r = incwage * cpi_deflator
label variable incwage_r "real wage and salary income"

save "nca_acs_soc.dta", replace 


* CREATE RELEVANT OUTCOME VARIABLES -------------------------------------------- 
clear all

use "nca_acs_soc.dta", clear 

* Create Early-, Mid-, and Late-Career indicators

gen early_career = (age >= 16 & age <= 25)
label variable early_career "early-career indicator"

gen mid_career = (age > 25 & age <= 45)
label variable mid_career "mid-career indicator"

gen late_career = (age > 45 & age <= 64)
label variable late_career "late-career indicator"


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
label define BLACK 1 "black" 0 "not black"
label values black BLACK 


* Create male indicator
gen male = (sex == 1)
label variable male "male indicator"
label define MALE 1 "male" 0 "female"
label values male MALE 


* Create years of schooling variable 
gen yrschool = .
		
* No schooling
replace yrschool = 0 if inlist(educd, 0, 1, 2, 11, 12)

* Grade 1-4
*replace yrschool = 4 if educd == 10 // nursery school to grade 4
*replace yrschool = 4 if educd == 13 // grade 1, 2, 3, or 4
replace yrschool = 1 if educd == 14 // grade 1 
replace yrschool = 2 if educd == 15 // grade 2
replace yrschool = 3 if educd == 16 // grade 3
replace yrschool = 4 if educd == 17 // grade 4 

* Grade 5-8
*replace yrschool = 8 if educd == 20 // grade 5, 6, 7, or 8
*replace yrschool = 6 if educd == 21 // grade 5 or 6
replace yrschool = 5 if educd == 22 // grade 5 
replace yrschool = 6 if educd == 23 // grade 6
*replace yrschool = 8 if educd == 24 // grade 7 or 8
replace yrschool = 7 if educd == 25 // grade 7
replace yrschool = 8 if educd == 26 // grade 8

* High school 
replace yrschool = 9 if educd == 30 // grade 9
replace yrschool = 10 if educd == 40 // grade 10
replace yrschool = 11 if educd == 50 // grade 11
replace yrschool = 12 if educd == 60 // grade 12 
replace yrschool = 12 if inrange(educd, 61, 64) // "12th grade, no diploma" etc.

* College 
replace yrschool = 13 if inlist(educd, 65, 70) // "some college, but less than 1 year" etc.
*replace yrschool = 16 if educd == 71 // 1 or more years of college credit, no degree
replace yrschool = 14 if educd == 80 // 2 years of college 
replace yrschool = 14 if inlist(educd, 81, 82, 83) // associate's degree 
replace yrschool = 15 if educd == 90 // 3 years of college 
replace yrschool = 16 if inlist(educd, 100, 101) // 4 years of college OR bachelor's degree

* Graduate 
*replace yrschool = 18 if educd == 110 // 5+
replace yrschool = 18 if educd == 111 // 6
replace yrschool = 19 if educd == 112 // 7 
*replace yrschool = 20 if educd == 113 // 8+
replace yrschool = 18 if educd == 114 // masters degree 
replace yrschool = 19 if educd == 115 // professional degree beyond bachelors 
replace yrschool = 20 if educd == 116 // doctoral 
label variable yrschool "years of school"

* Potential Experience Variable 
gen pot_exp = age - yrschool - 6 // assumes start school at age 6
replace pot_exp = 0 if pot_exp < 0 // truncate at 0 
label variable pot_exp "potential experience"	

save "nca_acs_soc.dta", replace 


* RESTRICT TO OCCUPATIONS WITH A HIGH-INCIDENCE OF NONCOMPETES -----------------
	
* Precompute the major-group prefix
gen str2 soc2 = substr(occsoc, 1, 2)

* Use byte target 
gen byte target = inlist(soc2, "49", "33", "39", "29", "19", "27") ///
	| inlist(soc2, "13", "25", "31", "11", "15", "17")
	
keep if target
	// (11,977,025 observations deleted)

drop target 

destring soc2, gen(socmaj)

drop soc2 

label define socmaj_names ///
11 "Management" ///
13 "Business & Financial Operations" ///
15 "Computer & Mathematical" ///
17 "Architecture & Engineering" ///
19 "Life, Physical, & Social Sciences" ///
25 "Educational Instruction & Library" ///
27 "Arts, Design, Entertainment, Sports, & Media" ///
29 "Healthcare Practitioners & Technical" ///
31 "Healthcare Support" ///
33 "Protective Service" ///
39 "Personal Care & Service" ///
49 "Installation, Maintenance, & Repair"

label values socmaj socmaj_names

label variable socmaj "major, 2-digit soc code"

save "nca_acs_soc.dta", replace 

* OBSERVATIONS AT THIS POINT: 10,466,055 


* RESTRICT TO ACCOUNT FOR INDUSTRY- AND OCC-LEVEL BANS -------------------------

* NOTE: 
* - Sales occupations (SOC-2: 41) already dropped. 
* - Office and Administrative Support occupations (SOC-2: 43) already dropped 

* (1) Broadcast  

use "nca_acs_soc.dta", clear 

gen broadcast = inlist(occsoc, "272012", "273010", "273011", "273020") ///
	| inlist(occsoc, "273023", "273041", "273043", "274010", "2740XX",  ///
	"274030", "274099")

// INCLUDES: Producers and Directors; Announcers; Broadcast Announcers And Radio 
// Disc Jockeys; News Analysts, Reporters and Correspondents; News Analysts, 
// Reporters, And Journalists; Editors; Writers and Authors; Broadcast and sound 
// engineering technicians and radio operators; Broadcast and Sound Engineering 
// Technicians and Radio Operators, and Media and Communication Equipment 
// Workers, All Other; Other Media And Communication Equipment Workers; 
// Television, Video, and Motion Picture Camera Operators and Editors; Media 
// and Communication Equipment Workers, All Other

drop if broadcast == 1
	// (100,859 observations deleted)

drop broadcast

* (2) Health   
// NOTE: Drop socmaj 29 and 31 to account for healthcare industry bans

drop if socmaj == 29 | socmaj == 31
	// (1,975,117 observations deleted)

* (3) High Tech
// Note: Drop Hawaii to account for its tech industry ban
 
drop if statefip == 15 
	// (48,054 observations deleted)

* (4) Motor Vehicle Industry 
// Note: Drop Montana to account for motor vehicle industry ban

drop if statefip == 30
	// (29,604 observations deleted)

save "nca_acs_soc.dta", replace 

* OBSERVATIONS AT THIS POINT: 8,312,421


* DROP OBS WITH MISSING VALUES OF OUTCOME VARIABLES ----------------------------

* DROP B/C UNCLEAR # OF YEARS OF SCHOOL: 
	*nursery school to grade 4
	*grade 5 or 6
	*grade 7 or 8
	*1 or more years of college credit, no degree
* Dropping where yrschool is missing will take care of this because these are 
* the values where it is missing. 

drop if missing(yrschool)
	// (1,074,901 observations deleted)

* CHECKING THAT SOME VARS AREN'T MISSING: 
*count if missing(pot_exp)
*count if missing(incwage)

save "nca_acs_soc.dta", replace

* OBSERVATIONS AT THIS POINT: 7,237,520


log close 