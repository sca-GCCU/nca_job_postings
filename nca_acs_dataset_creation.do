cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

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



* MERGE ACS AND NONCOMPETE BAN DATA (SOC VERSION)
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

	* Drop anyone who is not employed
keep if empstat == 1

save "nca_acs.dta", replace 
	

* MERGE COVARIATES WITH ACS DATA (SOC VERSION)
clear all 

use "nca_acs_soc_no_controls.dta", clear 

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

save "nca_acs_soc.dta", replace 


	
	
* CREATING SOME RELEVANT VARIABLES (NON-SOC DATA)
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
label variable yrschool "potential experience"

save "nca_acs.dta", replace 



* CREATING SOME RELEVANT VARIABLES (SOC VERSION)
clear all

use "nca_acs_soc.dta", clear 

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
label variable yrschool "potential experience"

save "nca_acs_soc.dta", replace 


* RESTRICTING ANALYSIS TO HIGH-INCIDENCE OCCUPATIONS 
	
	* Precompute the major-group prefix
gen str2 soc2 = substr(occsoc, 1, 2)
	
	* Use byte target 
gen byte target = inlist(soc2, "49", "33", "39", "29", "19", "27") ///
	| inlist(soc2, "13", "25", "31", "11", "15", "17")
	
keep if target

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


* RESTRICTIONS TO ACCOUNT FOR INDUSTRY/OCCUPATION BANS (SOC VERSION)

* NOTE: 
* - Sales occupations (SOC-2: 41) already dropped. 
* - Office and Administrative Support occupations (SOC-2: 43) already dropped 

* Broadcast  

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

drop broadcast

* Health   

drop if socmaj == 29 | socmaj == 31

// NOTE: Drop socmaj 29 and 31 to account for healthcare industry bans

* High Tech
 
drop if statefip == 15 

// Note: Drop Hawaii to account for its tech industry ban

* Motor Vehicle Industry 

drop if statefip == 30

// Note: Drop Montana to account for motor vehicle industry ban

save "nca_acs_soc.dta", replace 




log close 