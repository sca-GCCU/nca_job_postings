cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "oes_groups.log", replace 

* IDENTIFY OCCUPATION GROUPS BASED ON WAGES OES (2018)
clear all 

***import dataset

local myvar H_MEAN A_MEAN H_PCT10 H_PCT25 H_MEDIAN H_PCT75 H_PCT90 A_PCT10 A_PCT25 A_MEDIAN A_PCT75 A_PCT90
destring `myvar', replace force 

	* Decide which variable to group by
count if missing(A_PCT10)
tabulate OCC_TITLE if missing(A_PCT10)

	* Note: Use A_PCT10 because otherwise you run into top-coding.

		* Drop obervations for which A_PCT10 is missing. 
preserve 
keep if missing(A_PCT10)
save "oes_2018_missing.dta", replace 
restore 

drop if missing(A_PCT10)
		
		* Histogram 
histogram A_PCT10, width(2000) frequency

		* Create quartile cutoffs (rather than decile cutoffs)	
xtile wage_quartile = A_PCT10, n(4)
summarize A_PCT10, detail 
gen wage_q1 = (wage_quartile == 1) // low wage 
gen wage_q2 = (wage_quartile == 2) // modest wage 
gen wage_q3 = (wage_quartile == 3) // medium wage 
gen wage_q4 = (wage_quartile == 4) // high wage 
tabulate wage_quartile
label define wagegroup 1 "Yes" 0 "No"
label values wage_q1 wagegroup
label values wage_q2 wagegroup
label values wage_q3 wagegroup
label values wage_q4 wagegroup

	* Save occ group dataset
keep OCC_CODE OCC_TITLE wage_q1 wage_q2 wage_q3 wage_q4
save oes_2018_groups, replace 

	
* IDENTIFY OCCUPATION GROUPS BASED ON WAGES OES (2010)
clear all 

***import dataset

local myvar H_MEAN A_MEAN H_PCT10 H_PCT25 H_MEDIAN H_PCT75 H_PCT90 A_PCT10 A_PCT25 A_MEDIAN A_PCT75 A_PCT90
destring `myvar', replace force 

preserve 
keep if missing(A_PCT10)
save "oes_2010_missing.dta", replace 
restore 

drop if missing(A_PCT10)

histogram A_PCT10, width(2000) frequency

xtile wage_quartile = A_PCT10, n(4)
summarize A_PCT10, detail 
gen wage_q1 = (wage_quartile == 1) // low wage 
gen wage_q2 = (wage_quartile == 2) // modest wage 
gen wage_q3 = (wage_quartile == 3) // medium wage 
gen wage_q4 = (wage_quartile == 4) // high wage 
tabulate wage_quartile
label define wagegroup 1 "Yes" 0 "No"
label values wage_q1 wagegroup
label values wage_q2 wagegroup
label values wage_q3 wagegroup
label values wage_q4 wagegroup

keep OCC_CODE OCC_TITLE wage_q1 wage_q2 wage_q3 wage_q4
save oes_2010_groups, replace 


* IDENTIFY OCCUPATION GROUPS BASED ON WAGES OES (2000)
clear all 

***import dataset

local myvar h_mean a_mean mean_prse h_wpct10 h_wpct25 h_median h_wpct75 h_wpct90 a_wpct10 a_wpct25 a_median a_wpct75 a_wpct90
	
destring `myvar', replace force 	
	
count if missing(a_wpct10)
tabulate occ_titl if missing(a_wpct10)

preserve 
keep if missing(a_wpct10)
save "oes_2000_missing.dta", replace
restore 

drop if missing(a_wpct10)

histogram a_wpct10, width(2000) frequency

xtile wage_quartile = a_wpct10, n(4)
summarize a_wpct10, detail 
gen wage_q1 = (wage_quartile == 1) // low wage 
gen wage_q2 = (wage_quartile == 2) // modest wage 
gen wage_q3 = (wage_quartile == 3) // medium wage 
gen wage_q4 = (wage_quartile == 4) // high wage 
tabulate wage_quartile
label define wagegroup 1 "Yes" 0 "No"
label values wage_q1 wagegroup
label values wage_q2 wagegroup
label values wage_q3 wagegroup
label values wage_q4 wagegroup

keep occ_code occ_titl wage_q1 wage_q2 wage_q3 wage_q4
save oes_2000_groups, replace  


* PREP OCC GROUPS FOR MERGE

use oes_2000_groups, clear 
replace occ_code = subinstr(occ_code, "-", "", .)
list occ_code if strpos(occ_code, "-") > 0 
rename occ_code occsoc
save oes_2000_groups, replace  


use oes_2010_groups, clear 
replace OCC_CODE = subinstr(OCC_CODE, "-", "", .)
list OCC_CODE if strpos(OCC_CODE, "-") > 0
rename OCC_CODE occsoc
save oes_2010_groups, replace 

use oes_2018_groups, clear 
replace OCC_CODE = subinstr(OCC_CODE, "-", "", .)
list OCC_CODE if strpos(OCC_CODE, "-") > 0
rename OCC_CODE occsoc
save oes_2018_groups, replace 

	
*************************


* MERGE ACS DATA WITH OCC GROUP DATA

* Split data by time period 
use nca_acs_p2.dta, clear 

	* 2001–2009 subset
keep if inrange(year, 2001, 2009)
save acs_2001_2009.dta, replace

use nca_acs_p2.dta, clear 

	* 2010–2017 subset
keep if inrange(year, 2010, 2017)
save acs_2010_2017.dta, replace

use nca_acs_p2.dta, clear 

	* 2018–2023 subset
keep if inrange(year, 2018, 2023)
save acs_2018_2023.dta, replace

* Merge with proper oes group 

use acs_2001_2009.dta, clear
merge m:1 occsoc using "oes_2000_groups.dta"
save acs_2001_2009_merged.dta, replace

use acs_2010_2017.dta, clear
merge m:1 occsoc using "oes_2010_groups.dta"
save acs_2010_2017_merged.dta, replace

use acs_2018_2023.dta, clear
merge m:1 occsoc using "oes_2018_groups.dta"
save acs_2018_2023_merged.dta, replace

use oes_2018_groups, clear 
duplicates report occsoc

* NOTE: Not working/merging properly. Tons of unmerged values. 
	* Figure out a different way of segmenting my occupation group. 
	* See other Clemens paper for hints. 


log close 