cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "nca_acs_csdid.log", replace 

* Install drdid
//ssc install drdid, all replace 

* Install csdid
//ssc install csdid, all replace 

* Load data with 2-digit soc code restrictions 
clear all 

use "nca_acs_soc.dta", clear 

* ESTIMATION FOR AGE
* By year effective
		
keep age perwt year year_eff_ban // drop unnecessary variables 

compress 

* 1% per cell 
preserve 
sample 1, by(year year_eff_ban) // 1% w/i each group-time cell
timer on 1
csdid age [iweight=perwt], time(year) gvar(year_eff_ban) method(drimp)
timer off 1
restore 

* 5% per cell
preserve 
sample 5, by(year year_eff_ban) // 5% w/i each group-time cell 
timer on 2
csdid age [iweight=perwt], time(year) gvar(year_eff_ban) method(drimp)
timer off 2
restore 

timer list 

	// Note: The code does work. Though it takes about an hour to run (1% & 5%).

	
* By year enacted

replace year_enact_ban = 0 if missing(year_enact_ban) // recode gvar properly 


