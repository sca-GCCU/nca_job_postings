cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "nca_acs_sum_stats.log", replace 


* SUMMARY STATISTICS WITH NON-SOC CODE DATA

use "nca_acs.dta", clear 

* Recode sex 
replace sex = 0 if sex == 2
label define sex_label 0 "female" 1 "male"
label values sex sex_label


* BALANCE TABLE 

local balance_var age young_adult earlyc_adult mlc_adult older_adult ///
	yrschool incwage ///
	no_high_school high_school some_college college ///
	employment_nsa income_pcap hpi sex black
	
quietly estpost tabstat `balance_var' [fw=perwt], by(ban) ///
	statistics(mean sd) columns(statistics) listwise 

esttab . using balance_table.tex, main(mean) aux(sd) nostar unstack ///
	nonote label compress booktabs replace  

	// Add count separately 

qui summarize `balance_var' if ban == 0
di "The number of control observations is: " r(N)
qui summarize `balance_var' if ban == 1 
di "The number of treated observations is: " r(N)

	// Note ttest won't let me use frequency weights, so I'm not doing it 
	

* PRE AND POST TABLE  

local out_var age young_adult earlyc_adult mlc_adult older_adult ///
	no_high_school high_school some_college college
	
quietly estpost tabstat `out_var' if ban == 1 [fw=perwt], by(treated_eff) ///
	statistics(mean sd) columns(statistics) listwise 
	
esttab . using pre_post_table.tex, main(mean) aux(sd) nostar unstack ///
	nonote label compress booktabs replace  
	
	// Add count separately 

qui summarize `out_var' if ban == 1 & treated_eff == 0
di "The number of pre-treatment observations is: " r(N)
qui summarize `out_var' if ban == 1 & treated_eff == 1
di "The number of post-treatment observations is: " r(N)




* SUMMARY STATISTICS WITH SOC CODE DATA (NO WEIGHTS)
clear all 

use "nca_acs_soc.dta", clear 

* Recode sex 
replace sex = 0 if sex == 2
label define sex_label 0 "female" 1 "male"
label values sex sex_label

* BALANCE TABLE 
gen ban_rev = 1 - ban // Ensure proper direction of difference 

local balance_var age young_adult earlyc_adult mlc_adult older_adult ///
	yrschool incwage ///
	no_high_school high_school some_college college ///
	employment_nsa income_pcap hpi sex black

tabstat `balance_var', by(ban) statistics(mean sd) columns(statistics) ///
    longstub varwidth(18) format(%9.3f)
	
estpost ttest `balance_var', by(ban_rev) unequal // Welch two-sample t-test

esttab ., ///
    cells("mu_2(fmt(%9.3f)) mu_1(fmt(%9.3f)) b(fmt(%9.3f)) se(fmt(%9.3f))") ///
    unstack

drop ban_rev 

tab ban


* PRE AND POST TABLE - EXCLUDING RIGHT NOW  
gen treated_eff_rev = 1 - treated_eff // Ensure proper direction of difference 

tab treated_eff_rev if ban == 1

local out_var age young_adult earlyc_adult mlc_adult older_adult ///
	yrschool incwage ///
	no_high_school high_school some_college college
	
qui estpost tabstat `out_var' if ban == 1, by(treated_eff) ///
	statistics(mean sd) columns(statistics) listwise 
	
esttab ., main(mean) aux(sd) nostar unstack 

qui estpost ttest `out_var' if ban == 1, by(treated_eff) unequal
	
esttab ., ///
    cells("mu_2(fmt(%9.3f)) mu_1(fmt(%9.3f)) b(fmt(%9.3f)) se(fmt(%9.3f))") ///
    unstack

drop treated_eff_rev





	
	


log close 