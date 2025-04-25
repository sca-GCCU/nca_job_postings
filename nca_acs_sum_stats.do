cd "C:\Users\scana\OneDrive\Documents\nca_job_postings\data"

log using "nca_acs_sum_stats.log", replace 

use "nca_acs.dta", clear 

* Recode sex 
replace sex = 0 if sex == 2
label define sex_label 0 "female" 1 "male"
label values sex sex_label


* BALANCE TABLE 

local balance_var age young_adult earlyc_adult mlc_adult older_adult ///
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




log close 