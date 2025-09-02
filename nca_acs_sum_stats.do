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


******************************************************


* SUMMARY STATISTICS WITH SOC CODE DATA 

* BALANCE TABLE 
clear all 

use "nca_acs_soc.dta", clear 

* Recode sex 
replace sex = 0 if sex == 2
label define sex_label 0 "female" 1 "male"
label values sex sex_label

* Recale to account for pooling 23 years 
gen perwt_pool = perwt/23

qui summ perwt_pool, meanonly
gen double perwt_norm = perwt_pool/r(mean) 

* Weighted means and SD by ban
local balance_var age young_adult earlyc_adult mlc_adult older_adult ///
	yrschool incwage no_high_school high_school some_college college ///
	employment_nsa income_pcap hpi sex black

eststo clear 
	
quietly estpost summarize `balance_var' if ban==0 [aweight=perwt_norm], listwise
eststo control 

quietly estpost summarize `balance_var' if ban==1 [aweight=perwt_norm], listwise
eststo treated 

* Welch unequal-variance t-tests
quietly estpost ttest `balance_var', by(ban) unequal 
eststo diff 

* Export table - DIFFERENCE 
esttab control treated diff using "balance_soc_table.tex", ///
    replace label compress nonote ///
    mlabels("Control" "Treated" "Control - Treated") ///
    cells( (mean(fmt(%9.3f))) ///
		   (sd(par fmt(%9.3f))) ///
           (b(fmt(%9.3f))) ///
		   (se(par fmt(%9.3f))) ) ///
	title("Balance Table (ACS pooled, weights normalized; Welch t-test)")
	
	// Note: Delete observations in difference column. The number is different 
	// from the sum of the obs in the Control and Treated columns because those
	// columns only use observations for which none of the variables is missing.

* Export table - P-VALUE 
esttab control treated diff using "balance_soc_table_pv.tex", ///
    replace label compress nonote ///
    mlabels("Control" "Treated" "Welch p-value") ///
    cells( ///
        "mean(fmt(%9.3f))" ///   // Control
        "sd(par fmt(%9.3f))" ///   // Treated
        "p(fmt(%9.3g))" ) ///                       // Welch two-sided p-value in diff
    title("Balance Table (ACS pooled, weights normalized; Welch t-test)")

	
* SAMPLE COMPOSITION
* By year effective 
tab year_eff_ban
	
* By year enacted 
tab year_enact_ban

	// Note: Need to create Latex tables from these outputs. 


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