cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "nca_acs_analysis.log", replace 

* STACKED DID 

use "nca_acs.dta", clear 

* Choose Event Window 

tab statefip year_eff_ban 
tab statefip hw_ban

	// pre = 8 years, post = 3 years

* Build dataset for each subexperiment 

local years 2008 2015 2017 2018 2019 2020 
local ka = 7 
local kb = 3

foreach y in `years' {
	use nca_acs.dta, clear 
	gen Tsd = (year_eff_ban == `y') // treated for subexp
	gen Csd = (year_eff_ban > `y' + `kb') // clean control for subexp
	gen Mtd = (year <= `y' + `kb' & year >= `y' - `ka') // date in subexp
	gen Istd = Mtd*(Tsd | Csd) // inclusion indicator 
	qui keep if Istd == 1
	
	gen rel_year = year - `y' // reindexing year
	gen cohort = `y' // keeps track of subexperiment
	
	qui save sub_experiment_`y', replace 
}

use sub_experiment_2008, clear 
foreach y in 2015 2017 2018 2019 2020 {
	qui append using sub_experiment_`y'
}
save "stacked_did_data.dta", replace 




* Try for individual sub-experiments 

	// EVERY OUTCOME, EVERY SUB-EXP, NO CONTROLS

local years 2008 2015 2017 2018 2019 2020 
local out_var age young_adult earlyc_adult mlc_adult older_adult ///
	no_high_school high_school some_college college
foreach o in `out_var' {
	local out_est_`o' ""
	foreach y in `years' {
		use sub_experiment_`y', clear 
		keep `o' treated_eff perwt statefip year 
		qui didregress (`o') (treated_eff) [fw=perwt], group(statefip) time(year)
		estimates store did_`o'_`y'
		local out_est_`o' "`out_est_`o'' did_`o'_`y'"
	}
	esttab `out_est_`o'' using atet_tab_`o'.tex, ///
		se star(* 0.1 ** 0.05 *** 0.01) ///
		label compress booktabs replace ///
		drop(20*) ///
		mtitles("2008 ban" "2015 ban" "2017 ban" ///
            "2018 ban" "2019 ban" "2020 ban") ///
		title("DID Estimates - `o'")
}

	// EVERY OUTCOME, EVERY SUB-EXP, W/ CONTROLS  
	
local years 2008 2015 2017 2018 2019 2020 
local out_var age young_adult earlyc_adult mlc_adult older_adult ///
	no_high_school high_school some_college college
local controls employment_nsa income_pcap hpi
foreach o in `out_var' {
	local out_est_`o' ""
	foreach y in `years' {
		use sub_experiment_`y', clear 
		keep `o' treated_eff perwt statefip year `controls'
		qui didregress (`o' `controls') (treated_eff) [fw=perwt], ///
			group(statefip) time(year)
		estimates store did_`o'_`y'
		local out_est_`o' "`out_est_`o'' did_`o'_`y'"
	}
	di "`out_est_`o''"
	esttab `out_est_`o'' using atet_tab_`o'_controls.tex, ///
		se star(* 0.1 ** 0.05 *** 0.01) ///
		label compress booktabs replace ///
		drop(20*) ///
		mtitles("2008 ban" "2015 ban" "2017 ban" ///
            "2018 ban" "2019 ban" "2020 ban") ///
		title("DID Estimates with Controls - `o'")
}

	// EVERY OUTCOME, EVERY SUB-EXP, W/ CONTROLS + OCC 

local years 2008 2015 2017 2018 2019 2020 
local out_var age young_adult earlyc_adult mlc_adult older_adult ///
	no_high_school high_school some_college college
local controls employment_nsa income_pcap hpi
foreach o in `out_var' {
	local out_est_`o' ""
	foreach y in `years' {
		use sub_experiment_`y', clear 
		keep `o' treated_eff perwt statefip year `controls' occ
		qui didregress (`o' `controls' i.occ) (treated_eff) [fw=perwt], ///
			group(statefip) time(year)
		estimates store did_`o'_`y'
		local out_est_`o' "`out_est_`o'' did_`o'_`y'"
	}
	esttab `out_est_`o'' using atet_tab_`o'_controls_occ.tex, ///
		se star(* 0.1 ** 0.05 *** 0.01) ///
		label compress booktabs replace ///
		drop(20* *occ*) ///
		mtitles("2008 ban" "2015 ban" "2017 ban" ///
            "2018 ban" "2019 ban" "2020 ban") ///
		title("DID Estimates with Controls and Occupation Fixed Effects - `o'")
}

		// note: ran with di "`out_est_`o''" to ensure that the order of the
			// local macro was right. 
 


* Tests I did 

	// testing for single sub-experiment 
	
use sub_experiment_2008, clear 

keep age treated_eff perwt statefip year // DON'T SAVE 
didregress (age) (treated_eff) [fw=perwt], group(statefip) time(year)

	// Now age for every sub-experiment

local years 2008 2015 2017 2018 2019 2020 
foreach y in `years' {
	use sub_experiment_`y', clear 
	keep age treated_eff perwt statefip year 
	didregress (age) (treated_eff) [fw=perwt], group(statefip) time(year)
	estimates store did_`y'
}
estimates table did_2008 did_2015 did_2017 did_2018 did_2019 did_2020, ///
	b(%6.5g) se(%5.4g) modelwidth(10) ///
	equations(1:1:1:1:1:1) drop(Controls:)
	
	// testing single sub-experiment with controls

use sub_experiment_2008, clear 

keep age treated_eff perwt statefip year employment_nsa income_pcap hpi  
didregress (age employment_nsa income_pcap hpi) (treated_eff) [fw=perwt], ///
	group(statefip) time(year)

	// testing with esttab 
	
local years 2008 2015  
local controls employment_nsa income_pcap hpi
foreach y in `years' {
	use sub_experiment_`y', clear 
	keep age treated_eff perwt statefip year `controls'
	qui didregress (age `controls') (treated_eff) [fw=perwt], ///
		group(statefip) time(year)
	estimates store did_`y'
}
esttab did_2008 did_2015, ///
	se star(* 0.1 ** 0.05 *** 0.01) ///
	label compress booktabs replace ///
	drop(20*) ///
	mtitles("age (2008 ban)" "age (2015 ban)") ///
	title("Difference-in-Differences Estimates - Age")

	// testing occupation fixed effects 
	
use sub_experiment_2008, clear 

keep age treated_eff perwt statefip year employment_nsa income_pcap hpi occ 
didregress (age employment_nsa income_pcap hpi i.occ) (treated_eff) ///
	[fw=perwt], group(statefip) time(year)

estimates store test_stuff 

esttab test_stuff, ///
	se star(* 0.1 ** 0.05 *** 0.01) ///
	label compress booktabs replace ///
	drop(20* *occ*) ///
	title("Difference-in-Differences Estimates with Controls + Occ - Age")


	
	
* Try Stacked DID 

use "stacked_did_data.dta", clear 

	// test for age 

	// prepping the data for use 

keep statefip perwt treated_eff age rel_year // DON'T SAVE

// PRIOR STACKED DID ATTEMPTS  

// Error: treatment variable varies at the statefip rel_year level 
//replace rel_year = rel_year + 7	
//didregress (age) (treated_eff) [fw=perwt] , group(statefip) time(rel_year)


// This was crashing my code
//gen Ptd = (rel_year >= 0)
//reg age i.Tsd i.Ptd i.Tsd#i.Ptd [fw=perwt] , vce(cluster statefip)

	

log close 