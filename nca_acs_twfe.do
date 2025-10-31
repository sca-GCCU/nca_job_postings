cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "nca_acs_twfe.log", replace 

clear all 

* LOAD DATA --------------------------------------------------------------------

use nca_acs_soc, clear 

* TWFE ESTIMATES ---------------------------------------------------------------

* NOTE: Ask Drukker about whether I should use fw for how many obs are in each
* cross-section or if I should use perwt. Also whether I should use aweights or
* fweights.

* (I) HW 2008 ------------------------------------------------------------------

* (1) Age Estimates

local age_vars age early_career mid_career late_career 

* Spec 1: Unconditional 
foreach v in `age_vars' {
	di "*** `v' | unconditional ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs
foreach v in `age_vars' {
	di "*** `v' | with OCC FE ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 
foreach v in `age_vars' {
	di "*** `v' | with OCC FE and lagged controls ***"
	reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if ((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}


* (2) Wage Estimates 

* Spec 1: Unconditional

* (a) All Age Groups

di "*** real income | all groups | unconditional ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | unconditional ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs

* (a) All Age Groups

di "*** real income | all groups | with OCC FE ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 

* (a) All Age Groups

di "*** real income | all groups | with OCC FE and lagged controls ***"
reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
	if ((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE and lagged controls ***"
	reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if (((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}




* (II) HW 2020 -----------------------------------------------------------------

* (1) Age Estimates

local age_vars age early_career mid_career late_career 

* Spec 1: Unconditional 
foreach v in `age_vars' {
	di "*** `v' | unconditional ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs
foreach v in `age_vars' {
	di "*** `v' | with OCC FE ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 
foreach v in `age_vars' {
	di "*** `v' | with OCC FE and lagged controls ***"
	reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if ((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}


* (2) Wage Estimates 

* Spec 1: Unconditional

* (a) All Age Groups

di "*** real income | all groups | unconditional ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | unconditional ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs

* (a) All Age Groups

di "*** real income | all groups | with OCC FE ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 

* (a) All Age Groups

di "*** real income | all groups | with OCC FE and lagged controls ***"
reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
	if ((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE and lagged controls ***"
	reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if (((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}



* (III) HW 2022 ----------------------------------------------------------------

* (1) Age Estimates

local age_vars age early_career mid_career late_career 

* Spec 1: Unconditional 
foreach v in `age_vars' {
	di "*** `v' | unconditional ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs
foreach v in `age_vars' {
	di "*** `v' | with OCC FE ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 
foreach v in `age_vars' {
	di "*** `v' | with OCC FE and lagged controls ***"
	reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if ((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}


* (2) Wage Estimates 

* Spec 1: Unconditional

* (a) All Age Groups

di "*** real income | all groups | unconditional ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | unconditional ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs

* (a) All Age Groups

di "*** real income | all groups | with OCC FE ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 

* (a) All Age Groups

di "*** real income | all groups | with OCC FE and lagged controls ***"
reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
	if ((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE and lagged controls ***"
	reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if (((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}



* (IV) LW 2017 -----------------------------------------------------------------

* (1) Age Estimates

local age_vars age early_career mid_career late_career 

* Spec 1: Unconditional 
foreach v in `age_vars' {
	di "*** `v' | unconditional ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs
foreach v in `age_vars' {
	di "*** `v' | with OCC FE ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 
foreach v in `age_vars' {
	di "*** `v' | with OCC FE and lagged controls ***"
	reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if ((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}


* (2) Wage Estimates 

* Spec 1: Unconditional

* (a) All Age Groups

di "*** real income | all groups | unconditional ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | unconditional ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs

* (a) All Age Groups

di "*** real income | all groups | with OCC FE ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 

* (a) All Age Groups

di "*** real income | all groups | with OCC FE and lagged controls ***"
reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
	if ((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE and lagged controls ***"
	reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if (((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}



* (V) LW 2018 ------------------------------------------------------------------

* (1) Age Estimates

local age_vars age early_career mid_career late_career 

* Spec 1: Unconditional 
foreach v in `age_vars' {
	di "*** `v' | unconditional ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs
foreach v in `age_vars' {
	di "*** `v' | with OCC FE ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 
foreach v in `age_vars' {
	di "*** `v' | with OCC FE and lagged controls ***"
	reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if ((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}


* (2) Wage Estimates 

* Spec 1: Unconditional

* (a) All Age Groups

di "*** real income | all groups | unconditional ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | unconditional ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs

* (a) All Age Groups

di "*** real income | all groups | with OCC FE ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 

* (a) All Age Groups

di "*** real income | all groups | with OCC FE and lagged controls ***"
reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
	if ((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE and lagged controls ***"
	reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if (((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}



* (VI) LW 2019 -----------------------------------------------------------------

* (1) Age Estimates

local age_vars age early_career mid_career late_career 

* Spec 1: Unconditional 
foreach v in `age_vars' {
	di "*** `v' | unconditional ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs
foreach v in `age_vars' {
	di "*** `v' | with OCC FE ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 
foreach v in `age_vars' {
	di "*** `v' | with OCC FE and lagged controls ***"
	reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if ((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}


* (2) Wage Estimates 

* Spec 1: Unconditional

* (a) All Age Groups

di "*** real income | all groups | unconditional ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | unconditional ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs

* (a) All Age Groups

di "*** real income | all groups | with OCC FE ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 

* (a) All Age Groups

di "*** real income | all groups | with OCC FE and lagged controls ***"
reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
	if ((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE and lagged controls ***"
	reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if (((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}



* (VII) LW 2020 ----------------------------------------------------------------

* (1) Age Estimates

local age_vars age early_career mid_career late_career 

* Spec 1: Unconditional 
foreach v in `age_vars' {
	di "*** `v' | unconditional ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs
foreach v in `age_vars' {
	di "*** `v' | with OCC FE ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 
foreach v in `age_vars' {
	di "*** `v' | with OCC FE and lagged controls ***"
	reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if ((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}


* (2) Wage Estimates 

* Spec 1: Unconditional

* (a) All Age Groups

di "*** real income | all groups | unconditional ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | unconditional ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs

* (a) All Age Groups

di "*** real income | all groups | with OCC FE ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 

* (a) All Age Groups

di "*** real income | all groups | with OCC FE and lagged controls ***"
reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
	if ((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE and lagged controls ***"
	reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if (((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}



* (VIII) LW 2021 ---------------------------------------------------------------

* (1) Age Estimates

local age_vars age early_career mid_career late_career 

* Spec 1: Unconditional 
foreach v in `age_vars' {
	di "*** `v' | unconditional ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs
foreach v in `age_vars' {
	di "*** `v' | with OCC FE ***"
	reghdfe `v' treated_eff ///
		if ((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 
foreach v in `age_vars' {
	di "*** `v' | with OCC FE and lagged controls ***"
	reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if ((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}


* (2) Wage Estimates 

* Spec 1: Unconditional

* (a) All Age Groups

di "*** real income | all groups | unconditional ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | unconditional ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year) vce(cluster statefip)
}

* Spec 2: Occupation FEs

* (a) All Age Groups

di "*** real income | all groups | with OCC FE ***"
reghdfe incwage_r treated_eff ///
	if ((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE ***"
	reghdfe incwage_r treated_eff ///
		if (((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}

* Spec 3: Occupation FEs and Lagged Controls 

* (a) All Age Groups

di "*** real income | all groups | with OCC FE and lagged controls ***"
reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
	if ((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) ///
	[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
	
* (b) Stratified 

local age_groups early_career mid_career late_career

foreach a in `age_groups' {
	di "*** real income | stratum `a' | with OCC FE and lagged controls ***"
	reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
		if (((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) & `a' == 1) ///
		[pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
}



log close 