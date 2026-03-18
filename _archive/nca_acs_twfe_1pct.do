cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "nca_acs_twfe_1pct.log", replace 

clear all 


* NOTE: Ask Drukker about whether I should use fw for how many obs are in each
* cross-section or if I should use perwt. Also whether I should use aweights or
* fweights.


* LOAD DATA --------------------------------------------------------------------

use nca_acs_soc_1pct, clear 


* OUTPUT DIRECTORIES -----------------------------------------------------------
local root "C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/results_acs_1pct/twfe"
local tabdir "`root'/tables_twfe"
local sterdir "`root'/estimates_twfe"

cap mkdir "`tabdir'"
cap mkdir "`sterdir'"


* HW 2008 | AGE OUTCOMES -------------------------------------------------------
local age_vars age early_career mid_career late_career

eststo clear
foreach v of local age_vars {

    * Spec 1: Unconditional
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store `v'_s1
    estimates save "`sterdir'/`v'_hw2008_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s2
    estimates save "`sterdir'/`v'_hw2008_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if ((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s3
    estimates save "`sterdir'/`v'_hw2008_s3.ster", replace
}

* Export an "across-specs" table for each outcome in columns
foreach v of local age_vars {
    esttab `v'_s1 `v'_s2 `v'_s3 using "`tabdir'/hw2008_`v'.csv", ///
        replace label nogaps compress nonotes ///
        keep(treated_eff) ///
        b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
        stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
              labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
        addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
        title("HW 2008 - Outcome: `v' (treated_eff)")
}


* HW 2008 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------
local age_groups early_career mid_career late_career

eststo clear
foreach a of local age_groups {

    * Spec 1: Unconditional
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store wage_`a'_s1
    estimates save "`sterdir'/wage_`a'_hw2008_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s2
    estimates save "`sterdir'/wage_`a'_hw2008_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if (((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s3
    estimates save "`sterdir'/wage_`a'_hw2008_s3.ster", replace
}

label var treated_eff "Date-effective treatment indicator"

esttab  ///
    wage_early_career_s1 wage_early_career_s2 wage_early_career_s3 ///
    wage_mid_career_s1   wage_mid_career_s2   wage_mid_career_s3   ///
    wage_late_career_s1  wage_late_career_s2  wage_late_career_s3  ///
    using "`tabdir'/hw2008_wage_strata.csv", replace ///
    label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(0 0 3 3) labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    mtitle("Early: Uncond." "Early: Occ FE" "Early: Occ FE + L1" ///
           "Mid:   Uncond." "Mid:   Occ FE" "Mid:   Occ FE + L1" ///
           "Late:  Uncond." "Late:  Occ FE" "Late:  Occ FE + L1") ///
    title("HW 2008 - Real income by career stage (treated_eff)")



	
* HW 2020 | AGE OUTCOMES -------------------------------------------------------
local age_vars age early_career mid_career late_career

eststo clear
foreach v of local age_vars {

    * Spec 1: Unconditional
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store `v'_s1
    estimates save "`sterdir'/`v'_hw2020_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s2
    estimates save "`sterdir'/`v'_hw2020_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if ((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s3
    estimates save "`sterdir'/`v'_hw2020_s3.ster", replace
}

* Export an "across-specs" table for each outcome in columns
foreach v of local age_vars {
    esttab `v'_s1 `v'_s2 `v'_s3 using "`tabdir'/hw2020_`v'.csv", ///
        replace label nogaps compress nonotes ///
        keep(treated_eff) ///
        b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
        stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
              labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
        addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
        title("HW 2020 - Outcome: `v' (treated_eff)")
}


* HW 2020 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------
local age_groups early_career mid_career late_career

eststo clear
foreach a of local age_groups {

    * Spec 1: Unconditional
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store wage_`a'_s1
    estimates save "`sterdir'/wage_`a'_hw2020_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s2
    estimates save "`sterdir'/wage_`a'_hw2020_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if (((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s3
    estimates save "`sterdir'/wage_`a'_hw2020_s3.ster", replace
}

label var treated_eff "Date-effective treatment indicator"

esttab  ///
    wage_early_career_s1 wage_early_career_s2 wage_early_career_s3 ///
    wage_mid_career_s1   wage_mid_career_s2   wage_mid_career_s3   ///
    wage_late_career_s1  wage_late_career_s2  wage_late_career_s3  ///
    using "`tabdir'/hw2020_wage_strata.csv", replace ///
    label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(0 0 3 3) labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    mtitle("Early: Uncond." "Early: Occ FE" "Early: Occ FE + L1" ///
           "Mid:   Uncond." "Mid:   Occ FE" "Mid:   Occ FE + L1" ///
           "Late:  Uncond." "Late:  Occ FE" "Late:  Occ FE + L1") ///
    title("HW 2020 - Real income by career stage (treated_eff)")

	

	
* HW 2022 | AGE OUTCOMES -------------------------------------------------------
local age_vars age early_career mid_career late_career

eststo clear
foreach v of local age_vars {

    * Spec 1: Unconditional
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store `v'_s1
    estimates save "`sterdir'/`v'_hw2022_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s2
    estimates save "`sterdir'/`v'_hw2022_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if ((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s3
    estimates save "`sterdir'/`v'_hw2022_s3.ster", replace
}

* Export an "across-specs" table for each outcome in columns
foreach v of local age_vars {
    esttab `v'_s1 `v'_s2 `v'_s3 using "`tabdir'/hw2022_`v'.csv", ///
        replace label nogaps compress nonotes ///
        keep(treated_eff) ///
        b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
        stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
              labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
        addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
        title("HW 2022 - Outcome: `v' (treated_eff)")
}


* HW 2022 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------
local age_groups early_career mid_career late_career

eststo clear
foreach a of local age_groups {

    * Spec 1: Unconditional
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store wage_`a'_s1
    estimates save "`sterdir'/wage_`a'_hw2022_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s2
    estimates save "`sterdir'/wage_`a'_hw2022_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if (((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s3
    estimates save "`sterdir'/wage_`a'_hw2022_s3.ster", replace
}

label var treated_eff "Date-effective treatment indicator"

esttab  ///
    wage_early_career_s1 wage_early_career_s2 wage_early_career_s3 ///
    wage_mid_career_s1   wage_mid_career_s2   wage_mid_career_s3   ///
    wage_late_career_s1  wage_late_career_s2  wage_late_career_s3  ///
    using "`tabdir'/hw2022_wage_strata.csv", replace ///
    label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(0 0 3 3) labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    mtitle("Early: Uncond." "Early: Occ FE" "Early: Occ FE + L1" ///
           "Mid:   Uncond." "Mid:   Occ FE" "Mid:   Occ FE + L1" ///
           "Late:  Uncond." "Late:  Occ FE" "Late:  Occ FE + L1") ///
    title("HW 2022 - Real income by career stage (treated_eff)")
	
	
	

	

* LW 2017 | AGE OUTCOMES -------------------------------------------------------
local age_vars age early_career mid_career late_career

eststo clear
foreach v of local age_vars {

    * Spec 1: Unconditional
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store `v'_s1
    estimates save "`sterdir'/`v'_lw2017_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s2
    estimates save "`sterdir'/`v'_lw2017_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if ((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s3
    estimates save "`sterdir'/`v'_lw2017_s3.ster", replace
}

* Export an "across-specs" table for each outcome in columns
foreach v of local age_vars {
    esttab `v'_s1 `v'_s2 `v'_s3 using "`tabdir'/lw2017_`v'.csv", ///
        replace label nogaps compress nonotes ///
        keep(treated_eff) ///
        b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
        stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
              labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
        addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
        title("LW 2017 - Outcome: `v' (treated_eff)")
}


* LW 2017 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------
local age_groups early_career mid_career late_career

eststo clear
foreach a of local age_groups {

    * Spec 1: Unconditional
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store wage_`a'_s1
    estimates save "`sterdir'/wage_`a'_lw2017_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s2
    estimates save "`sterdir'/wage_`a'_lw2017_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if (((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s3
    estimates save "`sterdir'/wage_`a'_lw2017_s3.ster", replace
}

label var treated_eff "Date-effective treatment indicator"

esttab  ///
    wage_early_career_s1 wage_early_career_s2 wage_early_career_s3 ///
    wage_mid_career_s1   wage_mid_career_s2   wage_mid_career_s3   ///
    wage_late_career_s1  wage_late_career_s2  wage_late_career_s3  ///
    using "`tabdir'/lw2017_wage_strata.csv", replace ///
    label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(0 0 3 3) labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    mtitle("Early: Uncond." "Early: Occ FE" "Early: Occ FE + L1" ///
           "Mid:   Uncond." "Mid:   Occ FE" "Mid:   Occ FE + L1" ///
           "Late:  Uncond." "Late:  Occ FE" "Late:  Occ FE + L1") ///
    title("LW 2017 - Real income by career stage (treated_eff)")	
	

	
	
	
* LW 2018 | AGE OUTCOMES -------------------------------------------------------
local age_vars age early_career mid_career late_career

eststo clear
foreach v of local age_vars {

    * Spec 1: Unconditional
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store `v'_s1
    estimates save "`sterdir'/`v'_lw2018_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s2
    estimates save "`sterdir'/`v'_lw2018_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if ((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s3
    estimates save "`sterdir'/`v'_lw2018_s3.ster", replace
}

* Export an "across-specs" table for each outcome in columns
foreach v of local age_vars {
    esttab `v'_s1 `v'_s2 `v'_s3 using "`tabdir'/lw2018_`v'.csv", ///
        replace label nogaps compress nonotes ///
        keep(treated_eff) ///
        b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
        stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
              labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
        addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
        title("LW 2018 - Outcome: `v' (treated_eff)")
}


* LW 2018 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------
local age_groups early_career mid_career late_career

eststo clear
foreach a of local age_groups {

    * Spec 1: Unconditional
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store wage_`a'_s1
    estimates save "`sterdir'/wage_`a'_lw2018_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s2
    estimates save "`sterdir'/wage_`a'_lw2018_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if (((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s3
    estimates save "`sterdir'/wage_`a'_lw2018_s3.ster", replace
}

label var treated_eff "Date-effective treatment indicator"

esttab  ///
    wage_early_career_s1 wage_early_career_s2 wage_early_career_s3 ///
    wage_mid_career_s1   wage_mid_career_s2   wage_mid_career_s3   ///
    wage_late_career_s1  wage_late_career_s2  wage_late_career_s3  ///
    using "`tabdir'/lw2018_wage_strata.csv", replace ///
    label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(0 0 3 3) labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    mtitle("Early: Uncond." "Early: Occ FE" "Early: Occ FE + L1" ///
           "Mid:   Uncond." "Mid:   Occ FE" "Mid:   Occ FE + L1" ///
           "Late:  Uncond." "Late:  Occ FE" "Late:  Occ FE + L1") ///
    title("LW 2018 - Real income by career stage (treated_eff)")	
	
	




* LW 2019 | AGE OUTCOMES -------------------------------------------------------
local age_vars age early_career mid_career late_career

eststo clear
foreach v of local age_vars {

    * Spec 1: Unconditional
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store `v'_s1
    estimates save "`sterdir'/`v'_lw2019_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s2
    estimates save "`sterdir'/`v'_lw2019_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if ((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s3
    estimates save "`sterdir'/`v'_lw2019_s3.ster", replace
}

* Export an "across-specs" table for each outcome in columns
foreach v of local age_vars {
    esttab `v'_s1 `v'_s2 `v'_s3 using "`tabdir'/lw2019_`v'.csv", ///
        replace label nogaps compress nonotes ///
        keep(treated_eff) ///
        b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
        stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
              labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
        addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
        title("LW 2019 - Outcome: `v' (treated_eff)")
}


* LW 2019 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------
local age_groups early_career mid_career late_career

eststo clear
foreach a of local age_groups {

    * Spec 1: Unconditional
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store wage_`a'_s1
    estimates save "`sterdir'/wage_`a'_lw2019_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s2
    estimates save "`sterdir'/wage_`a'_lw2019_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if (((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s3
    estimates save "`sterdir'/wage_`a'_lw2019_s3.ster", replace
}

label var treated_eff "Date-effective treatment indicator"

esttab  ///
    wage_early_career_s1 wage_early_career_s2 wage_early_career_s3 ///
    wage_mid_career_s1   wage_mid_career_s2   wage_mid_career_s3   ///
    wage_late_career_s1  wage_late_career_s2  wage_late_career_s3  ///
    using "`tabdir'/lw2019_wage_strata.csv", replace ///
    label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(0 0 3 3) labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    mtitle("Early: Uncond." "Early: Occ FE" "Early: Occ FE + L1" ///
           "Mid:   Uncond." "Mid:   Occ FE" "Mid:   Occ FE + L1" ///
           "Late:  Uncond." "Late:  Occ FE" "Late:  Occ FE + L1") ///
    title("LW 2019 - Real income by career stage (treated_eff)")	
	

	

	
* LW 2020 | AGE OUTCOMES -------------------------------------------------------
local age_vars age early_career mid_career late_career

eststo clear
foreach v of local age_vars {

    * Spec 1: Unconditional
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store `v'_s1
    estimates save "`sterdir'/`v'_lw2020_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s2
    estimates save "`sterdir'/`v'_lw2020_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if ((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s3
    estimates save "`sterdir'/`v'_lw2020_s3.ster", replace
}

* Export an "across-specs" table for each outcome in columns
foreach v of local age_vars {
    esttab `v'_s1 `v'_s2 `v'_s3 using "`tabdir'/lw2020_`v'.csv", ///
        replace label nogaps compress nonotes ///
        keep(treated_eff) ///
        b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
        stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
              labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
        addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
        title("LW 2020 - Outcome: `v' (treated_eff)")
}


* LW 2020 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------
local age_groups early_career mid_career late_career

eststo clear
foreach a of local age_groups {

    * Spec 1: Unconditional
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store wage_`a'_s1
    estimates save "`sterdir'/wage_`a'_lw2020_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s2
    estimates save "`sterdir'/wage_`a'_lw2020_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if (((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s3
    estimates save "`sterdir'/wage_`a'_lw2020_s3.ster", replace
}

label var treated_eff "Date-effective treatment indicator"

esttab  ///
    wage_early_career_s1 wage_early_career_s2 wage_early_career_s3 ///
    wage_mid_career_s1   wage_mid_career_s2   wage_mid_career_s3   ///
    wage_late_career_s1  wage_late_career_s2  wage_late_career_s3  ///
    using "`tabdir'/lw2020_wage_strata.csv", replace ///
    label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(0 0 3 3) labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    mtitle("Early: Uncond." "Early: Occ FE" "Early: Occ FE + L1" ///
           "Mid:   Uncond." "Mid:   Occ FE" "Mid:   Occ FE + L1" ///
           "Late:  Uncond." "Late:  Occ FE" "Late:  Occ FE + L1") ///
    title("LW 2020 - Real income by career stage (treated_eff)")	
	

	

	
* LW 2021 | AGE OUTCOMES -------------------------------------------------------
local age_vars age early_career mid_career late_career

eststo clear
foreach v of local age_vars {

    * Spec 1: Unconditional
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store `v'_s1
    estimates save "`sterdir'/`v'_lw2021_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe `v' treated_eff ///
        if ((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s2
    estimates save "`sterdir'/`v'_lw2021_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe `v' treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if ((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store `v'_s3
    estimates save "`sterdir'/`v'_lw2021_s3.ster", replace
}

* Export an "across-specs" table for each outcome in columns
foreach v of local age_vars {
    esttab `v'_s1 `v'_s2 `v'_s3 using "`tabdir'/lw2021_`v'.csv", ///
        replace label nogaps compress nonotes ///
        keep(treated_eff) ///
        b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
        stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
              labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
        addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
        title("LW 2021 - Outcome: `v' (treated_eff)")
}


* LW 2021 | WAGE OUTCOMES (Stratified by Career Stage) -------------------------
local age_groups early_career mid_career late_career

eststo clear
foreach a of local age_groups {

    * Spec 1: Unconditional
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year) vce(cluster statefip)
    estimates store wage_`a'_s1
    estimates save "`sterdir'/wage_`a'_lw2021_s1.ster", replace

    * Spec 2: + Occupation FE
    quietly reghdfe incwage_r treated_eff ///
        if (((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s2
    estimates save "`sterdir'/wage_`a'_lw2021_s2.ster", replace

    * Spec 3: + Occupation FE + Lagged Controls
    quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
        if (((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) & `a'==1) ///
        [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
    estimates store wage_`a'_s3
    estimates save "`sterdir'/wage_`a'_lw2021_s3.ster", replace
}

label var treated_eff "Date-effective treatment indicator"

esttab  ///
    wage_early_career_s1 wage_early_career_s2 wage_early_career_s3 ///
    wage_mid_career_s1   wage_mid_career_s2   wage_mid_career_s3   ///
    wage_late_career_s1  wage_late_career_s2  wage_late_career_s3  ///
    using "`tabdir'/lw2021_wage_strata.csv", replace ///
    label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(0 0 3 3) labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    mtitle("Early: Uncond." "Early: Occ FE" "Early: Occ FE + L1" ///
           "Mid:   Uncond." "Mid:   Occ FE" "Mid:   Occ FE + L1" ///
           "Late:  Uncond." "Late:  Occ FE" "Late:  Occ FE + L1") ///
    title("LW 2021 - Real income by career stage (treated_eff)")	
	



log close 