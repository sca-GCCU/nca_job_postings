********************************************************************************
* Unstratified wage TWFE only
* Runs for HW: 2008, 2020, 2022  |  LW: 2017, 2018, 2019, 2020, 2021
* Saves .ster and .csv in the same directories and with the same filenames
********************************************************************************

cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "nca_acs_twfe_wage_unstrat.log", replace

clear all
set more off

* LOAD DATA --------------------------------------------------------------------
use nca_acs_soc, clear
compress

* OUTPUT DIRECTORIES -----------------------------------------------------------
local root "C:/Users/scana/OneDrive/Documents/research/projects/nca_job_postings/results_acs_full/twfe"
local tabdir "`root'/tables_twfe"
local sterdir "`root'/estimates_twfe"

cap mkdir "`tabdir'"
cap mkdir "`sterdir'"

* Labels for tables ------------------------------------------------------------
label var treated_eff "Date-effective treatment indicator"

********************************************************************************
* HW 2008 | WAGE OUTCOMES (Unstratified)
********************************************************************************
eststo clear

* Spec 1: Unconditional
quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year) vce(cluster statefip)
estimates store wage_s1
estimates save "`sterdir'/wage_hw2008_s1.ster", replace

* Spec 2: + Occupation FE
quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s2
estimates save "`sterdir'/wage_hw2008_s2.ster", replace

* Spec 3: + Occupation FE + Lagged Controls
quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
    if ((year_eff_ban == 2008 & hw_ban == 1) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s3
estimates save "`sterdir'/wage_hw2008_s3.ster", replace

* Export across-specs table
esttab wage_s1 wage_s2 wage_s3 using "`tabdir'/hw2008_wage.csv", ///
    replace label nogaps compress nonotes ///
    keep(treated_eff) ///
    b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
          labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
    title("HW 2008 - Real income (treated_eff)")

********************************************************************************
* HW 2020 | WAGE OUTCOMES (Unstratified)
********************************************************************************
eststo clear

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year) vce(cluster statefip)
estimates store wage_s1
estimates save "`sterdir'/wage_hw2020_s1.ster", replace

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s2
estimates save "`sterdir'/wage_hw2020_s2.ster", replace

quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
    if ((year_eff_ban == 2020 & hw_ban == 1) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s3
estimates save "`sterdir'/wage_hw2020_s3.ster", replace

esttab wage_s1 wage_s2 wage_s3 using "`tabdir'/hw2020_wage.csv", ///
    replace label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
          labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
    title("HW 2020 - Real income (treated_eff)")

********************************************************************************
* HW 2022 | WAGE OUTCOMES (Unstratified)
********************************************************************************
eststo clear

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year) vce(cluster statefip)
estimates store wage_s1
estimates save "`sterdir'/wage_hw2022_s1.ster", replace

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s2
estimates save "`sterdir'/wage_hw2022_s2.ster", replace

quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
    if ((year_eff_ban == 2022 & hw_ban == 1) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s3
estimates save "`sterdir'/wage_hw2022_s3.ster", replace

esttab wage_s1 wage_s2 wage_s3 using "`tabdir'/hw2022_wage.csv", ///
    replace label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
          labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
    title("HW 2022 - Real income (treated_eff)")

********************************************************************************
* LW 2017 | WAGE OUTCOMES (Unstratified)
********************************************************************************
eststo clear

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year) vce(cluster statefip)
estimates store wage_s1
estimates save "`sterdir'/wage_lw2017_s1.ster", replace

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s2
estimates save "`sterdir'/wage_lw2017_s2.ster", replace

quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
    if ((year_eff_ban == 2017 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s3
estimates save "`sterdir'/wage_lw2017_s3.ster", replace

esttab wage_s1 wage_s2 wage_s3 using "`tabdir'/lw2017_wage.csv", ///
    replace label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
          labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
    title("LW 2017 - Real income (treated_eff)")

********************************************************************************
* LW 2018 | WAGE OUTCOMES (Unstratified)
********************************************************************************
eststo clear

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year) vce(cluster statefip)
estimates store wage_s1
estimates save "`sterdir'/wage_lw2018_s1.ster", replace

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s2
estimates save "`sterdir'/wage_lw2018_s2.ster", replace

quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
    if ((year_eff_ban == 2018 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s3
estimates save "`sterdir'/wage_lw2018_s3.ster", replace

esttab wage_s1 wage_s2 wage_s3 using "`tabdir'/lw2018_wage.csv", ///
    replace label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
          labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
    title("LW 2018 - Real income (treated_eff)")

********************************************************************************
* LW 2019 | WAGE OUTCOMES (Unstratified)
********************************************************************************
eststo clear

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year) vce(cluster statefip)
estimates store wage_s1
estimates save "`sterdir'/wage_lw2019_s1.ster", replace

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s2
estimates save "`sterdir'/wage_lw2019_s2.ster", replace

quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
    if ((year_eff_ban == 2019 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s3
estimates save "`sterdir'/wage_lw2019_s3.ster", replace

esttab wage_s1 wage_s2 wage_s3 using "`tabdir'/lw2019_wage.csv", ///
    replace label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
          labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
    title("LW 2019 - Real income (treated_eff)")

********************************************************************************
* LW 2020 | WAGE OUTCOMES (Unstratified)
********************************************************************************
eststo clear

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year) vce(cluster statefip)
estimates store wage_s1
estimates save "`sterdir'/wage_lw2020_s1.ster", replace

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s2
estimates save "`sterdir'/wage_lw2020_s2.ster", replace

quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
    if ((year_eff_ban == 2020 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s3
estimates save "`sterdir'/wage_lw2020_s3.ster", replace

esttab wage_s1 wage_s2 wage_s3 using "`tabdir'/lw2020_wage.csv", ///
    replace label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
          labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
    title("LW 2020 - Real income (treated_eff)")

********************************************************************************
* LW 2021 | WAGE OUTCOMES (Unstratified)
********************************************************************************
eststo clear

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year) vce(cluster statefip)
estimates store wage_s1
estimates save "`sterdir'/wage_lw2021_s1.ster", replace

quietly reghdfe incwage_r treated_eff ///
    if ((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s2
estimates save "`sterdir'/wage_lw2021_s2.ster", replace

quietly reghdfe incwage_r treated_eff c.inc_pcap_r_l1 c.hpi_r_l1 c.employment_sa_l1 ///
    if ((year_eff_ban == 2021 & hw_ban == 0) | missing(hw_ban)) ///
    [pweight=perwt], absorb(statefip year occ1990) vce(cluster statefip)
estimates store wage_s3
estimates save "`sterdir'/wage_lw2021_s3.ster", replace

esttab wage_s1 wage_s2 wage_s3 using "`tabdir'/lw2021_wage.csv", ///
    replace label nogaps compress nonotes ///
    keep(treated_eff) b(%9.3f) se(%9.3f) star(* 0.10 ** 0.05 *** 0.01) ///
    stats(N N_clust r2 r2_a, fmt(%9.0f %9.0f %9.3f %9.3f) ///
          labels("Obs." "Clusters" "R^2" "Adj. R^2")) ///
    addnotes("All models use [pweight=perwt] and cluster by statefip.") ///
    title("LW 2021 - Real income (treated_eff)")

log close
