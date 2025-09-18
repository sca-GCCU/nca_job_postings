cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "nca_acs_soc_1pct.log", replace 

clear all 

* 1) Load full dataset 

use nca_acs_soc, clear 

local drop_vars year_enact_ban month_enact_ban ban_coverage multi_leg ///
	multi_leg_year treated_enact month_eff_ban

drop `drop_vars'

* 2) Draw a 1% random sample (uniform over rows)

set seed 8924

sample 1, by(year year_eff_ban)

* 3) Save tiny dataset to use in R

save "nca_acs_soc_1pct.dta", replace 


log close 