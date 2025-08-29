cd "C:\Users\scana\OneDrive\Documents\research\projects\nca_job_postings\data"

log using "nca_acs_map.log", replace 

* CREATE A MAP OF STATES BY BAN-TYPE 

	* Ban data 
	
use "nca_laws_gks.dta", clear 

replace hw_ban = 0 if missing(hw_ban)

gen state_type = . 
replace state_type = 1 if full_ban == 0 & ban == 0 
replace state_type = 2 if full_ban == 0 & ban == 1 & hw_ban == 0
replace state_type = 3 if full_ban == 0 & ban == 1 & hw_ban == 1
replace state_type = 4 if full_ban == 1
label define state_type_labels 1 "no ban" 2 "low-wage ban" 3 "high-wage ban" ///
	4 "full ban"
label values state_type state_type_labels

save "nca_acs_map_dataset.dta", replace 


	* Map data v 1 
	
spshape2dta cb_2018_us_state_20m.shp, saving(usastates) replace 

use usastates.dta, clear 
gen statefip = real(GEOID)
save usastates.dta, replace 
list statefip _ID NAME if NAME == "Alaska" | NAME == "Hawaii" 
	
	// Alaska 26, Hawaii 49 

use usastates_shp.dta, clear 

drop if _X < -165 & _X != . & _ID == 49
replace _X = _X + 55 if _X != . & _ID == 49
replace _Y = _Y + 4 if _Y != . & _ID == 49

replace _X = _X*.4 - 55 if _X !=. & _ID == 26
replace _Y = _Y*.4 + 1 if _Y != . & _ID == 26
drop if _X > -10 & _X != . & _ID == 26 

save usastates_shp.dta, replace 

use _ID _CX _CY GEOID statefip using usastates.dta 
merge 1:1 statefip using nca_acs_map_dataset ///
	, keepusing(state state_type)

drop if _merge == 1 // Puerto Rico 
drop _merge 

save nca_ban_map.dta, replace 

	* Map generation 
	
ssc install geo2xy, replace 
ssc install palettes, replace 
ssc install colrspace, replace 
	
use nca_ban_map.dta, clear 
	
spmap state_type using usastates_shp, id(_ID) /// 
	fcolor(Blues2) ///
	clmethod(unique) ///
	legend(pos(4)) 
	
graph export "nca_ban_map.png", replace 

	
log close 

