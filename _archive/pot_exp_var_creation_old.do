* CREATING YEARS SCHOOLING AND POTENTIAL EXPERIENCE VARIABLES ------------------

* Create years of schooling variable 
gen yrschool = .
		
* No schooling
replace yrschool = 0 if inlist(educd, 0, 1, 2, 11, 12)

* Grade 1-4
*replace yrschool = 4 if educd == 10 // nursery school to grade 4
*replace yrschool = 4 if educd == 13 // grade 1, 2, 3, or 4
replace yrschool = 1 if educd == 14 // grade 1 
replace yrschool = 2 if educd == 15 // grade 2
replace yrschool = 3 if educd == 16 // grade 3
replace yrschool = 4 if educd == 17 // grade 4 

* Grade 5-8
*replace yrschool = 8 if educd == 20 // grade 5, 6, 7, or 8
*replace yrschool = 6 if educd == 21 // grade 5 or 6
replace yrschool = 5 if educd == 22 // grade 5 
replace yrschool = 6 if educd == 23 // grade 6
*replace yrschool = 8 if educd == 24 // grade 7 or 8
replace yrschool = 7 if educd == 25 // grade 7
replace yrschool = 8 if educd == 26 // grade 8

* High school 
replace yrschool = 9 if educd == 30 // grade 9
replace yrschool = 10 if educd == 40 // grade 10
replace yrschool = 11 if educd == 50 // grade 11
replace yrschool = 12 if educd == 60 // grade 12 
replace yrschool = 12 if inrange(educd, 61, 64) // "12th grade, no diploma" etc.

* College 
replace yrschool = 13 if inlist(educd, 65, 70) // "some college, but less than 1 year" etc.
*replace yrschool = 16 if educd == 71 // 1 or more years of college credit, no degree
replace yrschool = 14 if educd == 80 // 2 years of college 
replace yrschool = 14 if inlist(educd, 81, 82, 83) // associate's degree 
replace yrschool = 15 if educd == 90 // 3 years of college 
replace yrschool = 16 if inlist(educd, 100, 101) // 4 years of college OR bachelor's degree

* Graduate 
*replace yrschool = 18 if educd == 110 // 5+
replace yrschool = 18 if educd == 111 // 6
replace yrschool = 19 if educd == 112 // 7 
*replace yrschool = 20 if educd == 113 // 8+
replace yrschool = 18 if educd == 114 // masters degree 
replace yrschool = 19 if educd == 115 // professional degree beyond bachelors 
replace yrschool = 20 if educd == 116 // doctoral 
label variable yrschool "years of school"

* Potential Experience Variable 
gen pot_exp = age - yrschool - 6 // assumes start school at age 6
replace pot_exp = 0 if pot_exp < 0 // truncate at 0 
label variable pot_exp "potential experience"	


* DROP OBS W/ MISSING VALUES OF POT. EXP. --------------------------------------

* DROP B/C UNCLEAR # OF YEARS OF SCHOOL: 
	*nursery school to grade 4
	*grade 5 or 6
	*grade 7 or 8
	*1 or more years of college credit, no degree
* Dropping where yrschool is missing will take care of this because these are 
* the values where it is missing. 

*drop if missing(yrschool)
	// (1,074,901 observations deleted)

*save "nca_acs_soc.dta", replace

* OBSERVATIONS AT THIS POINT: 7,237,520

