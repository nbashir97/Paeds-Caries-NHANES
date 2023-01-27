**************************************************
*** NHANES 2011 - 2020 Pediatric Caries Trends ***
**************************************************

*** This do-file contains the Stata code for epidemiological analysis on trends in caries in the US pediatric population ***
*** Primary outcome is to present trends in the prevalence of dental caries in the US pediatric population for the 10-year period 2011 to 2020 ***

*** Use the file as appropriate for the NHANES cycle which you would like to analyse ***

use "{User directory}\NHANES_2011.dta", clear
use "{User directory}\NHANES_2013.dta", clear

****************************************************************************************************

*** Section 1: Cleaning Dataset ***
** New variables are defined for each socio-demographic characteristic and BMI **

** Age

generate primary_age = 1 if(ridageyr >= 2 & ridageyr <= 5)
replace primary_age = 2 if(ridageyr >= 6 & ridageyr <= 11)

replace primary_age = 1 if(ridagemn == 24)

label define prim_lab 1 "2-5" 2 "6-11"
label values primary_age prim_lab

generate perm_age = 1 if(ridageyr >= 6 & ridageyr <= 8)
replace perm_age = 2 if(ridageyr >= 9 & ridageyr <= 11)

label define perm_lab 1 "6-8" 2 "9-11"
label values perm_age perm_lab

** Sex

generate sex = riagendr

label define sex_lab 1 "Boys" 2 "Girls"
label values sex sex_lab

** Ethnicity

generate ethnicity = 1 if(ridreth3 == 3)
replace ethnicity = 2 if(ridreth3 == 4)
replace ethnicity = 3 if(ridreth3 == 1 | ridreth3 == 2)
replace ethnicity = 4 if(ridreth3 == 6)
replace ethnicity = 5 if(ridreth3 == 7)

label define eth_lab 1 "White" 2 "Black" 3 "Hispanic" 4 "Asian" 5 "Other"
label values ethnicity eth_lab

** Income to Poverty Ratio

generate fpl = 0 if(indfmpir < 0.5)
replace fpl = 1 if(indfmpir >= 0.5 & indfmpir < 1.0)
replace fpl = 2 if(indfmpir >= 1.0 & indfmpir < 2.5)
replace fpl = 3 if(indfmpir >= 2.5 & indfmpir < 4.0)
replace fpl = 4 if(indfmpir >= 4.0 & indfmpir !=. )

label define fpl_lab 0 "<0.5" 1 "0.5" 2 "1.0" 3 "2.5" 4 "4.0"
label values fpl fpl_lab

** BMI

generate bmi = 1 if(bmdbmic == 1)
replace bmi = 2 if(bmdbmic == 2)
replace bmi = 3 if(bmdbmic == 3)
replace bmi = 4 if(bmdbmic == 4)

label define bmi_lab 1 "Underweight" 2 "Normal" 3 "Overweight" 4 "Obese"
label values bmi bmi_lab

****************************************************************************************************

*** Section 2: Oral Health ***
** New variables are defined according to various aspects of the oral health examination **

** Complete exam
* Those who have received a complete examination are identified

generate examined = 1 if(ohdexsts == 1)

** Teeth present
* The number of teeth present are calculated

forvalues i = 02/31 {
	if inlist(`i', 16, 17) continue
	local I : di %02.0f `i'
	foreach var of varlist ohx`I'tc {
			generate ohx`I'pres = 1 if(`var' == 1 | `var' == 2)
			recode ohx`I'pres(. = 0)
	}
}

egen teeth = rowtotal(ohx02pres-ohx31pres)

** Decayed teeth

* Primary dentition

forvalues i = 02/31 {
	if inlist(`i', 16, 17) continue
	local I : di %02.0f `i'
	generate ohx`I'prim_d = 0
	foreach var of varlist ohx`I'ctc {
			replace ohx`I'prim_d = 1 if(`var' == "K" & (strpos(ohx`I'csc, "0") | strpos(ohx`I'csc, "1") | strpos(ohx`I'csc, "2") | strpos(ohx`I'csc, "3") | strpos(ohx`I'csc, "4")))
	}
}


egen prim_total_decay = rowtotal(ohx02prim_d-ohx31prim_d)

* Permanent dentition

forvalues i = 02/31 {
	if inlist(`i', 16, 17) continue
	local I : di %02.0f `i'
	generate ohx`I'perm_d = 0
	foreach var of varlist ohx`I'ctc {
			replace ohx`I'perm_d = 1 if(`var' == "Z" & (strpos(ohx`I'csc, "0") | strpos(ohx`I'csc, "1") | strpos(ohx`I'csc, "2") | strpos(ohx`I'csc, "3") | strpos(ohx`I'csc, "4")))
	}
}

egen perm_total_decay = rowtotal(ohx02perm_d-ohx31perm_d)

** Filled teeth

* Primary dentition

forvalues i = 02/31 {
	if inlist(`i', 16, 17) continue
	local I : di %02.0f `i'
	generate ohx`I'prim_f = 0
	foreach var of varlist ohx`I'ctc {
			replace ohx`I'prim_f = 1 if(`var' == "K" & (strpos(ohx`I'csc, "5") | strpos(ohx`I'csc, "6") | strpos(ohx`I'csc, "7") | strpos(ohx`I'csc, "8") | strpos(ohx`I'csc, "9")))
	}
}

egen prim_total_fill = rowtotal(ohx02prim_f-ohx31prim_f)

* Permanent dentition

forvalues i = 02/31 {
	if inlist(`i', 16, 17) continue
	local I : di %02.0f `i'
	generate ohx`I'perm_f = 0
	foreach var of varlist ohx`I'ctc {
			replace ohx`I'perm_f = 1 if(`var' == "Z" & (strpos(ohx`I'csc, "5") | strpos(ohx`I'csc, "6") | strpos(ohx`I'csc, "7") | strpos(ohx`I'csc, "8") | strpos(ohx`I'csc, "9")))
	}
}

egen perm_total_fill = rowtotal(ohx02perm_f-ohx31perm_f)

** Prevalence variable

generate prim_decay = 1 if(prim_total_decay > 0)
replace prim_decay = 0 if(prim_total_decay == 0)

generate perm_decay = 1 if(perm_total_decay > 0)
replace perm_decay = 0 if(perm_total_decay == 0)

generate prim_fill = 1 if(prim_total_fill > 0)
replace prim_fill = 0 if(prim_total_fill == 0)

generate perm_fill = 1 if(perm_total_fill > 0)
replace perm_fill = 0 if(perm_total_fill == 0)

label define disease 0 "Absent" 1 "Present"

foreach var of varlist prim_decay-perm_fill {
	label values `var' disease
}

****************************************************************************************************

*** Section 3: Data Analysis ***
** This section documents the formal statistical analyses **

** Weights
* Probability sampling weights, primary sampling units, and strata are provided (wtmecprp renamed to wtmec2yr in NHANES 2017-2020 dataset)

svyset [w = wtmec2yr], psu(sdmvpsu) strata(sdmvstra)

** Specifying cohort
* Those with no missing socio-demographic data (except age), a full examination, and at least one tooth present are identified

generate inAnalysis = 1
replace inAnalysis = 0 if teeth == 0

foreach var of varlist sex-examined{
	replace inAnalysis = . if(`var' == .)
}

** Unweighted counts
* These are the raw counts of included parcipants

foreach var in primary_age sex ethnicity fpl bmi {
	tabulate `var' if(inAnalysis == 1 & primary_age != .)
}

** Weighted counts
* These are the probability weighted counts of included participants

foreach var in primary_age sex ethnicity fpl bmi {
	svy, subpop(if inAnalysis == 1 & primary_age != .): tabulate `var', count format(%11.3g)
}

foreach var in primary_age sex ethnicity fpl bmi {
	svy, subpop(if inAnalysis == 1 & primary_age != .): proportion `var'
}

** Prevalence

* Decayed teeth, primary dentition

svy, subpop(if inAnalysis == 1 & primary_age != .): proportion prim_decay

foreach var in primary_age sex ethnicity fpl bmi {
	svy, subpop(if inAnalysis == 1 & primary_age != .): proportion prim_decay, over(`var')
}

* Filled teeth, primary dentition

svy, subpop(if inAnalysis == 1 & primary_age != .): proportion prim_fill

foreach var in primary_age sex ethnicity fpl bmi {
	svy, subpop(if inAnalysis == 1 & primary_age != .): proportion prim_fill, over(`var')
}

* Decayed teeth, permanent dentition

svy, subpop(if inAnalysis == 1 & perm_age != .): proportion perm_decay

foreach var in perm_age sex ethnicity fpl bmi {
	svy, subpop(if inAnalysis == 1 & perm_age != .): proportion perm_decay, over(`var')
}

* Filled teeth, permanent dentition

svy, subpop(if inAnalysis == 1 & perm_age != .): proportion perm_fill

foreach var in perm_age sex ethnicity fpl bmi {
	svy, subpop(if inAnalysis == 1 & perm_age != .): proportion perm_fill, over(`var')
}

****************************************************************************************************
