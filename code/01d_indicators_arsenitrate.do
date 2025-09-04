
********************************************************************************
* Creating indicators for arsenic and nitrate using the SYR3 and SYR4 data
* last edited: 5/10/24
********************************************************************************


/*Table of Contents:

	- Load arsenic data from SYR3 and SYR4
	- combine arsenic samples with webscraped data to ensure more complete coverage 
	- Load nitrate data from SYR3 and SYR4 
	- combine nitrate samples with webscraped data to ensure more complete coverage 

*/

********************************************************************************
* Arsenic  
********************************************************************************



* SYR3 - arsenic

import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\6yr review\processed\syr3_arsenic.csv" , clear

* varlist analyte_code analyte_name state_code pws_id system_name system_type retail_population_served adjusted_total_population_served source_water_type water_facility_id water_facility_type sampling_point_id sampling_point_type source_type_code sample_type_code laboratory_assigned_id six_year_id sample_id sample_collection_date detection_limit_value detection_limit_unit detection_limit_code detect result_value unit presence_indicator_code residual_field_free_chlorine_mg_ residual_field_total_chlorine_mg

rename (pws_id result_value) (pwsid value)
replace unit = trim(unit)
replace pwsid = trim(pwsid)
destring value, replace ignore("NA") 
tab unit /*units are all mg/l*/
tempfile syr3_arsenic
save `syr3_arsenic'


* SYR4

import delimited "C:\Users\GAUSTIN\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\6yr review\SYR4\updated_files\SUMMARY_ANALYTE_ARSENIC.txt", clear 
append using `syr3_arsenic' , force /*combine SYR3 and SYR4*/

* drop raw/ unfinished water samples
drop if sampling_point_type == "RW"
drop if sampling_point_type == "SR"
drop if source_type_code == "RW"
drop if source_type_code == "UN"

* standardize units to mg/l
tab unit 
destring value, replace ignore("NA")
replace value = value / 1000 if unit == "UG/L"
replace unit = "mg/l" if unit == "UG/L"
replace value= 0 if value == . 
replace pwsid = trim(pwsid)
gen syr = 1 

keep pwsid system_type value syr sample_id unit sample_collection_date
tostring sample_id, replace

tempfile syrs_ars
save `syrs_ars'



* Webscraped samples for systems that are not in the Six Year Review 

use "C:\Users\gaustin\Box\Arsenic retrospective\Data\Drinking Water\scraped_arsenic_samples.dta" , clear

gen value = real(concentration)
replace concentration_uom = trim(concentration_uom)
replace value = 0 if value == . 
replace value = value / 1000 if concentration_uom == "Â UG/L"
replace value = value / 1000 if concentration_uom == "UG/L"
replace concentration_uom = "mg/l" if concentration_uom == "UG/L" | concentration_uom == "Â UG/L"

* dropping five obs with weird UOMs
drop if concentration_uom =="MFL" | concentration_uom =="PH" | concentration_uom == "PCI/L"  | concentration_uom =="Â PH"

* Limit to same years as SYR 
gen year = substr(sample_date, -4, .)
destring year, replace 
keep if year>=2006 & year <2020


* drop outliers that are likely implausible values  

replace pwsid = trim(pwsid)

* rename variables and standardize 

gen syr = 0 
rename 	( federal_type concentration_uom sample_date ) /// 
		( system_type unit sample_collection_date )
		
keep pwsid system_type value syr sample_id unit sample_collection_date


append using `syrs_ars'

bys pwsid : egen in_syr = total(syr)
replace in_syr = 1 if in_syr >1& in_syr !=.
drop if syr == 0 & in_syr ==1 /*retaining just samples not already in SYR */
drop in_syr 

* merge with list of PWSs with service areas 

drop system_type 
merge m:1 pwsid using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\data\pws_list.dta"
keep if _merge == 3 
drop _merge

* save temp file of all samples 

export delimited using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\arsenic_samples.csv", replace

* Create system-level indicator 

collapse (mean) value, by(pwsid )

* Drop extreme outlier systems 
drop if value >150 /*none after filtering to CWS*/
rename value arsenic

export delimited using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicator_arsenic_v2.csv", replace





********************************************************************************
********************************************************************************
* Nitrates 
********************************************************************************
********************************************************************************


import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\6yr review\processed\syr3_nitrate.csv" , clear
rename (pws_id result_value) (pwsid value)
cap destring value, replace ignore("NA")
tab unit 
replace pwsid = trim(pwsid)
tempfile syr3_nitrate
save `syr3_nitrate'


import delimited "C:\Users\GAUSTIN\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\6yr review\SYR4\updated_files\SUMMARY_ANALYTE_NITRATE-NITRITE.txt" , clear 
cap destring value, replace ignore("NA")
tab unit
replace pwsid = trim(pwsid)
tempfile syr4_nitnat
save `syr4_nitnat'


import delimited "C:\Users\GAUSTIN\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\6yr review\SYR4\updated_files\SUMMARY_ANALYTE_NITRATE.txt", clear 
cap destring value, replace ignore("NA")
append using `syr4_nitnat' , force 
append using `syr3_nitrate' , force 
replace pwsid = trim(pwsid)


* drop raw/ unfinished water samples
drop if sampling_point_type == "RW"
drop if sampling_point_type == "SR"
drop if source_type_code == "RW"
drop if source_type_code == "UN"


tab unit 
replace unit = trim(unit)
replace value= 0 if value == . 
replace value = value / 1000 if unit == "UG/L"
replace unit = "mg/l" if unit == "UG/L" 

* drop nitrate/nitrite samples if nitrate exists
gen nitrate = (analyte_name == "NITRATE")
bys pwsid : egen total= total(nitrate) 
replace total = 1 if total >1 & total!=.
drop if analyte_name == "NITRATE-NITRITE" & total ==1
drop total 

gen syr =1 
tostring sample_id , replace 
keep pwsid system_type analyte_name value syr sample_id unit sample_collection_date
drop if value > 500 & value !=. 
/*five values dropped, only one is an actual CWS*/

tempfile syr_nitrates
save `syr_nitrates'



* Fill in missing systems with web-scraped data where relevant 

use "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\DWDB\Data\exports\nitrates.dta", clear
gen value = real(concentration)

* drop raw water and other non-potable sources
drop if water_type =="RW" | water_type == "R"
drop if activity_status == "AG" | activity_status == "WW" | activity_status == "DS" | ///
		activity_status == "AR" | activity_status == "SU" | activity_status == "SR" | ///
		activity_status == "AB" | activity_status == "DS" | activity_status == "IU" | ///
		activity_status == "IR" | activity_status == "AR" | activity_status == "AU" 


* convert to mg/l
tab concentration_uom
replace concentration_uom = subinstr(concentration_uom, " ", "", .)
replace concentration_uom = trim(concentration_uom)
* dropping <20 obs with UOMs that are likely incorrect
drop if concentration_uom =="%LUM" | concentration_uom =="CM-1" | concentration_uom == "MFL"  | concentration_uom =="P800" | concentration_uom == "PCI/L" | concentration_uom == "PH" | concentration_uom =="NG/L" | concentration_uom =="null null"
tab concentration_uom /*for some reason MG and UG still showing up twice*/

generate unit = "UG/L" if strpos(concentration_uom, "UG/L") > 0
replace unit = "MG/L" if strpos(concentration_uom, "MG/L") > 0

replace value = 0 if value == . 
replace value = value /1000 if unit == "UG/L"
replace unit = "MG/L" if unit == "UG/L" 


drop year 
gen year = substr(sample_date, -4, .)
destring year, replace 
keep if year>=2006 & year <2020

* drop outliers that are likely implausible values  
replace pwsid = trim(pwsid)

* drop nitrate/nitrite samples if nitrate exists
gen nitrate = (analytename == "NITRATE")
bys pwsid : egen total= total(nitrate) 
replace total = 1 if total >1 & total!=.
drop if analytename == "NITRATE-NITRITE" & total ==1

drop total 

* rename variables and standardize 

gen syr = 0 
rename 	( federal_type sample_date analytename ) /// 
		( system_type sample_collection_date analyte_name )
		
keep pwsid system_type analyte_name value syr sample_id unit sample_collection_date

append using `syr_nitrates'

bys pwsid : egen in_syr = total(syr)
replace in_syr = 1 if in_syr >1& in_syr !=.
drop if syr == 0 & in_syr ==1 /*retaining just samples for systems not already in SYR */
drop in_syr 

* merge with list of PWSs with service areas 

drop system_type 
merge m:1 pwsid using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\data\pws_list.dta"
keep if _merge == 3 
drop _merge

* Drop extreme outlier samples 
drop if value >500 & value ! = .  /*2 systems, these are nitrate-nitrite samples */
rename value nitrate

* save temp file of all samples 

export delimited using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\nitrate_samples.csv", replace

* Create system-level indicator 

collapse (mean) nitrate , by(pwsid )

export delimited using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicator_nitrate_v2.csv", replace


