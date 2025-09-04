


********************************************************************************
* TCR indicator  
********************************************************************************


* Editing coliform indicator 

use "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\Flooding and Drinking Water\data\AP_coliform_v2.dta" , clear

drop dam_property injuries deaths flood flash_flood heavy_rain flood_21d flood_10d flood_3d flash_flood_21d flash_flood_10d flash_flood_3d heavy_rain_21d heavy_rain_10d heavy_rain_3d both both_3d both_10d both_21d tmean precip_mean precip_sum l1tmean l1precip_mean l2tmean l2precip_mean l3tmean l3precip_mean l4tmean l4precip_mean l5tmean l5precip_mean l6tmean l6precip_mean l7tmean l7precip_mean f1precip_mean f2precip_mean f3precip_mean precip_l14 precip_l13 precip_l12 precip_l11 precip_l10 precip_l9 precip_l8 precip_l7 precip_l6 precip_l5 precip_l4 precip_l3 precip_l2 precip_l1 precip_l0 precip_f1 precip_f2 precip_f3 precip_f4 precip_f5 precip_f6 precip_f7 precip_f8 precip_f9 precip_f10 precip_f11 precip_f12 precip_f13 precip_f14 precip_3d precip_7d source_sdwa system_size population_served_count service_connections pop_served med_inc frac_white frac_black frac_amerind frac_asian frac_pacisl frac_hisp frac_pov50 month sample_type sampleid analytename facility_id samplingpointtype sample_point_id federal_type source facility_type fips

drop if year >2019 | year <2006

gen result_syr4 = result if year >2012
gen result_2016 = result if year >2015

gen sample_count = 1

*collapse (mean) result , by(pwsid facility_id)
collapse (mean) result_syr4 result_2016 result (sum) sample_count , by(pwsid)


replace pwsid = trim(pwsid)

gen state = substr(pwsid, 1 ,2 )
tab state if result >0.75 & result!=.

* check results by state
cap drop too_large too_large_share
gen too_large = ( result >0.5 & result!=.)
bys state: egen too_large_share = mean(too_large)
bys state: tab too_large_share

* check averages by state 
bys state: egen mean = mean(result)
bys state: tab mean

cap drop too_large too_large_share 
cap drop mean 

* drop unreasonable values that are likely just reports related to violations 
foreach var in result result_syr4 result_2016 {
	replace `var' = . if `var' > 0.5 & `var' != . 
}
drop if state == "SC" | state == "MD"

rename result detection_share
rename result_syr4  detection_share_syr4 
rename result_2016 detection_share_2016

********************************************************************************
* Raw SYR data 
********************************************************************************

preserve
import delimited "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicator_tcr_v1.csv", varnames(1) clear 

* clean variables
replace pwsid = trim(pwsid)

* remove duplicates 
egen tag = tag(pwsid)
tab tag
keep if tag == 1 

* rename variable so it doesn't overwrite 
rename detection_share detection_share_syr
destring detection_share_syr , replace ignore("NA")

tempfile alt
save `alt'
restore
merge 1:1 pwsid using `alt'
drop tag 
drop _merge 

gen state = substr(pwsid, 1, 2)
tabstat detection_share_syr4 detection_share_2016 detection_share sample_count , by(state)
drop state 

order pwsid detection_share detection_share_syr detection_share_syr4 detection_share_2016 sample_count

export delimited using "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\data\indicators\indicator_tcr_v2.csv", replace

