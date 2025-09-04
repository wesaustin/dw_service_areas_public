********************************************************************************
********************************************************************************
* Creating Figures for EJ & Service Areas PAper 
* National Center for Environmental Economics 
* Last Edited: 6/28/2024
********************************************************************************
********************************************************************************


********************************************************************************
* I. Initialize Program - Set Paths and Install Packages 
********************************************************************************

global path "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\" 
global plus "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\code\packages\"
global output "C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\ej_service_areas\output\"
sysdir set PLUS "${plus}"
sysdir set PERSONAL "${plus}"
net install scheme-modern, from("https://raw.githubusercontent.com/mdroste/stata-scheme-modern/master/")
set scheme modern, perm 
graph set window fontface "Times New Roman"

* ssc install colorpallette
grstyle init
grstyle set color hue, n(5) opacity(75)

	
********************************************************************************
* Load RRs at the average and RRs at the 90th percentile 
********************************************************************************

import delimited "${path}ej_service_areas\data\rel_risk\rel_risk_all_subpop.csv" , clear 	

keep if boundary == "hm"
drop boundary  v1
gen type = "Average"
tempfile orig
save `orig'


import excel  "${path}data\risk_90pctile\dem_all_90pct_reshape.xlsx" , sheet("Sheet1") firstrow clear 
rename indicator race_cat
gen type = "90th Pct."
rename 	(hb nitrate ars tcr dbp pfas lcr) /// 
		(hb_rel_risk nitrate_rel_risk ars_rel_risk tcr_rel_risk dbp_rel_risk pfas_rel_risk lcr_rel_risk)

		append using `orig'
		drop if race_cat == "poc_risk"



********************************************************************************
* Histogram of relative risks by demographic group 
********************************************************************************


replace race_cat = "American Indian" if race_cat == "amer_ind"
replace race_cat = "Asian" if race_cat == "asian"
replace race_cat = "Black" if race_cat == "black"
replace race_cat = "Hispanic" if race_cat == "hisp"
replace race_cat = "Pacific Islander" if race_cat == "pac_isl"
cap replace race_cat = "Low-Income" if race_cat == "lowinc"

foreach var in hb nitrate ars tcr dbp pfas lcr{
	cap rename `var'_rel_risk risk_`var'
}


reshape long risk_  , i(race_cat  type ) j(indicator)	string

gen border = 1 if type == "Average"
replace border = 2 if type == "90th Pct."

gen rorder =1 if race_cat  == "American Indian" 
replace rorder =2 if  race_cat == "Asian" 
replace rorder =3 if  race_cat == "Black" 
replace rorder =4 if  race_cat == "Hispanic" 
replace rorder =5 if  race_cat == "Pacific Islander" 
replace rorder =6 if  race_cat == "Low-Income" 

gen ind = "Health-based SDWA Violations" if indicator == "hb"
replace ind = "PFAS" if indicator == "pfas"
replace ind = "DBPs" if indicator == "dbp"
replace ind = "Total Coliform" if indicator == "tcr"
replace ind = "Arsenic" if indicator == "ars"
replace ind = "Nitrate" if indicator == "nitrate"
replace ind = "Lead ALEs" if indicator == "lcr"

	
	* Looking at the avg and the 90th together 
	
	
		* Looking at each of them separately 
	
	foreach indicator in ars dbp hb lcr nitrate pfas tcr{
	if `"`indicator'"'=="hb"  {
		local i "Health-based SDWA Violations"
	}
	if `"`indicator'"'=="lcr"  {
		local i "Lead Action Level Exceedances"
	}
	if `"`indicator'"'=="pfas"  {
		local i "Total PFAS Concentration"
	}
	if `"`indicator'"'=="dbp"  {
		local i "Disinfection Byproducts"
	}
	if `"`indicator'"'=="tcr"  {
		local i "Total Coliform Detections"
	}
	if `"`indicator'"'=="ars"  {
		local i "Arsenic"
	}	
	if `"`indicator'"'=="nitrate"  {
		local i "Nitrate"
	}
	
		graph bar risk_ if indicator == "`indicator'" , over(type, sort(type)) ///
		over(race_cat, sort(rorder)  )  ///
		asyvars name(solo_avg__`indicator', replace) ///
		ytitle("Prevalence Ratio", size(large)) yline(1) ///
		title("")  ///
		legend(pos(6) cols(7) order(1 "Average" 2 "90th Percentile") size(large)) ///
		scheme(modern)	plotregion(margin(medium))    
		graph export "${output}bar charts\december\bar_`indicator'_dec.png" , as(png) replace
		 

		
	}
	
	
/*******************************************************************************
* Draft Code  
******************************************************************************** 

	graph bar hb_rel_risk nitrate_rel_risk ars_rel_risk tcr_rel_risk dbp_rel_risk pfas_rel_risk lcr_rel_risk  , over(border) over(race_cat, sort(rorder)) asyvars name(overtime, replace) ///
	ytitle("Disparity Measure") yline(1) ///
	legend(pos(6) cols(5) order(1 "County" 2 "Zipcode" 3 "USGS" 4 "EPIC" 5 "EPA ORD"))
	graph export "${output}bar_`var'_final.png" , as(png) replace
		title("All Measures by Race and Ethnicity") ///

	graph bar hb_rel_risk nitrate_rel_risk ars_rel_risk tcr_rel_risk dbp_rel_risk pfas_rel_risk lcr_rel_risk  if border ==2 , ///
	over(race_cat, sort(rorder)) asyvars name(overtime, replace) ///
	 ytitle("Disparity Measure") yline(1) ///
	legend(pos(6) cols(6)  ) ///
			plotregion(margin(medium)) 
	graph export "${output}bar_`var'_final.png" , as(png) replace
		title("All Measures at 90th Percentile by Race and Ethnicity") ///	 		 
	
	
		
		* Looking at all of them together 

graph bar risk_ if border ==1 , ///
	over(race_cat, sort(rorder)) over(indicator) asyvars name(g1, replace) ///
	 ytitle("Disparity Measure") yline(1) ///
	legend(pos(6) cols(6)  ) ///
			plotregion(margin(medium)) 
	*graph export "${output}bar_`var'_final.png" , as(png) replace
		*title("`i' by Race and Ethnicity") ///	 
		
		graph bar risk_ if border ==1 , ///
	over(indicator) over(race_cat, sort(rorder)) asyvars name(g2, replace) ///
	 ytitle("Disparity Measure") yline(1) ///
	legend(pos(6) cols(7)  ) ///
		 plotregion(margin(medium)) 
	*graph export "${output}bar_`var'_final.png" , as(png) replace
		*title("`i' by Race and Ethnicity") ///	 
		 
graph bar risk_ if border ==2 , ///
	over(race_cat, sort(rorder)) over(indicator) asyvars name(g3_90pct, replace) ///
	ytitle("Disparity Measure") yline(1) ///
	legend(pos(6) cols(6)  ) ///
			plotregion(margin(medium)) 
	*graph export "${output}bar_`var'_final.png" , as(png) replace
		*title("`i' by Race and Ethnicity") ///	 
		
		graph bar risk_ if border ==2 , ///
	over(indicator) over(race_cat, sort(rorder)) asyvars name(g4_90pct, replace) ///
	 ytitle("Disparity Measure") yline(1) ///
	legend(pos(6) cols(7)  ) ///
			plotregion(margin(medium)) 
	*graph export "${output}bar_`var'_final.png" , as(png) replace
		*title("`i' by Race and Ethnicity") ///	 
		 		 
				 
	* Looking at each of them separately 
	
	foreach indicator in ars dbp hb lcr nitrate pfas tcr{
	if `"`indicator'"'=="hb"  {
		local i "Health-based SDWA Violations"
	}
	if `"`indicator'"'=="lcr"  {
		local i "Lead Action Level Exceedances"
	}
	if `"`indicator'"'=="pfas"  {
		local i "Total PFAS Concentration (ppt)"
	}
	if `"`indicator'"'=="dbp"  {
		local i "Average DBP Concentration"
	}
	if `"`indicator'"'=="tcr"  {
		local i "Total Coliform Detection Share"
	}
	if `"`indicator'"'=="ars"  {
		local i "Arsenic Concentration (mg/l)"
	}	
	if `"`indicator'"'=="nitrate"  {
		local i "Nitrate Concentration (mg/l)"
	}
	
		graph bar risk_ if border ==1 & indicator == "`indicator'" , over(race_cat, sort(rorder)) ///
		asyvars name(solo_avg_`indicator', replace) ///
		ytitle("Disparity Measure") yline(1) ///
		legend(pos(6) cols(7)  ) ///
			plotregion(margin(medium)) 
		
		graph bar risk_ if border ==1 & indicator == "`indicator'" , over(race_cat, sort(rorder)) ///
		asyvars name(solo_90pct_`indicator', replace) ///
		ytitle("Disparity Measure") yline(1) ///
		legend(pos(6) cols(7)  ) ///
			plotregion(margin(medium)) 
		
	}
	
	gr combine solo_avg_hb solo_avg_lcr solo_avg_ars solo_avg_dbp solo_avg_nitrate solo_avg_pfas solo_avg_tcr
	gr combine solo_avg_hb solo_90pct_hb 		 
		 

	
	
	
	
		global xlabel "ml12_`dis' = "m-12" ml11_`dis' = "m-11" ml10_`dis' = "m-10" ml9_`dis' = "m-9" ml8_`dis' = "m-8" ml7_`dis' = "m-7" ml6_`dis' = "m-6" ml5_`dis'= "m-5" ml4_`dis' = "m-4" ml3_`dis' = "m-3" ml2_`dis'= "m-2" ml1_`dis' = "m-1" `dis' = "m"  mf1_`dis' = "m+1" mf2_`dis' = "m+2" mf3_`dis' = "m+3" mf4_`dis' = "m+4" mf5_`dis'= "m+5" mf6_`dis' = "m+6"  "
			
	
	rename(${xlabel})
		
		
		