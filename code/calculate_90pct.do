********************************************************************************
* File to calculate 90th percentile values for each demographic group and each drinking water indicator 
	* amd also the medians
* National Center for Environmental Economics 
* Last edited: 10/28/2024
********************************************************************************


global path C:\Users\gaustin\OneDrive - Environmental Protection Agency (EPA)\NCEE - Water System Service Boundaries\data\



********************************************************************************
* Create 90th percentile versions 
********************************************************************************



clear
foreach indicator in hb nitrate ars tcr dbp pfas lcr{
preserve
import delimited "${path}risk_by_pws\risk_hm_`indicator'.csv", bindquote(strict) clear 

* rename variables to use the same string for all variable / file procedures
cap rename arsenic ars 
cap rename total_violations hb 
cap rename detection_share tcr 
cap rename combined_dbp dbp 
cap rename concentration_avg pfas 
cap rename pb_vio_count lcr 

* drop unnecessary variables 

keep 	pwsid ///	
		pop_served minor_served amer_ind_served asian_served black_served hispanic_served pac_isl_served nhw_served 		highinc_served lowinc_served ///
		`indicator'  

* convert to numeric variables 		
destring `indicator'  , replace ignore("NA")
drop if `indicator'   == . 	
	
* Calculate 90th percentile by group 	
foreach var in pop_served minor_served amer_ind_served asian_served black_served hispanic_served pac_isl_served nhw_served highinc_served lowinc_served{
		
	sort `indicator' /*sort from lowest to highest water quality value*/
	gen cumulative = sum(`var') /*generate cumulative population served for each demographic group (this computes the total population served of all systems with better water quality for each row)*/
	egen total = sum(`var') /*total population served by demographic */
	gen pctile= cumulative / total /*share of population served */
	gen tmp_90 = `indicator' if pctile >= .90 /*cut off water quality variable at the 90th percentile*/
	egen pct90_`var' = min( tmp_90 ) /*pick the lowest water quality value above at least the 90th percentile  */
	drop cumulative total pctile tmp_90
	}


keep  	pct90_pop_served pct90_minor_served pct90_amer_ind_served ///
		pct90_asian_served pct90_black_served pct90_hispanic_served ///
		pct90_pac_isl_served pct90_nhw_served pct90_highinc_served pct90_lowinc_served

gen indicator = "`indicator'" /*generate a column for the indicator */
		
duplicates drop 
tempfile f_`indicator'
save `f_`indicator''
restore
append using `f_`indicator''

	
}


* compute RRs

foreach var in  pct90_minor_served pct90_amer_ind_served pct90_asian_served ///
				pct90_black_served pct90_hispanic_served pct90_pac_isl_served {
					gen rr_`var' = `var' / pct90_nhw_served
	
}

gen rr_income =  pct90_lowinc_served / pct90_highinc_served
	
order indicator 

save "${path}risk_90pctile\dem_all_90pct.dta" , replace
	
	
	
	
	
	
********************************************************************************
* Create 50th or 75th percentiles by dem 
********************************************************************************


clear
foreach indicator in hb nitrate ars tcr dbp pfas lcr{
preserve
import delimited "${path}risk_by_pws\risk_hm_`indicator'.csv", bindquote(strict) clear 

* rename variables to use the same string for all variable / file procedures
cap rename arsenic ars 
cap rename total_violations hb 
cap rename detection_share tcr 
cap rename combined_dbp dbp 
cap rename concentration_avg pfas 
cap rename pb_vio_count lcr 

* drop unnecessary variables 

keep 	pwsid ///	
		pop_served minor_served amer_ind_served asian_served black_served hispanic_served pac_isl_served nhw_served 		highinc_served lowinc_served ///
		`indicator'  

* convert to numeric variables 		
destring `indicator'  , replace ignore("NA")
drop if `indicator'   == . 	
	
* Calculate 50th percentile by group 	
foreach var in pop_served minor_served amer_ind_served asian_served black_served hispanic_served pac_isl_served nhw_served highinc_served lowinc_served{
		
	sort `indicator' /*sort from lowest to highest water quality value*/
	gen cumulative = sum(`var') /*generate cumulative population served for each demographic group (this computes the total population served of all systems with better water quality for each row)*/
	egen total = sum(`var') /*total population served by demographic */
	gen pctile= cumulative / total /*share of population served */
	gen tmp_90 = `indicator' if pctile >= .75 /*cut off water quality variable at the 90th percentile*/
	egen pct90_`var' = min( tmp_90 ) /*pick the lowest water quality value above at least the 90th percentile  */
	drop cumulative total pctile tmp_90
	}


keep  	pct90_pop_served pct90_minor_served pct90_amer_ind_served ///
		pct90_asian_served pct90_black_served pct90_hispanic_served ///
		pct90_pac_isl_served pct90_nhw_served pct90_highinc_served pct90_lowinc_served

gen indicator = "`indicator'" /*generate a column for the indicator */
		
duplicates drop 
tempfile f_`indicator'
save `f_`indicator''
restore
append using `f_`indicator''

	
}


* compute RRs

foreach var in  pct90_minor_served pct90_amer_ind_served pct90_asian_served ///
				pct90_black_served pct90_hispanic_served pct90_pac_isl_served {
					gen rr_`var' = `var' / pct90_nhw_served
	
}

gen rr_income =  pct90_lowinc_served / pct90_highinc_served
	
order indicator 

save "${path}risk_90pctile\dem_all_75pct.dta" , replace	



* These are often just missing bc the denominator is 0. So probably not as interesting to use. 

