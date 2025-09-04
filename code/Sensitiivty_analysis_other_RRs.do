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
	gen tmp_99 = `indicator' if pctile >= .99 /*cut off water quality variable at the 99th percentile, so dropping the top 1 %*/
	egen pct99_`var' = min( tmp_99 ) /*pick the lowest water quality value above at least the 90th percentile  */
	drop cumulative total pctile tmp_99
	}


keep  	pct99_pop_served pct99_minor_served pct99_amer_ind_served ///
		pct99_asian_served pct99_black_served pct99_hispanic_served ///
		pct99_pac_isl_served pct99_nhw_served pct99_highinc_served pct99_lowinc_served

gen indicator = "`indicator'" /*generate a column for the indicator */
		
duplicates drop 
tempfile f_`indicator'
save `f_`indicator''
restore
append using `f_`indicator''

	
}


* compute RRs

foreach var in  pct99_minor_served pct99_amer_ind_served pct99_asian_served ///
				pct99_black_served pct99_hispanic_served pct99_pac_isl_served {
					gen rr_`var' = `var' / pct99_nhw_served
	
}

gen rr_income =  pct99_lowinc_served / pct99_highinc_served
	
order indicator 

save "${path}risk_90pctile\june\dem_all_99pct.dta" , replace
	
	
	
	
	
	
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
	gen tmp_90 = `indicator' if pctile >= .50 /*cut off water quality variable at the 50th percentile*/
	egen pct50_`var' = min( tmp_90 ) /*pick the lowest water quality value above at least median  */
	drop cumulative total pctile tmp_90
	}


keep  	pct50_pop_served pct50_minor_served pct50_amer_ind_served ///
		pct50_asian_served pct50_black_served pct50_hispanic_served ///
		pct50_pac_isl_served pct50_nhw_served pct50_highinc_served pct50_lowinc_served

gen indicator = "`indicator'" /*generate a column for the indicator */
		
duplicates drop 
tempfile f_`indicator'
save `f_`indicator''
restore
append using `f_`indicator''

	
}


* compute RRs

foreach var in  pct50_minor_served pct50_amer_ind_served pct50_asian_served ///
				pct50_black_served pct50_hispanic_served pct50_pac_isl_served {
					gen rr_`var' = `var' / pct50_nhw_served
	
}

gen rr_income =  pct50_lowinc_served / pct50_highinc_served
	
order indicator 
gen type = "Median"

save "${path}risk_90pctile\june\dem_all_50pct.dta" , replace	





********************************************************************************
* Now Drop just the top 10 for each group 
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
		pop_served ///
		minor_served amer_ind_served asian_served black_served hispanic_served pac_isl_served nhw_served 		highinc_served lowinc_served ///
		`indicator'  

* convert to numeric variables 		
destring `indicator'  , replace ignore("NA")
drop if `indicator'   == . 	


* drop the top 10

gsort -`indicator'
gen rank = _n
drop if rank <=10 
	
* Calculate average by group 	
foreach var in pop_served minor_served amer_ind_served asian_served black_served hispanic_served pac_isl_served nhw_served highinc_served lowinc_served{
		cap drop products
	egen denominator_`var' = total(`var')
	gen products_`var' = `var' * `indicator'
	egen numerator_`var' = total(products_`var' )
	gen mean_`var' = numerator_`var' / denominator_`var'
	
	}

	


keep  	mean_pop_served mean_minor_served mean_amer_ind_served mean_asian_served mean_black_served mean_hispanic_served mean_pac_isl_served mean_nhw_served mean_highinc_served mean_lowinc_served

gen indicator = "`indicator'" /*generate a column for the indicator */
		
duplicates drop 
tempfile f_`indicator'
save `f_`indicator''
restore
append using `f_`indicator''

	
}


* compute RRs

foreach var in  mean_minor_served mean_amer_ind_served mean_asian_served ///
				mean_black_served mean_hispanic_served mean_pac_isl_served {
					gen rr_`var' = `var' / mean_nhw_served
	
}

gen rr_income =  mean_lowinc_served / mean_highinc_served
	
order indicator 
gen type = "Drop Top 10 PWS"

save "${path}risk_90pctile\june\dem_all_droptop10.dta" , replace	





********************************************************************************
* Drop top 1 % of PWS
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
		pop_served ///
		minor_served amer_ind_served asian_served black_served hispanic_served pac_isl_served nhw_served 		highinc_served lowinc_served ///
		`indicator'  

* convert to numeric variables 		
destring `indicator'  , replace ignore("NA")
drop if `indicator'   == . 	


* drop the top 10

sort `indicator'
gen rank = _n
gen total_PWS = _N
gen pctile = rank / total_PWS
drop if pctile >=0.99



* Calculate average by group 
	
foreach var in pop_served minor_served amer_ind_served asian_served black_served hispanic_served pac_isl_served nhw_served highinc_served lowinc_served{
		cap drop products
	egen denominator_`var' = total(`var')
	gen products_`var' = `var' * `indicator'
	egen numerator_`var' = total(products_`var' )
	gen mean_`var' = numerator_`var' / denominator_`var'
	
	}

	


keep  	mean_pop_served mean_minor_served mean_amer_ind_served mean_asian_served mean_black_served mean_hispanic_served mean_pac_isl_served mean_nhw_served mean_highinc_served mean_lowinc_served

gen indicator = "`indicator'" /*generate a column for the indicator */
		
duplicates drop 
tempfile f_`indicator'
save `f_`indicator''
restore
append using `f_`indicator''

	
}


* compute RRs

foreach var in  mean_minor_served mean_amer_ind_served mean_asian_served ///
				mean_black_served mean_hispanic_served mean_pac_isl_served {
					gen rr_`var' = `var' / mean_nhw_served
	
}

gen rr_income =  mean_lowinc_served / mean_highinc_served
	
order indicator 
gen type = "Drop Top 1%"

save "${path}risk_90pctile\june\dem_all_droptop1pct.dta" , replace	









********************************************************************************
* Compare RRs at FPL vs. 2X FPL
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
		`indicator'  frac_pov99 frac_pov199
	
	foreach var in frac_pov99 frac_pov199{
	 destring `var' , replace ignore("NA")
	 }

	gen lowinc_served_FPL = pop_served * frac_pov99
	

* convert to numeric variables 		
destring `indicator'  , replace ignore("NA")
drop if `indicator'   == . 	
	
* Calculate avg by group 	

foreach var in lowinc_served lowinc_served_FPL highinc_served{
	cap drop products
	egen denominator_`var' = total(`var')
	gen products_`var' = `var' * `indicator'
	egen numerator_`var' = total(products_`var' )
	gen mean_`var' = numerator_`var' / denominator_`var'
	
	}


keep  mean_lowinc_served mean_lowinc_served_FPL mean_highinc_served

gen indicator = "`indicator'" /*generate a column for the indicator */
		
duplicates drop 
tempfile f_`indicator'
save `f_`indicator''
restore
append using `f_`indicator''
	
}



* compute RRs

foreach var in  mean_lowinc_served mean_lowinc_served_FPL {
					gen rr_`var' = `var' / mean_highinc_served
	
}

order indicator 

save "${path}risk_90pctile\june\rr_fpl_fplX2.dta" , replace
	
	
	