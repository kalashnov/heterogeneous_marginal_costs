/***************************************************************
PROJECT: 		ENCOURAGING SANITATION INVESTMENT IN THE 
				DEVELOPING WORLD: A CLUSTER-RANDOMIZED TRIAL

AUTHORS: 		Raymond Guiteras, James Levinsohn &
				Mushfiq Mobarak
				
CODE AUTHORS: 	Laura Feeney (IPA) & Derek Wolfson (IPA)

PURPOSE: 		CREATE DATASETS FOR GRAPHING
****************************************************************/
set more off

/* *****************************************
1 - CREATE ELIGIBLE DATASETS 
********************************************/
use "${REPO_DATA}/BD-SAN-FINAL.dta", clear
local SAMPLE ELIGIBLE
	foreach TREAT_CAT in 1 5 { // loop through different treatment variables

		/* 
		This section sets up the # of categories for each different treatment category variable.
		This is used to determine how many times to go through the loop below, and the number lists used
		to label the graphs. Each number in numlist corresponds to a treatment category, so the graph knows
		which lines to output.
		*/
			 
		qui sum treat_cat_`TREAT_CAT'
		loc n = r(max)
		numlist "0/`n'"
		loc NUMLIST0 = r(numlist)
		numlist "0/`n'"
		loc NUMLIST1 = r(numlist)

		/* 
		This section uses postfile to create a placeholder dataset. We will put results of the regressions
		and summary stats in this file, then once done will use this dataset to create graphs, then delete it. 
		Variables must be "posted" in the same order they're originally called. Descriptions below. 
		*/
			
		tempname post_handle`TREAT_CAT'
		local postfile_varlist /// 
			str20(treatment_label) ///1 - label of treatment category variable
			double(pt_est ul ll)  ///2 3 4 - beta, upper and lower limit of estimate/diff/mean
			long(N)  ///5 - # obs
			str180(cmdline) /// 6 - commandline
			str10(gr_type)  /// 7 - text - Means or diffs.
			str15(lat_type) /// 8 - text - hygienic or any kind of latrine
			str2(round) /// 9 - text - round (r3, r4, etc)
			int(x_axis) /// value of aggreg_treatment, for use in plotting mean against this category
			double(controlmean) // 11 - value of control mean

		postfile `post_handle`TREAT_CAT'' `postfile_varlist' /// 
			using "${REPO_DATA}/postfile_test_`TREAT_CAT'_`SAMPLE'.dta" ///
			, replace


		/* 
		Loop through rounds, latrine types, and ownership/access. 
		Regress latrine ownership on set of dummy variables corresponding to the categories of treatment
		aggregation as defined above. (Remember, we're already in a loop of different treatment categorizations). 
		*/

		
		foreach round in r4 {
			foreach lat in any_access hyg_access any_own hyg_own any_od_adults any_od_male any_od_female { 
			
				ta treat_cat_`TREAT_CAT' if eligible, sum(`round'_`lat')
				
				sum `round'_`lat' if treat_cat_`TREAT_CAT' == 0 & eligible 
				scalar control_mean = `r(mean)'
				
				/* 
				Control for hyg/any access/ownership at baseline (cluster rate, since baseline was a 
				subsample. Edit here to pick which control to use. Could also edit file name and
				notes of graphs. 
				*/
				
				if inlist("`lat'", "any_access", "hyg_access") loc bl_cluster_mean bl_c_any_access
				if inlist("`lat'", "any_own", "hyg_own") loc bl_cluster_mean bl_c_any_own
				if inlist("`lat'", "any_od_adults") loc bl_cluster_mean bl_c_any_access
				
				if `TREAT_CAT'==1 {
					local cluster "vid"
					local clusternote "village"
					}
				if `TREAT_CAT'>1 {
					local cluster "cid"
					local clusternote "neighborhood"
					}	
				
				reg `round'_`lat' i.treat_cat_`TREAT_CAT' `bl_cluster_mean' i.uid if eligible, vce(cluster `cluster')
				di "`e(cmdline)'"
				loc cmdline1 `e(cmdline)'
				mat DIFFS = r(table) // this will hold the regression table
				la li treat_cat_`TREAT_CAT'
				
				* Save results in matrices and locals

				forval x=0/`n' { // n goes up to the max value of treat_cat_`TREAT_CAT'
					
					mat TMT`x' = DIFFS[1..., colnumb(DIFFS, "`x'.treat_cat_`TREAT_CAT'")] // column of matrix corresponding to variable
					loc pt_est	= TMT`x'[1,1] // beta
					loc se 		= TMT`x'[2,1] // std error
					loc ll		= TMT`x'[5,1] // lower limit of 95CI
					loc ul		= TMT`x'[6,1] // uppler limit of 95CI
					
					// adding control mean to put graph on the same scale as control - display as difference from control,
					// rather than just the coefficient
					
					loc pt_est_plus_control	= TMT`x'[1,1] + control_mean // beta
					loc se_plus_control 	= TMT`x'[2,1]  // std error
					loc ll_plus_control		= TMT`x'[5,1] + control_mean // lower limit of 95CI
					loc ul_plus_control		= TMT`x'[6,1] + control_mean // uppler limit of 95CI

					/* 
					The following -post- command adds observations one by one to the dataset created in the 
					postfile command. Result is one observation per treatment - for example
					Supply Only - point estimate - upper limit CI - lower limit CI - # observations in that category
					- regression command that produced the estiamtes - diff - type of latrine (access/own/hygienic/any/OD/etc)
					- round - `x' (which is the value of treat_cat_* corresponding to Supply Only, the x-axis value
					that will be used in the graph for this category) - control mean 
					*/
					
					post `post_handle`TREAT_CAT'' ///
						("`: label treat_cat_`TREAT_CAT' `x''")  ///
						(`pt_est_plus_control')  (`ul_plus_control') (`ll_plus_control') ///
						(`e(N)')  ("`cmdline1'")  ("Diff") ("`lat'") ("`round'") ///
						(`x') (control_mean)
						
						
					} // values of x
					*
				} // latrine type (any or hyg)
				*
			} // round
			*
			
			
		postclose `post_handle`TREAT_CAT''
	}

	//FINAL PROCESSING
	use "${REPO_DATA}/postfile_test_1_`SAMPLE'.dta", clear
	//PREP DATA FOR GRAPHING
	lab var pt_est "Point estimate"
	lab var ul "95% CI"
	labmask x_axis, val(treatment_label) 
	la def x_axis 0 "Control", modify
	gen y_line_control = controlmean 
	save "${REPO_DATA}/postfile_test_1_`SAMPLE'.dta", replace
			
	use "${REPO_DATA}/postfile_test_5_`SAMPLE'.dta", clear
	//PREP DATA FOR GRAPHING
	lab var pt_est "Point estimate"
	lab var ul "95% CI"
	labmask x_axis, val(treatment_label) 
	la def x_axis 0 "Control", modify

	//CREATE VARIABLES FOR HORIZONTAL LINES
	gen y_line_control = controlmean if inlist(treatment_label, "Control", "LPP Only")

	loc v y_line_losers
	bys gr_type lat_type round: gen `v'2 = pt_est if treatment_label=="Loser (Low)"
	bys gr_type lat_type round: egen `v'1 = max(`v'2)
	bys gr_type lat_type round: gen `v' = `v'1 if inlist(treatment_label, "Loser (Low)", "Loser (Med)", "Loser (High)")
	drop `v'1 `v'2

	loc v y_line_winners
	gen `v'2 = pt_est if treatment_label=="Winner (Low)"
	bys gr_type lat_type round: egen `v'1 = max(`v'2)
	bys gr_type lat_type round: gen `v' = `v'1 if inlist(treatment_label, "Winner (Low)", "Winner (Med)", "Winner (High)")
	drop `v'1 `v'2
	save "${REPO_DATA}/postfile_test_5_`SAMPLE'.dta", replace	

/* *****************************************
1 - CREATE INELIGIBLE DATASETS 
********************************************/
use "${REPO_DATA}/BD-SAN-FINAL.dta", clear
local SAMPLE INELIGIBLE
foreach TREAT_CAT in 1 2 { // loop through different treatment vars

	/* 
	This section sets up the # of categories for each different treatment category variable.
	This is used to determine how many times to go through the loop below, and the number lists used
	to label the graphs.
	*/
		 
	qui sum treat_cat_`TREAT_CAT'
	loc n = r(max)
	numlist "0/`n'"
	loc NUMLIST0 = r(numlist)
	numlist "0/`n'"
	loc NUMLIST1 = r(numlist)

	/* 
	This section uses postfile to create a placeholder dataset. We will put results of the regressions
	and summary stats in this file, then once done will use this dataset to create graphs, then delete it. 
	Variables must be "posted" in the same order they're originally called. Descriptions below.	
	*/
		
	tempname post_handle`TREAT_CAT'
	local postfile_varlist /// 
		str20(treatment_label) ///1 - label of treatment category variable
		double(pt_est ul ll)  ///2 3 4 - beta, upper and lower limit of estimate/diff/mean
		long(N)  ///5 - # obs
		str180(cmdline) /// 6 - commandline
		str10(gr_type)  /// 7 - text - Means or diffs.
		str15(lat_type) /// 8 - text - hygienic or any kind of latrine
		str2(round) /// 9 - text - round (r3, r4, etc)
		int(x_axis) /// value of aggreg_treatment, for use in plotting mean against this category
		double(controlmean) // 11 - value of control mean

	postfile `post_handle`TREAT_CAT'' `postfile_varlist' /// 
		using "${REPO_DATA}/postfile_test_`TREAT_CAT'_`SAMPLE'.dta" ///
		, replace

	/* 
	Loop through rounds, latrine types, and ownership/access. 
	Regress latrine ownership on set of dummy variables corresponding to the categories of treatment
	aggregation as defined above. (Remember, we're already in a loop of different treatment categorizations). 
	*/

	foreach round in r4 {
		foreach lat in any_access hyg_access any_own hyg_own any_od_adults any_od_male any_od_female { 

			ta treat_cat_`TREAT_CAT' if eligible==0, sum(`round'_`lat')
			
			sum `round'_`lat' if treat_cat_`TREAT_CAT' == 0 & eligible==0
			scalar control_mean = `r(mean)'
			
			/* 
			Control for hyg/any access/ownership at baseline (cluster rate, since baseline was a 
			subsample. Edit here to pick which control to use. Could also edit file name and
			notes of graphs. 
			*/
			
			if inlist("`lat'", "any_access", "hyg_access") loc bl_cluster_mean bl_c_any_access
			if inlist("`lat'", "any_own", "hyg_own") loc bl_cluster_mean bl_c_any_own
			if inlist("`lat'", "any_od_adults") loc bl_cluster_mean bl_c_any_access
			
			if `TREAT_CAT'==1 {
				local cluster "vid"
				local clusternote "village"
				}
			if `TREAT_CAT'>1 {
				local cluster "cid"
				local clusternote "neighborhood"
				}			
			
			reg `round'_`lat' i.treat_cat_`TREAT_CAT' `bl_cluster_mean' i.uid if eligible==0, vce(cluster `cluster')
			di "`e(cmdline)'"
			loc cmdline1 `e(cmdline)'
			mat DIFFS = r(table) // this will hold the regression table
			la li treat_cat_`TREAT_CAT'
			
			
			* Save results in matrices and locals

			forval x=0/`n' { // n goes up to the max value of treat_cat_`TREAT_CAT'
				
				mat TMT`x' = DIFFS[1..., colnumb(DIFFS, "`x'.treat_cat_`TREAT_CAT'")] // column of matrix corresponding to variable
				loc pt_est	= TMT`x'[1,1] // beta
				loc se 		= TMT`x'[2,1] // std error
				loc ll		= TMT`x'[5,1] // lower limit of 95CI
				loc ul		= TMT`x'[6,1] // uppler limit of 95CI
				
				// adding control mean to put graph on different scale of actual ownership
				
				loc pt_est_plus_control	= TMT`x'[1,1] + control_mean // beta
				loc se_plus_control 	= TMT`x'[2,1]  // std error
				loc ll_plus_control		= TMT`x'[5,1] + control_mean // lower limit of 95CI
				loc ul_plus_control		= TMT`x'[6,1] + control_mean // uppler limit of 95CI


				post `post_handle`TREAT_CAT'' ///
					("`: label treat_cat_`TREAT_CAT' `x''")  ///
					(`pt_est_plus_control')  (`ul_plus_control') (`ll_plus_control') ///
					(`e(N)')  ("`cmdline1'")  ("Diff") ("`lat'") ("`round'") ///
					(`x') (control_mean)
					
					
				} // values of x
				*
			} // latrine type (any or hyg)
			*
		} // round
		*
	postclose `post_handle`TREAT_CAT''
} //END TREAT_CAT
*
//FINAL PROCESSING
use "${REPO_DATA}/postfile_test_1_`SAMPLE'.dta", clear
lab var pt_est "Point estimate"
lab var ul "95% CI"
labmask x_axis, val(treatment_label) 
la def x_axis 0 "Control", modify

gen y_line_control = controlmean 

save "${REPO_DATA}/postfile_test_1_`SAMPLE'.dta", replace

use "${REPO_DATA}/postfile_test_2_`SAMPLE'.dta", clear
lab var pt_est "Point estimate"
lab var ul "95% CI"
labmask x_axis, val(treatment_label) 
la def x_axis 0 "Control", modify

gen y_line_control = controlmean if inlist(treatment_label, "Control" "LPP Only", "Supply Only")

loc v y_line_winners
bys gr_type lat_type round: gen `v'2 = pt_est if treatment_label=="Low Intensity"
bys gr_type lat_type round: egen `v'1 = max(`v'2)
bys gr_type lat_type round: gen `v' = `v'1 if inlist(treatment_label, "Low Intensity", "Medium Intensity", "High Intensity")
drop `v'1 `v'2

save "${REPO_DATA}/postfile_test_2_`SAMPLE'.dta", replace


