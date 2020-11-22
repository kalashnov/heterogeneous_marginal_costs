cap log close
log using "${REPO_LOG}\01-Figures", replace text

/***************************************************************
PROJECT: 		ENCOURAGING SANITATION INVESTMENT IN THE 
				DEVELOPING WORLD: A CLUSTER-RANDOMIZED TRIAL

AUTHORS: 		Raymond Guiteras, James Levinsohn &
				Mushfiq Mobarak
				
CODE AUTHORS: 	Laura Feeney (IPA) & Derek Wolfson (IPA)

PURPOSE: 		RUN ANALYSIS + CREATE MAIN TEXT FIGURES
****************************************************************/

/* *****************************************************
0 - SETUP AND OPTIONS
********************************************************/
local GRAPHOPTS "msymbol(d) mcolor(navy) lcolor(navy) lpattern(solid)"

/* *****************************************************
1 - CREATE DATASETS FROM REGRESSION OUTPUT FOR GRAPHING
********************************************************/
qui include "${REPO_CODE}\01-Figures-DataPrep.do"

/* ****************************
2 - GENERATE MAIN TEXT FIGURES
*******************************/
	*****************
	//FIGURE 1
	*****************
	use "${REPO_DATA}/postfile_test_1_ELIGIBLE.dta", clear
		//SET LOCALS
		local ylab .2(.1).9
		local NUMLIST 0 1 2 3 4 

		//FIG 1A
		local lat_type any_access
		twoway 	///
			(rcap ul ll x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_control x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'", `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST', angle(stdarrow) valuelabel) ///
				title("Fig 1A - Any Latrine Access") ///
				subtitle("Eligible Sample") ///
				legend(off) ///
				saving("${REPO_OUT}\figures\FIG1A.gph", replace)
				graph export "${REPO_OUT}\figures\FIG1A.pdf", replace
				
		//FIG 1B
		local lat_type hyg_access
		twoway 	///
			(rcap ul ll x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_control x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'", `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST', angle(stdarrow) valuelabel) ///
				title("Fig 1B - Hygienic Latrine Access") ///
				subtitle("Eligible Sample") ///				
				legend(off) ///
				saving("${REPO_OUT}\figures\FIG1B.gph", replace)
				graph export "${REPO_OUT}\figures\FIG1B.pdf", replace

		//FIG 1C
		local lat_type any_od_adults
		twoway 	///
			(rcap ul ll x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_control x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'", `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST', angle(stdarrow) valuelabel) ///
				title("Fig 1C - Adult Open Defecation") ///
				subtitle("Eligible Sample") ///				
				legend(off) ///
				saving("${REPO_OUT}\figures\FIG1C.gph", replace)
				graph export "${REPO_OUT}\figures\FIG1C.pdf", replace


	*****************
	//FIGURE 2
	*****************
		use "${REPO_DATA}/postfile_test_5_ELIGIBLE.dta", clear
		//SET LOCALS
		loc NUMLIST "inlist(x_axis, 1, 3, 4, 6, 7)" // this sets the treatments over which we want to show CIs
		loc NUMLIST2 0 1 2 3 4 5 6 7
		local ylab .1(.1).7
			
		//FIG 2A
		local lat_type any_own
		twoway /// 
			(rcap ul ll x_axis if lat_type == "`lat_type'" & `NUMLIST', `GRAPHOPTS') ///
			(line y_line_winners x_axis  if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_losers x_axis  if lat_type == "`lat_type'" , `GRAPHOPTS') ///
			(line y_line_control x_axis  if lat_type == "`lat_type'" , `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'" , `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST2', angle(stdarrow) valuelabel) ///
				title("Fig 2A - Any Latrine Ownership") ///
				subtitle("Eligible Sample") ///			
				legend(off) ///
				saving("${REPO_OUT}\figures\FIG2A.gph", replace)
				graph export "${REPO_OUT}\figures\FIG2A.pdf", replace

		//FIG 2B
		local lat_type hyg_own
		twoway /// 
			(rcap ul ll x_axis if lat_type == "`lat_type'" & `NUMLIST', `GRAPHOPTS') ///
			(line y_line_winners x_axis  if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_losers x_axis  if lat_type == "`lat_type'" , `GRAPHOPTS') ///
			(line y_line_control x_axis  if lat_type == "`lat_type'" , `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'" , `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST2', angle(stdarrow) valuelabel) ///
				title("Fig 2B - Hygienic Latrine Ownership") ///
					subtitle("Eligible Sample") ///			
				legend(off) ///
				saving("${REPO_OUT}\figures\FIG2B.gph", replace)
				graph export "${REPO_OUT}\figures\FIG2B.pdf", replace

		//FIG 2C
		local lat_type any_od_adults
		twoway /// 
			(rcap ul ll x_axis if lat_type == "`lat_type'" & `NUMLIST', `GRAPHOPTS') ///
			(line y_line_winners x_axis  if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_losers x_axis  if lat_type == "`lat_type'" , `GRAPHOPTS') ///
			(line y_line_control x_axis  if lat_type == "`lat_type'" , `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'" , `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST2', angle(stdarrow) valuelabel) ///
				title("Fig 2C - Adults Open Defecation") ///
				subtitle("Eligible Sample") ///				
				legend(off) ///
				saving("${REPO_OUT}\figures\FIG2C.gph", replace)
				graph export "${REPO_OUT}\figures\FIG2C.pdf", replace
			
/* ************************************
2 - GENERATE SUPPLEMENTAL TEXT FIGURES
***************************************/
	*****************
	//FIGURE S4
	*****************
	use "${REPO_DATA}/postfile_test_1_ELIGIBLE.dta", clear
	local ylab .2(.1).5
	local NUMLIST 0 1 2 3 4 
	
		//FIG S4A
		local lat_type any_od_male
		twoway 	///
			(rcap ul ll x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_control x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'", `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST', angle(stdarrow) valuelabel) ///
				title("Fig S4A - Male Open Defecation") ///
				subtitle("Eligible Sample") ///				
				legend(off) ///
				saving("${REPO_OUT}\figures\FIGS4A.gph", replace)
				graph export "${REPO_OUT}\figures\FIGS4A.pdf", replace
				
		//FIG S4B
		local lat_type any_od_female
		twoway 	///
			(rcap ul ll x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_control x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'", `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST', angle(stdarrow) valuelabel) ///
				title("Fig S4B - Female Open Defecation") ///
				subtitle("Eligible Sample") ///				
				legend(off) ///
				saving("${REPO_OUT}\figures\FIGS4B.gph", replace)
				graph export "${REPO_OUT}\figures\FIGS4B.pdf", replace
				
				
	*****************
	//FIGURE S5
	*****************				
		use "${REPO_DATA}/postfile_test_5_ELIGIBLE.dta", clear
		//SET LOCALS
		loc NUMLIST "inlist(x_axis, 1, 3, 4, 6, 7)" // this sets the treatments over which we want to show CIs
		loc NUMLIST2 0 1 2 3 4 5 6 7
		local ylab .1(.1).5
			
		//FIG S5A
		local lat_type any_od_male
		twoway /// 
			(rcap ul ll x_axis if lat_type == "`lat_type'" & `NUMLIST', `GRAPHOPTS') ///
			(line y_line_winners x_axis  if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_losers x_axis  if lat_type == "`lat_type'" , `GRAPHOPTS') ///
			(line y_line_control x_axis  if lat_type == "`lat_type'" , `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'" , `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST2', angle(stdarrow) valuelabel) ///
				title("Fig S5A - Male Open Defecation") ///
				subtitle("Eligible Sample") ///				
				legend(off) ///
				saving("${REPO_OUT}\figures\FIGS5A.gph", replace)
				graph export "${REPO_OUT}\figures\FIGS5A.pdf", replace

		//FIG S5B
		local lat_type any_od_female
		twoway /// 
			(rcap ul ll x_axis if lat_type == "`lat_type'" & `NUMLIST', `GRAPHOPTS') ///
			(line y_line_winners x_axis  if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_losers x_axis  if lat_type == "`lat_type'" , `GRAPHOPTS') ///
			(line y_line_control x_axis  if lat_type == "`lat_type'" , `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'" , `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST2', angle(stdarrow) valuelabel) ///
				title("Fig S5B - Female Open Defecation") ///
				subtitle("Eligible Sample") ///			
				legend(off) ///
				saving("${REPO_OUT}\figures\FIGS5B.gph", replace)
				graph export "${REPO_OUT}\figures\FIGS5B.pdf", replace
	

	*****************
	//FIGURE S6
	*****************
	use "${REPO_DATA}/postfile_test_1_INELIGIBLE.dta", clear
		//SET LOCALS
		local ylab 0(.2)1
		local NUMLIST 0 1 2 3 4

		//FIG S6A
		local lat_type any_own
		twoway 	///
			(rcap ul ll x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_control x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'", `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST', angle(stdarrow) valuelabel) ///
				title("Fig S6A - Any Latrine Ownership") ///
				subtitle("Ineligible Sample") ///
				legend(off) ///
				saving("${REPO_OUT}\figures\FIGS6A.gph", replace)
				graph export "${REPO_OUT}\figures\FIGS6A.pdf", replace
				
		//FIG S6B
		local lat_type hyg_own
		twoway 	///
			(rcap ul ll x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_control x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'", `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST', angle(stdarrow) valuelabel) ///
				title("Fig S6B - Hygienic Latrine Ownership") ///
				subtitle("Ineligible Sample") ///
				legend(off) ///
				saving("${REPO_OUT}\figures\FIGS6B.gph", replace)
				graph export "${REPO_OUT}\figures\FIGS6B.pdf", replace

		//FIG S6C
		local lat_type any_od_adults
		twoway 	///
			(rcap ul ll x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_control x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'", `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST', angle(stdarrow) valuelabel) ///
				title("Fig S6C - Adult Open Defecation") ///
				subtitle("Ineligible Sample") ///
				legend(off) ///
				saving("${REPO_OUT}\figures\FIGS6C.gph", replace)
				graph export "${REPO_OUT}\figures\FIGS6C.pdf", replace

	*****************
	//FIGURE S7
	*****************
	use "${REPO_DATA}/postfile_test_2_INELIGIBLE.dta", clear
		//SET LOCALS
		loc ylab 0(.2)1
		loc NUMLIST 0 1 2 3 4 5
		loc NUMLIST2 "inlist(x_axis, 1, 2, 4, 5)" // this sets the treatments over which we want to show CIs

		//FIG S7A
		local lat_type any_own
		twoway 	///
			(rcap ul ll x_axis if lat_type == "`lat_type'" & `NUMLIST2', `GRAPHOPTS') ///
			(line y_line_winners x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_control x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'", `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST', angle(stdarrow) valuelabel) ///
				title("Fig S7A - Any Latrine Ownership") ///
				subtitle("Ineligible Sample") ///
				legend(off) ///
				saving("${REPO_OUT}\figures\FIGS7A.gph", replace)
				graph export "${REPO_OUT}\figures\FIGS7A.pdf", replace
				
		//FIG S7B
		local lat_type hyg_own
		twoway 	///
			(rcap ul ll x_axis if lat_type == "`lat_type'" & `NUMLIST2', `GRAPHOPTS') ///
			(line y_line_winners x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_control x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'", `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST', angle(stdarrow) valuelabel) ///
				title("Fig S7B - Hygienic Latrine Ownership") ///
				subtitle("Ineligible Sample") ///
				legend(off) ///
				saving("${REPO_OUT}\figures\FIGS7B.gph", replace)
				graph export "${REPO_OUT}\figures\FIGS7B.pdf", replace

		//FIG S7C
		local lat_type any_od_adults
		twoway 	///
			(rcap ul ll x_axis if lat_type == "`lat_type'" & `NUMLIST2', `GRAPHOPTS') ///
			(line y_line_winners x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(line y_line_control x_axis if lat_type == "`lat_type'", `GRAPHOPTS') ///
			(scatter pt_est x_axis if lat_type == "`lat_type'", `GRAPHOPTS'), ///
				ytitle("Share") ///
					ylab(`ylab', angle(horizontal) grid gmin gmax format(%3.2f)) ///
				xtitle("") ///
					xlab(`NUMLIST', angle(stdarrow) valuelabel) ///
				title("Fig S7C - Adult Open Defecation") ///
				subtitle("Ineligible Sample") ///
				legend(off) ///
				saving("${REPO_OUT}\figures\FIGS7C.gph", replace)
				graph export "${REPO_OUT}\figures\FIGS7C.pdf", replace
				window manage close graph

/* ************************************
2 - CLEAN-UP AUX FILES
***************************************/
cap erase "${REPO_DATA}/postfile_test_1_ELIGIBLE.dta"
cap erase "${REPO_DATA}/postfile_test_5_ELIGIBLE.dta"
cap erase "${REPO_DATA}/postfile_test_1_INELIGIBLE.dta"
cap erase "${REPO_DATA}/postfile_test_2_INELIGIBLE.dta"

cap log close
