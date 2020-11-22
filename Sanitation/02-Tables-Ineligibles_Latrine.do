cap log close
log using "${REPO_LOG}/02-Tables-Ineligibles_Latrine", replace text

/***************************************************************
PROJECT: 		ENCOURAGING SANITATION INVESTMENT IN THE 
				DEVELOPING WORLD: A CLUSTER-RANDOMIZED TRIAL

AUTHORS: 		Raymond Guiteras, James Levinsohn &
				Mushfiq Mobarak
				
CODE AUTHORS: 	Laura Feeney (IPA) & Derek Wolfson (IPA)

PURPOSE: 		Creates Tables S6 & S7 from the 
				Supplementary Materials
****************************************************************/

/* *************************************************
0 - SETUP AND OPTIONS
***************************************************/
set more off
use "${REPO_DATA}/BD-SAN-FINAL.dta", clear

	//TABULATE TREATMENT VARIABLES FOR USE IN REGRESSIONS
	levelsof treat_cat_3, local(LEVELS)
	foreach level of local LEVELS {
		gen TREAT3_`level' = (treat_cat_3==`level') if !mi(treat_cat_3)
		la var TREAT3_`level' "`: lab treat_cat_3 `level''"
		}
	drop TREAT3_0 // drop Control - force this to be omitted category
	la var TREAT3_3 "Ineligible (Low)"
	la var TREAT3_4 "Ineligible (Med)"
	la var TREAT3_5 "Ineligible (High)"
	drop TREAT3_6-TREAT3_8

	levelsof treat_cat_1, local(LEVELS)
	foreach level of local LEVELS {
		gen TREAT1_`level' = (treat_cat_1==`level') if !mi(treat_cat_1)
		la var TREAT1_`level' "`: lab treat_cat_1 `level''"
		}
	drop TREAT1_0 // drop Control - force this to be omitted category
		
	//OPTIONS FOR OUTREG2
	loc outregoptions excel \`ap' label nor2 noobs dec(3) drop(*uid* 2.uid 3.uid 4.uid) ctitle("\`\`x'l' \`\`lat'l'","\`RNOTE'")
	*ADDSTAT OPTIONS
	local ADDSTAT_ALL		`""Control Mean", control_mean, "Observations", e(N), "No. of Neighborhoods", nbhd_count, "No. of Villages", vil_count, "R-Squared", e(r2), "ICC Coef.", icc_rho"'
	local ADDSTAT_2X2		`""P-val: LPP Only = LPP+Subsidy", lppsubs_lpp, "P-val: LPP+Subsidy = LPP+Subsidy+Supply", subsidy_subssupply, "P-val: LPP Only = LPP+Subsidy+Supply", lppsubssup_lpp"'
	local ADDSTAT_3INT		`""P-val: Low = Med", LvM, "P-val: Low = High", LvH, "P-val: Med = High", MvH, "P-val: LPP Only = Low", LPPvL, "P-val: LPP Only = Medium", LPPvM, "P-val: LPP Only = High", LPPvH"'	

	*COLUMN HEADERS
	loc hygl "Hygienic Latrine"
	loc anyl "Any Latrine"
	loc accessl "Access to"
	loc ownl "Ownership of"

/* *************************************************
1 - RUN REGRESSIONS AND OUTPUT TABLES
***************************************************/
gen sample=0
loc ap replace
loc ap2 replace

foreach lat in any hyg {
	foreach x in access own { 
		foreach control in none bl_any {
			foreach R in r4{
		
			//SET CONTROLS
			loc nonec		
			loc bl_anyc 	bl_c_any_`x' 
			loc bl_hygc 	bl_c_hyg_`x'
				
			/************
			*  TABLE S6 *
			************/	
			loc cluster vid
			xi: reg `R'_`lat'_`x' TREAT1* ``control'c' i.uid if eligible==0, vce(cluster `cluster')
				replace sample = sample + e(sample)
				sum `e(depvar)' if treat_cat_1==0 & eligible==0 & e(sample)
					scalar control_mean = `r(mean)'
				tab cid if e(sample)
					scalar nbhd_count = `r(r)'
				tab vid if e(sample)
					scalar vil_count = `r(r)'
				//ADD ICC
				loneway `e(depvar)' `cluster' if e(sample)
					scalar icc_rho = `r(rho)'
			
			di "LPP+Subsidy vs LPP Only"
			test TREAT1_2 == TREAT1_1
			scalar lppsubs_lpp = `r(p)'
			
			di "LPP+Subsidy vs LPP+Subsidy+Supply"
			test TREAT1_2==TREAT1_4
			scalar subsidy_subssupply = `r(p)'
			
			di "LPP+Subsidy+Supply vs LPP Only"
			test TREAT1_4 == TREAT1_1
			scalar lppsubssup_lpp = `r(p)'

			outreg2 using "${REPO_OUT}/tables/Table_S6", `outregoptions'  ///
				addnote(Ineligible sample only. Robust standard errors clustered by `cluster'. All specifications include union fixed effects.) ///
				addstat(`ADDSTAT_ALL', `ADDSTAT_2X2') ///	
				ti("Table S6. Effects of Community-level Treatments on Individual Latrine Ownership and Access" "Ineligible Sample")				
						
			/************
			*  TABLE S7 *
			************/				
			local cluster cid
			xi: reg `R'_`lat'_`x' TREAT3* ``control'c' i.uid if eligible==0, vce(cluster `cluster')
				replace sample = sample + e(sample)

				sum `e(depvar)' if treat_cat_3==0 & eligible==0 & e(sample)
					scalar control_mean = `r(mean)'
				tab cid if e(sample)
					scalar nbhd_count = `r(r)'
				tab vid if e(sample)
					scalar vil_count = `r(r)'
				//ADD ICC
				loneway `e(depvar)' `cluster' if e(sample)
					scalar icc_rho = `r(rho)'
					
			di "Ineligible: Low vs Medium"
			test TREAT3_3== TREAT3_4
			scalar LvM = `r(p)'
			
			di "Ineligible: Low vs High"
			test TREAT3_3== TREAT3_5
			scalar LvH = `r(p)'
			
			di "Ineligible: Medium vs High"
			test TREAT3_3== TREAT3_5
			scalar MvH = `r(p)'
							
			di "Ineligible(Low) = LPP Only"
			test TREAT3_3 == TREAT3_1
			scalar LPPvL = `r(p)'
			
			di "Ineligible(Med) = LPP Only"
			test TREAT3_4 == TREAT3_1
			scalar LPPvM = `r(p)'
			
			di "Ineligible(High) = LPP Only"
			test TREAT3_5 == TREAT3_1
			scalar LPPvH = `r(p)'				
							
			outreg2 using "${REPO_OUT}/tables/Table_S7", `outregoptions'  ///
				addnote(Ineligible sample only. Robust standard errors clustered by group. All specifications include union fixed effects.) ///
				addstat(`ADDSTAT_ALL', `ADDSTAT_3INT') ///
				ti("Table S7. Effects of the Proportion of the Community Treated on Individual Latrine Ownership and Access" "Ineligible Sample") 
						
			loc ap append
			} // END ROUND
		} //END CONTROL
	} // END X ACCESS/OWN
} // END LAT

cap erase "${REPO_OUT}/tables/Table_S6.txt"		
cap erase "${REPO_OUT}/tables/Table_S7.txt"

