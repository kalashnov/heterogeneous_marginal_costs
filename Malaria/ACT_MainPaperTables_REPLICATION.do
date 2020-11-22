
*************************************************************************************************************************************
**** TITLE: "PRICE SUBSIDIES, DIAGNOSTIC TESTS, AND TARGETING OF MALARIA TREATMENT: EVIDENCE FROM A RANDOMIZED CONTROLLED TRIAL" ****
**** DATE: OCTOBER 2016																				      						 ****	
**** REPLICATION FILE																					  						 ****	
*************************************************************************************************************************************


* ****************************************************************************************
* PROGRAM: ACT_MainPaperTables_REPLICATION
* PURPOSE: This code replicates all main and appendix A tables and figures 
*	for the published version of "Price Subsidies, Diagnostic Tests, and Targeting of 
* 	Malaria Treatment: Evidence from a Randomized Controlled Trial"
* *****************************************************************************************

capture log close
clear

* FILE NAMES
* MAIN DATASETS
	global all_main ACT_AllMain_FINAL_pub 
	global all_ill_prob ACT_IllLvlMainWithMalProbs_FINAL_pub
	global hh_follow_all ACT_HHFollowUp_All_FINAL_pub
	global chem_main_long ACT_PharmLogPos_FINAL_pub
	global baselines_mal ACT_BaselineMal_FINAL_pub
	global pharma_txn ACT_NonProjectTxns_FINAL_pub


* put the file path to the replication data in quotation marks after the finaldata global
* put the file path to the FDO do file in quotation marks after the dofiles global
* note: you must end you file paths with a slash: / for Mac, \ for PC for the FDR code to run
	global finaldata "YOUR PATH"
	global dofiles "YOUR PATH"
	global output "YOUR PATH"
	global logfiles "YOUR PATH"

**********************
****CREATE LOGFILE****
**********************

log using "${logfiles}\ACT_MainPaperTables", smcl replace
	
cd "$finaldata"
	
* SET ADDITIONAL CONTROLS FOR SPECS
global cont "B_head_age_imputed B_head_age_missing"

* YOU MUST SET THE doFDR GLOBAL TO 'Yes' AND RUN CODE CONTINUOUSLY FROM THE TOP ALL THE WAY DOWN TO
*	APPENDIX TABLE A7 TO CREATE APPENDIX TABLE A7! (Note -- in order to run tables one by one you
*	should set doFDR to 'No')
 global doFDR "Yes"
* global doFDR "No"

clear
set more off 
set mem 500m
set matsize 1000

mat fdr=J(1000,3,.)
	local ftable=2
	local frow=1

		
* **************************************************************
* TABLE 1 :: SUMMARY STATISTICS AND BALANCE CHECK
* **************************************************************
use $all_main, clear
set more off
gen B_antimal_cost_US = B_antimal_cost/78
	mat t=J(43,7,.)
local row=1
foreach var in head_fem B_head_age head_edu head_lit head_mar subfarm head_dep B_hh_size B_adultteen head_acres B_dist_km ///
	dist_clinic num_bednets share_undernet B_knowledge_correct B_heard_act B_act_best B_heard_rdt treat_h2o B_mal_ct B_antimal_cost_US {
	local col=1

	qui sum `var' if act500==1
	mat t[`row',`col']= r(mean)
	mat t[`row'+1,`col']= r(sd)
		local ++col
	cap reg `var' act40 act60 act100 rdt_any i.totstrata, r
	foreach cov in act40 act60 act100 rdt_any {
		mat t[`row',`col']= _b[`cov']
		mat t[`row'+1,`col']= _se[`cov']
			local ++col
			}
			
		test act40=act60=act100=rdt_any=0
			mat t[`row',`col']= r(F)
			mat t[`row'+1,`col']= r(p)
				local ++col
		mat t[`row',`col']= e(N)
		local row=`row'+2
	}
local col=1	
foreach var in act500 act40 act60 act100 {
	replace `var'=0 if rdt_any==1
	}
foreach var in act500 act40 act60 act100 rdt_any {
	sum `var'
	mat t[`row',`col']= r(mean)*r(N)
		local ++col
}
mat li t

* **************************************************************************************
* TABLE 2 :: IMPACT OF ACT AND RDT SUBSIDY ON CARE SEEKING, ACCESS
* **************************************************************************************
use $all_ill_prob, clear
	set more off
	if "$doFDR" == "No" {
		local ftable=1
		local frow=1
		}
keep if  first_ep==1 & ex_post==0 & rdt_any==0
	g byte ses_low= head_lit==0 if head_lit!=.	
	g young= LOG_patient_age<14
	g act_any= act40 | act60 | act100

mat t= J(12,9,.)
local rowst=1
foreach group in all {
local col=1
	foreach out in took_act took_act_chem took_act_hc care_chem care_hc care_nothing took_maltest took_antibio {
	local row=`rowst'
	
* SPECIFICATION 1 (POOLED)
reg `out' act_any i.totstrata ex_post $cont if `group'==1, clu(househo)
		foreach v in act_any {
			mat t[`row',`col']= _b[`v']
				local ++row
			mat t[`row',`col']= _se[`v']
				local ++row
		if "`group'"=="all" {		
			mat fdr[`frow',1]= `ftable'
			mat fdr[`frow',2]= 2*ttail(e(df_r),abs(_b[`v']/_se[`v']))
			mat fdr[`frow',3]= _b[`v']

				local ++frow
			}				
			}

* SPECIFICATION 2
	reg `out' act40 act60 act100 i.totstrata ex_post $cont if `group'==1, clu(househo)
		foreach v in act40 act60 act100 {
			mat t[`row',`col']= _b[`v']
				local ++row
			mat t[`row',`col']= _se[`v']
				local ++row

		if "`group'"=="all" {		
			mat fdr[`frow',1]= `ftable'
			mat fdr[`frow',2]= 2*ttail(e(df_r),abs(_b[`v']/_se[`v']))
			mat fdr[`frow',3]= _b[`v']
				local ++frow
			}			
			}
		test act40=act60=act100=0
			mat t[`row',`col']= r(p)
				local ++row		
		test act40=act60=act100
			mat t[`row',`col']= r(p)
				local ++row	
		qui sum `out' if `group'==1 & act500 & e(sample)
			mat t[`row',`col']= r(mean)
				local ++row
			mat t[`row',`col']= e(N)
	local ++col
	}
	local rowst=`row'+1
	}
mat li t
	
	
* **************************************************************************************
* TABLE 3 :: TARGETING RESULTS
* **************************************************************************************
local ftable=3
	if "$doFDR" == "No" local frow=1
		
use $all_ill_prob, clear
	keep if ex_post==0 & rdt_any==0 & took_act==1 & act500==0
	keep if first_ep==1
	g group=2
	tempfile endline
		save "`endline'"

use $chem_main_long, clear
	keep if ex_post==1 & used_act==1 & act500==0
	drop if rdt_any==1
	ren LOG_patient_age LOG_patient_age1
	ren used_act_adult adult
	g group=1
	tempfile tomerge
		save "`tomerge'"
	
use $all_main, clear
	set more off
	drop if rdt_any==1 | ex_post==0 | act500
	keep if used_act==1
	replace LOG_mal_prob21=. if rdt_pos==. | ex_post==0
	ren used_act_adult adult
	g group=0
	append using "`tomerge'"	
	append using "`endline'"
		
replace all=1
g young= adult==0 

g rdt_posA= rdt_pos if group==0
g rdt_posB= rdt_pos if group==1
g mal_probA= LOG_mal_prob21 if group==0
g mal_probB= LOG_mal_prob2 if group==1
g mal_probC= mal_prob2 if group==2

mat t=J(8,3,.)
local col=1
  foreach take in rdt_posA mal_probA mal_probC {	
	local row=1
	
	foreach type in all {	
		xi: reg `take' act60 act100 if `type'==1, clu(househo)
			foreach var in act60 act100 {
				mat t[`row',`col']= _b[`var']
					local ++row
				mat t[`row',`col']= _se[`var']
				local ++row		
			mat fdr[`frow',1]= `ftable'
			mat fdr[`frow',2]= 2*ttail(e(df_r),abs(_b[`var']/_se[`var']))
			mat fdr[`frow',3]= _b[`var']
				local ++frow				
				}
	qui test act60=act100==0	
	mat t[`row',`col']= r(p)
		local ++row
		
	qui test act60=act100
	mat t[`row',`col']= r(p)
		local ++row
		
	qui sum `take' if e(sample) & act40 & rdt_any==0
		mat t[`row',`col']= r(mean)
			local ++row
		mat t[`row',`col']= e(N)
			local ++row
	}	
	local ++col
	}
mat li t

* **********************************************************************************
* TABLE 4  :: MECHANISMS
* **********************************************************************************
local ftable=4
	if "$doFDR" == "No" local frow=1
use $all_main, clear
	set more off
	drop if act500 | rdt_any==1	
		g used_act_young= used_act_baby+used_act_kid+used_act_teen
		g frac_adult= used_act_adult if used_act==1
		g rdt_pos_young= rdt_pos if ex_post & used_act_young==1
		g rdt_pos_adult= rdt_pos if ex_post & used_act_adult==1
		g finalout= rdt_pos if ex_post & used_act==1		
local col=1	
mat t=J(7,4,.)
foreach out in used_act_young used_act_adult rdt_pos_young rdt_pos_adult {
local cme "i.totstrata $cont"
	if "`out'"=="rdt_pos_adult" | "`out'"=="rdt_pos_young" local cme ""	
	local row=1
	reg `out' act60 act100 `cme', r
		foreach price in act60 act100 {
			mat t[`row',`col']= _b[`price']
				local ++row
			mat t[`row',`col']= _se[`price']
				local ++row
			
			mat fdr[`frow',1]= `ftable'
			mat fdr[`frow',2]= 2*ttail(e(df_r),abs(_b[`price']/_se[`price']))
			mat fdr[`frow',3]= _b[`price']
				local ++frow
			}
			
			test act60=act100=0
			mat t[`row',`col']= r(p)
				local ++row			
			qui sum `out' if e(sample) & act40
			mat t[`row',`col']= r(mean)
				local ++row
			mat t[`row',`col']= e(N)
				local ++row
		local ++col
	}
mat li t

* **************************************************************************************
* TABLE 5 :: IMPACT OF RDT SUBSIDY ON CARE SEEKING, ACCESS
* **************************************************************************************
local ftable=5
	if "$doFDR" == "No" local frow=1
use $all_ill_prob, clear
	set more off
	keep if  first_ep==1
 	drop if act500
 	keep if rdt_any==1 | ex_post==0
	
foreach p in act40 act60 act100 {
	g R`p'= rdt_any*`p'
	}	

mat t= J(17,9,.)
local rowst=1
foreach group in all {
local col=1
	foreach out in care_chem care_hc care_nothing took_maltest took_rdt took_micro took_act took_antibio   {
	local row=`rowst'
	
* SPECIFICATION 1
	reg `out' rdt_any i.totstrata $cont if `group'==1, clu(househo)
		foreach v in rdt_any {
			mat t[`row',`col']= _b[`v']
				local ++row
			mat t[`row',`col']= _se[`v']
				local ++row
			
		if "`group'"=="all" {		
			mat fdr[`frow',1]= `ftable'
			mat fdr[`frow',2]= 2*ttail(e(df_r),abs(_b[`v']/_se[`v']))
			mat fdr[`frow',3]= _b[`v']
				local ++frow
			}
			}			
		qui sum `out' if rdt_any==0 & e(sample) 
			mat t[`row',`col']= r(mean)
				local ++row
			mat t[`row',`col']= e(N)
				local ++row
	
* SPECIFICATION 2
	reg `out' Ract40 Ract60 Ract100 act60 act100 i.totstrata $cont if `group'==1, clu(househo)
		foreach v in Ract40 Ract60 Ract100 act60 act100 {
			mat t[`row',`col']= _b[`v']
				local ++row
			mat t[`row',`col']= _se[`v']
				local ++row
		if "`group'"=="all" {		
			mat fdr[`frow',1]= `ftable'
			mat fdr[`frow',2]= 2*ttail(e(df_r),abs(_b[`v']/_se[`v']))
			mat fdr[`frow',3]= _b[`v']
				local ++frow
			}			
			
			}
		test Ract40=Ract60=Ract100=0
		mat t[`row',`col']= r(p)
			local ++row
			
		qui sum `out' if `group'==1 & rdt_any==0 & e(sample) & act40
			mat t[`row',`col']= r(mean)
				local ++row
			mat t[`row',`col']= e(N)
	local ++col
	}
	local rowst=`row'+1
	}
mat li t


* **************************************************************
* TABLE 6: TARGETING TABLE FOR RDTS
* **************************************************************
local ftable=6
	if "$doFDR" == "No" local frow=1
use $all_main, clear
	drop if act500 
	keep if ex_post==1
g rdt_pos_trt= rdt_pos if sought_treat==1
g used_rdt_trt= used_rdt if sought_treat==1
g comp_neg= used_act==0 if used_rdt==1 & rdt_pos==0
g comp_pos= used_act==1 if used_rdt==1 & rdt_pos==1
g rdt_pos_act= rdt_pos if used_act==1

foreach var in used_rdt_trt comp_neg comp_pos {
	replace `var'=. if rdt_any==0
	}
	
foreach p in act40 act60 act100 {
	g R`p'= rdt_any*`p'
	}

local rowct=1
local col=1
mat t=J(10,4,.)	
foreach group in all {
foreach out in sought_treat rdt_pos_trt rdt_pos_act {
	ren `out' `out'_all
	local row=`rowct'

	local e1 ""
		if "`out'"=="sought_treat" local e1 "i.totstrata $cont"
		
	reg `out'_`group' rdt_any act60 act100 `e1', r
		mat t[`row',`col']= _b[rdt_any]
			local ++row
		mat t[`row',`col']= _se[rdt_any]
			local ++row
		if "`group'"=="all" {		
			mat fdr[`frow',1]= `ftable'
			mat fdr[`frow',2]= 2*ttail(e(df_r),abs(_b[rdt_any]/_se[rdt_any]))
			mat fdr[`frow',3]= _b[rdt_any]
				local ++frow
			}			
	reg `out'_`group' Ract40 Ract60 Ract100 act60 act100 `e1', r
		foreach cov in Ract40 Ract60 Ract100 {
			mat t[`row',`col']= _b[`cov']
				local ++row
			mat t[`row',`col']= _se[`cov']
				local ++row
		if "`group'"=="all" {		
			mat fdr[`frow',1]= `ftable'
			mat fdr[`frow',2]= 2*ttail(e(df_r),abs(_b[`cov']/_se[`cov']))
			mat fdr[`frow',3]= _b[`cov']
				local ++frow
			}
			}
		qui sum `out'_`group' if e(sample) & rdt_any==0 & act40
			mat t[`row',`col']= r(mean)
				local ++row
			mat t[`row',`col']= e(N)
				local ++row
	ren `out'_all `out'
	local ++col	
	}
	
	foreach out in used_rdt_trt {
		ren `out' `out'_all
		local row=`rowct'
		
	sum `out'_`group' if rdt_any==1		
			mat t[`row',`col']= r(mean)
				local ++row
				local ++row

	foreach val in act40 act60 act100 {		
		sum `out'_`group' if rdt_any==1	& `val'==1
			mat t[`row',`col']= r(mean)
				local ++row
				local ++row
				}
			local ++row
			qui sum `out'_`group' if rdt_any==1		
			mat t[`row',`col']= r(N)
	ren `out'_all `out'
	local ++col
	}				
	local rowct=`row'+1
	local col=1
	}
mat li t
	
* **********************************************************************************
* TABLE 7  :: T/OT/UT ESTIMATES -- EXTRA ESTIMATES
*	note: the calculations for this table are done in excel. information in other
*	tables can be combined with this information to create the estimates in 
*	Table 7
* **********************************************************************************
set more off
mat t=J(12,3,.)
use $all_ill_prob, clear
	set more off
	keep if  first_ep==1
 	keep if ex_post==0 | rdt_any==1
	replace ex_post=0	
foreach p in act40 act60 act100 {
	g R`p'= rdt_any*`p'
	}		
local col=1
	foreach out in took_act_hc took_act_chem  {
	local row=1	
	reg `out' Ract40 Ract60 Ract100 act60 act100 i.totstrata $cont if act500==0, clu(househo)
		foreach v in Ract40 Ract60 Ract100 act60 act100 {
			mat t[`row',`col']= _b[`v']
				local ++row
			mat t[`row',`col']= _se[`v']
				local ++row
			}
		qui sum `out' if act40 & rdt_any==0 & e(sample)
			mat t[`row',`col']= r(mean)
				local ++row
			mat t[`row',`col']= e(N)
				local ++row
				local ++row							
		local ++col
		}

use $all_main, clear
		keep if used_act==1 & (act40 | act60 | act100) & checklistonly==0 & ex_post==1
		foreach price in act40 act60 act100  {
			g `price'_rdt= `price'*rdt_any
			}	
	local row=1
	reg rdt_pos act60 act100 act40_rdt act60_rdt act100_rdt, r
		foreach cov in act60 act100 act40_rdt act60_rdt act100_rdt {
			mat t[`row',`col']= _b[`cov']
				local++row
			mat t[`row',`col']= _se[`cov']
				local ++row
			}
			mat t[`row',`col']= _b[_cons]
				local ++row
			mat t[`row',`col']= e(N)
mat li t


* **************************************************************
* TABLE A1 :: BASELINE TREATMENT SEEKING BEHAVIOR
* **************************************************************
use $all_main, clear
set more off
	g mal_kids= B_mal_baby_hh+B_mal_kid_hh+B_mal_teen
	g mal_adults= B_mal_adult
	g byte ep_kids= mal_kids>0 if mal_kids!=.
	g byte ep_adults= mal_adults>0 if mal_adults!=.
	
mat t=J(18,4,.)
	* FIRST TWO ROWS: AT HOUSEHOLD LEVEL
	replace base_rdt=. if B_mal_episode==0
	replace base_micro=. if B_mal_episode==0
local col=1
foreach group in all {
	local row=1
	foreach var in B_mal_ct B_mal_episode base_rdt base_micro {
		sum `var' if `group' 
			mat t[`row',`col']= r(mean)
				local ++row
			}
			local ++col
		}
	foreach var in kids adults {
	local row=1
	foreach st in mal ep {
		sum `st'_`var'  
			mat t[`row',`col']= r(mean)
			local ++row
			}
		local ++col
		}
keep all househo 
	sort househo
	tempfile tomerge
	save "`tomerge'"
use $baselines_mal, clear
	drop if malaria==0
		sort househo
		merge househo using "`tomerge'"
		tab _merge
		drop if _merge==2 /* not in episode ledger */
		drop _merge
local row=6
	g test= adult+teen+kid+baby
	foreach var in adult teen kid baby {
		replace `var'=. if test==0 & b5_==. & b6_==.
		}
replace took_other=0 if forgot_name==1
foreach var in took_act took_sp took_aq took_other forgot_name {
	replace `var'=0 if `var'==. & no_antimal==1
	}
	
g byte care_none= diag_init==4 if diag_init!=.
g byte care_hc= diag_init==1 | diag_init==2 if diag_init!=.
g byte care_chem= diag_init==3 if diag_init!=.
	replace care_hc=1 if care_none==1 & (got_drugs==1 | got_drugs==2)
	replace care_chem=1 if care_none==1 & got_drugs==3
	replace care_hc=. if care_none==1 & got_drugs==5
	replace care_chem=. if care_none==1 & got_drugs==5
	replace care_none=. if care_none==1 & got_drugs==5
	replace care_none=0 if got_drugs==1 | got_drugs==2 | got_drugs==3 | got_drugs==4
	replace care_hc=1 if care_none==0 & care_hc==. & got_drugs==1
	replace care_chem=1 if care_none==0 & care_chem==. & got_drugs==3
	replace antimal_cost=. if got_drugs==.
foreach var in drugs_public drugs_chemist drugs_oth {
	replace `var'=. if no_antimal==1
	}
foreach var in care_none care_hc care_chem no_antimal took_act took_sp took_aq took_oth forgot_name drugs_public drugs_chemist drugs_oth antimal_cost {
	sum `var'  
	mat t[`row',1]= r(mean)
	local col=2
foreach sub in adult {
	reg `var' `sub', clu(househo)
	mat t[`row',`col']= _b[_cons]
		local ++col
	mat t[`row',`col']=_b[_cons]+_b[`sub']
		local ++col
		test `sub'
	mat t[`row',`col']= r(p)
		local ++col
		}
	local ++row
	}
mat li t

* **************************************************************************************
* TABLE A2 :: REPORTING BIAS WITH ENDLINE ILLNESS EPISODES
* **************************************************************************************		
use $all_main, clear
	set more off
	g ill_hh= E_tabJ_illness>0 if E_tabJ_illness!=.
	g XE_tabJ_illness=E_tabJ_illness
	keep househo ill_hh XE_tabJ_illness act40 act60 act100  rdt_any ex_post totstrata $cont headage_x head_lit
	tempfile appender
	save "`appender'"
	
use $all_ill_prob, clear
*set more off
append using "`appender'"
	g byte act40100= coartemprice<500 
	g f_LOG_patient_age= LOG_patient_age if first_ep==1
	g mal_prob_hh= mal_prob2 if first_ep==1
	replace max_length=. if first_ep==0
	g byte ses_low = head_lit==0
	replace all=1
mat t=J(13,5,.)
local rowct=1
foreach group in all {
local col=1
foreach out in ill_hh XE_tabJ_illness mal_prob_hh max_length f_LOG_patient_age {
if "`out'"!="ill_hh" & "`out'"!="XE_tabJ_illness"  replace `out'=. if mal_prob2==.
local row= `rowct'
	xi: reg `out' act40 act60 act100 rdt_any ex_post i.totstrata $cont if `group'==1, clu(househo)
		foreach var in act40 act60 act100 rdt_any ex_post {
		mat t[`row',`col']= _b[`var']
			local ++row
		mat t[`row',`col']= _se[`var']
			local ++row
		}
		test act40=act60=act100
		mat t[`row',`col']= r(p)
			local ++row
	qui sum `out' if e(sample)
		mat t[`row',`col']= r(mean)
			local ++row
		mat t[`row',`col']= e(N)
			local ++row
			local ++col
	}
	local rowct= `row'
	}
mat li t


* ***********************************************************************
* TABLE A3 :: PREDICTING MALARIA POSITIVITY -- PROBIT MFX
* ***********************************************************************

use $hh_follow_all, clear
	keep if days_ago<=3
probit rdt_pos  cough chills headache diarrhea runnynose vomit bodypain malaise appetite  old2 LOG_patient_age age_sq cough_old2-appetite_old2 LOG_patient_age_old2 age_sq_old2, vce(cluster househo)
	mfx compute
	

* ****************************************************************
* TABLE A4 :: RDT TAKEUP BY RDT PRICE
* ****************************************************************
use $all_main, clear
set more off
	drop if coartemprice>100
	
g used_rdt_c= used_rdt if sought_treat==1
mat t=J(11,3,.)
local col=1
foreach out in used_rdt sought_treat used_rdt_c {
	local row=1
	xi: reg `out' rdt_free rdt_15r rdt_15 act60 act100 ex_post i.totstrata, r
		foreach coeff in rdt_free rdt_15r rdt_15 {
			mat t[`row',`col']= _b[`coeff']
				local ++row
			mat t[`row',`col']= _se[`coeff']
				local ++row
			}
		test rdt_free=rdt_15r=rdt_15
			mat t[`row',`col']= r(p)
				local ++row
	xi: reg `out' rdt_any act60 act100 ex_post i.totstrata , r
			mat t[`row',`col']= _b[rdt_any]
				local ++row
			mat t[`row',`col']= _se[rdt_any]
				local ++row
	qui sum `out' if e(sample) & rdt_none==1
			mat t[`row',`col']= r(mean)
				local ++row
			mat t[`row',`col']= e(N)
	local ++col
	}
mat li t


* ****************************************************************
* TABLE A5: TARGETING WITH RDTs 
* ****************************************************************
use $all_ill_prob, clear
	set more off
	drop if (ex_post & rdt_any==0) | act500
	keep if took_antimal==1 | took_mal==1	
			ren LOG_patient_age LOG_patient_age1
	
	ren mal_prob2 LOG_mal_prob21	
	replace sought_treat= took_antimal | took_mal
	replace used_act= took_antimal
	replace used_rdt= took_mal
	
	keep if first_ep | second_ep
	g dlong=3
	tempfile tomerge3
		save "`tomerge3'"

	keep if first_ep
		replace dlong=2
	tempfile tomerge2
		save "`tomerge2'"
		
use $chem_main_long, clear
	keep if used_act==1 | used_rdt==1
	drop if act500 
	
	ren LOG_patient_age LOG_patient_age1
	ren LOG_mal_prob2 LOG_mal_prob21
	g dlong=1
	tempfile tomerge
		save "`tomerge'"
		
use $all_main, clear
	drop if act500 | checklistonly
	g dlong=0

append using "`tomerge2'"
append using "`tomerge3'"
append using "`tomerge'"	

replace rdt_pos= LOG_mal_prob21
g rdt_pos_trt= rdt_pos if sought_treat==1
g rdt_pos_act= rdt_pos if used_act==1

foreach p in act40 act60 act100 {
	g R`p'= rdt_any*`p'
	}
	
local rowct=1
local col=1
mat t=J(10,4,.)	
foreach group in 0  2  {
foreach out in rdt_pos_trt rdt_pos_act {
	ren `out' `out'_all
	local row=`rowct'
		
	reg `out' rdt_any act60 act100 if dlong==`group', clu(household)
		mat t[`row',`col']= _b[rdt_any]
			local ++row
		mat t[`row',`col']= _se[rdt_any]
			local ++row
			
	reg `out' Ract40 Ract60 Ract100 act60 act100 if dlong==`group', clu(household)
		foreach cov in Ract40 Ract60 Ract100 {
			mat t[`row',`col']= _b[`cov']
				local ++row
			mat t[`row',`col']= _se[`cov']
				local ++row
			}
		qui sum `out' if e(sample) & rdt_any==0 & act40
			mat t[`row',`col']= r(mean)
				local ++row
			mat t[`row',`col']= e(N)
				local ++row
	ren `out'_all `out'
	local ++col	
	}
	}
	
mat li t

 * *************************************************************
 * APPENDIX TABLE A7
 *	note, you MUST
 *		A) run the code WITHOUT BREAKS from start to finish and
 *		B) set the global doFDR to "Yes" 
 * in order to successfully execute this code
 * *************************************************************
drop _all
	svmat fdr
		ren fdr2 pval
		ren fdr1 table
		drop if pval==.
do "${dofiles}\fdr_sharpened_qvalues.do"
	keep if pval<=.1
		ren pval orig_pval
		ren fdr3 coeff
		ren bky q_value
	list table coeff orig_pval q_value, noobs clean


	
* ****************************************************************
* ****************************************************************
* FIGURES
* ****************************************************************
* ****************************************************************

* *************************************************************
* FIGURE 3 :: POSITIVITY BY AGE
* *************************************************************	
use $all_main, clear
	keep if (act40 | act100 | act60) & ex_post==1 & rdt_any==0 & used_act==1 & checklistonly==0
	keep rdt_pos LOG_patient_age1
	g samp="CHEMIST"
	tempfile add2
	save "`add2'"
use $hh_follow_all, clear
	g samp="HH"
	* ONLY OBS W/ MAL TEST RESULTS
	keep if rdt_pos!=.
	
	* LIMIT SAMPLE TO EPS THAT STARTED THREE DAYS AGO OR LESS
	keep if days_ago<=3
	append using "`add2'"
		
	foreach var in LOG_patient_age LOG_patient_age1 {
		replace `var'=60 if `var'>60 & `var'!=.
		}
	lpoly rdt_pos LOG_patient_age, deg(1) noscatter ci tit("Panel A. All Illnesses", size(medium)) ylab(0(.2).8) xline(14, lcol(gs10)) /// 
		legend(off) graphregion(color(white) fcolor(white)) name(PA, replace) ytit("Share Positive") xtit("Age") ///
		xlab(0 14 20 40 60) note(" ") generate(pred_general) at(LOG_patient_age) se(se_general)
	lpoly rdt_pos LOG_patient_age1 if samp=="CHEMIST", deg(1) ci noscatter tit("Panel B. Retail-Sector Subsidized ACT Takers", size(medium)) xlab(0 14 20 40 60) ///
		legend(off) xline(14, lcol(gs10)) graphregion(color(white) fcolor(white))  ylab(0(.2).8) ytit("Share Positive") ///
		note(" ") xtit("Age") name(PB, replace) generate(pred_chem) at(LOG_patient_age) se(se_chem)
		
	foreach var in general chem {
		g ul_`var'= se_`var'*1.96 + pred_`var'
		g ll_`var'= pred_`var'- se_`var'*1.96
		}		
	gr combine PA PB, xsize(10) ysize(6) graphregion(color(white) fcolor(white)) ycommon saving("${output}\Figure 3", replace)
	
* **********************************************************************
* FIGURE 4: DEMAND CURVES (ALL)
* APPENDIX FIGURE A4: DEMAND CURVES BY AGE
* **********************************************************************

foreach age in all young adult {
local tit "title(" ")"
	if "`age'"=="young" local tit "title("I. Under Age 14", size(medsmall))"
	if "`age'"=="adult" local tit "title("II. Ages 14 and Older", size(medsmall))"

local ylab2 "ylab(0(.1).5)"
local ylab "ylab(0(.2).8)"
if "`age'"=="young" | "`age'"=="adult" { 
	local ylab "ylab(0(.1).5)"
	local ylab2 "ylab(0(.1).6)"
	}
use $all_main, clear
	set more off
	drop if checklisto 
	drop if rdt_any==1 | ex_post==1
qui tab totstrata, ge(strdum)
	foreach var of varlist $cont strdum* {
		qui sum `var'
			replace `var'= `var'-r(mean)
		}
replace num_actv=0 if num_actv==.

g used_act_young= used_act_baby + used_act_kid + used_act_teen + ///
				  used_act_baby2 + used_act_kid2 + used_act_teen2 + ///
				  used_act_baby3 + used_act_kid3 + used_act_teen3 + ///
				  used_act_baby4 + used_act_kid4 + used_act_teen4
replace used_act_adult= used_act_adult + used_act_adult2 + used_act_adult3 + used_act_adult4				  	
g used_act_all = used_act + used_act2 + used_act3 + used_act4	

mat t=J(10,16,.)
	local row=1	
	reg used_act_`age' act40 act60 act100 act500 ex_post $cont strdum1-strdum28, nocons r 
	test act40=act100
	test act40=act60
	test act60=act100
	test act40=act60=act100
		foreach price in 40 60 100 500 {
			mat t[`row',1]= `price'
			mat t[`row',2]= _b[act`price']
			mat t[`row',3]= _b[act`price'] - 1.96*_se[act`price']
			mat t[`row',4]= _b[act`price'] + 1.96*_se[act`price']
			local ++row
			}
									
use $all_ill_prob, clear
	drop if  ex_post==1 | illness==0
	drop if rdt_any==1
	keep if first_ep==1 
	replace used_act_v=0 if num_actv==0 | num_actv==.
	g young= LOG_patient_age<14

qui tab totstrata, ge(strdum)
	foreach var of varlist $cont strdum* {
		qui sum `var'
			replace `var'= `var'-r(mean)
		}
						
	local row=1
	reg took_act act40 act60 act100 act500 ex_post $cont strdum1-strdum28 if `age'==1, nocons clu(househo) 
		foreach price in 40 60 100 500 {
			mat t[`row',5]= `price'
			mat t[`row',6]= _b[act`price']
			mat t[`row',7]= _b[act`price'] - 1.96*_se[act`price']
			mat t[`row',8]= _b[act`price'] + 1.96*_se[act`price']
			local ++row
			}
			
	local row=1
	reg used_act_v act40 act60 act100 act500 ex_post $cont strdum1-strdum28 if `age'==1, nocons clu(househo) 
		foreach price in 40 60 100 500 {
			mat t[`row',9]= `price'
			mat t[`row',10]= _b[act`price']
			mat t[`row',11]= _b[act`price'] - 1.96*_se[act`price']
			mat t[`row',12]= _b[act`price'] + 1.96*_se[act`price']
			local ++row
			}
use $all_ill_prob, clear
	drop if rdt_any==1 | ex_post==1
	replace used_act_v=0 if num_actv==0 | num_actv==.
		g young= LOG_patient_age<14
	g used_act_v_young= used_act_v*young
	replace used_act_v_adult= used_act_v*adult
	g totadmin= used_act + used_act2 + used_act3 + used_act4
	
	collapse (sum) used_act_v used_act_v_young used_act_v_adult (mean) num_actv act40 act60 act100 act500 totadmin $cont totstrata ex_post, by(househo)
	
g anyuse_all= used_act_v if used_act_v!=.
g anyuse_young= used_act_v_young if used_act_v!=.
g anyuse_adult= used_act_v_adult if used_act_v!=.

qui tab totstrata, ge(strdum)
	foreach var of varlist $cont strdum* {
		qui sum `var'
			replace `var'= `var'-r(mean)
		}
	local row=1
	reg anyuse_`age' act40 act60 act100 act500 ex_post $cont strdum1-strdum28, nocons clu(househo) 
	test act40=act100
	test act40=act60
	test act60=act100
	test act40=act60=act100			
foreach price in 40 60 100 500 {
			mat t[`row',13]= `price'
			mat t[`row',14]= _b[act`price']
			mat t[`row',15]= _b[act`price'] - 1.96*_se[act`price']
			mat t[`row',16]= _b[act`price'] + 1.96*_se[act`price']
			local ++row
			}												
drop _all
	svmat t

	* ENDLINE
	twoway (scatter t10 t6 t1, connect(direct direct) lcol(gs7 black) mcol(gs7 black)) (rcap t11 t12 t1, lcol(gs7)) (rcap t7 t8 t1, lcol(black)), ///
		xtit("Subsidy Level", size(small)) xlab(40 "92%" 60 "88%" 100 "80%" 500 "None", labsize(vsmall)) name(PA, replace) tit("A. ACT Treatment for First Endline Illness Episodes", size(medsmall)) /// 		
		graphregion(color(white) fcolor(white)) `ylab2' legend(order(1 2) size(small) label(1 "Used ACT Voucher") label(2 "Took Any ACT")) saving("${output}\endline_`age'", replace)
	
	* CHEM LOG
	twoway 	(scatter t2 t14 t1, connect(direct direct) lcol(gs7 black) mcol(gs7 black)) (rcap t3 t4 t1, lcol(gs7)) (rcap t15 t16 t1, lcol(black)), ///
		xtit("Subsidy Level", size(small)) xlab(40 "92%" 60 "88%" 100 "80%" 500 "None", labsize(vsmall)) name(PB, replace) tit("B. Number ACT Vouchers Redeemed", size(medsmall)) /// 
		graphregion(color(white) fcolor(white)) `ylab' legend(order(1 2) size(small) label(1 "Admin. Data") label(2 "Endline Data"))  saving("${output}\number_`age'", replace)
		
	gr combine PA PB, row(2) col(1)	graphregion(color(white) fcolor(white)) xsize(7.5) ysize(10) name(`age', replace) `tit' saving("${output}\Figure4_`age'", replace)
	
	
}
	
* *********************************************************************
* FIGURE 5: RDT COMPLIANCE AND THE SUBISDY LEVEL
* *********************************************************************
use $all_main, clear
	set more off
	drop if  act500
	drop if checklistonly
	keep if rdt_any==1
	g no_act= used_act==0	
mat t=J(3,6,.)
local row=1
local col=1
* COMPLY WITH NEGATIVE
reg no_act act40 act60 act100 if used_rdt==1 & rdt_pos==0, r nocons
	foreach out in act40 act60 act100 {
		mat t[`row',`col']= _b[`out']
		mat t[`row',`col'+1]= _b[`out']-1.96*_se[`out']
		mat t[`row',`col'+2]= _b[`out']+1.96*_se[`out']		
			local ++row
		}
			local col=`col'+3
			local row=1	
* COMPLY WITH POSITIVE
reg used_act act40 act60 act100 if used_rdt==1 & rdt_pos==1, r nocons
	foreach out in act40 act60 act100 {
		mat t[`row',`col']= _b[`out']
		mat t[`row',`col'+1]= _b[`out']-1.96*_se[`out']
		mat t[`row',`col'+2]= _b[`out']+1.96*_se[`out']		
			local ++row
		}		
drop _all
	svmat t	
g priceg= _n
twoway (bar t1 priceg, barw(.8)) (rcap t2 t3 priceg), ylab(0(.2)1) xlab(1 "92%" 2 "88%" 3 "80%") ytit("Compliance Rate") name(PA, replace) /// 
	tit("Panel A. Complied - Negative Test (Did Not Take ACT)", size(medsmall)) legend(off) xtit("Subsidy Level") graphregion(color(white) fcolor(white)) 
twoway (bar t4 priceg, barw(.8)) (rcap t5 t6 priceg), ylab(0(.2)1) xlab(1 "92%" 2 "88%" 3 "80%") ytit("Compliance Rate") name(PB, replace) /// 
	tit("Panel B. Complied - Positive Test (Took ACT)", size(medsmall)) legend(off) xtit("Subsidy Level") graphregion(color(white) fcolor(white))	
gr combine PA PB, rows(1) col(2) ycommon graphregion(color(white) fcolor(white)) ysize(7.5) xsize(11) saving("${output}\Figure 5", replace)
	
				
* **********************************************************************
* FIGURE A2 -- PRED POS DISTN
* **********************************************************************
use $all_ill_prob, clear
	drop if mal_prob2 ==. 
	keep if first_ep 	
	qui sum mal_prob2 if wt5_tertile_NoFever_rel1==1
		local lim1=r(max)
	qui sum mal_prob2 if wt5_tertile_NoFever_rel2==1
		local lim2=r(max)	
	kdensity mal_prob2, xline(`lim1', lcol(gs13)) xline(`lim2', lcol(gs13)) xtit("Predicted Malaria Positivity", size(medsmall)) tit(" ") note(" ") graphregion(color(white) fcolor(white))	saving("${output}\Figure A2", replace)
	
* **********************************************************************
* FIGURE A3 -- PRICE HISTOGRAMS
* **********************************************************************	
use $pharma_txn, clear
histogram price_antimal if patient_age>=14, xline(40 60 100) xlab(50 "50" 100 "100" 150 "150" 200 "200" 250 "250+")  xtit("Antimalarial Price (Ksh)") /// 
	graphregion(color(white) fcolor(white))	tit("Ages 14+", size(medsmall)) name(p4, replace) 
histogram price_antimal if patient_age>=9 & patient_age<=13, xline(30 45 75) xlab(50 "50" 100 "100" 150 "150" 200 "200" 250 "250+")  xtit("Antimalarial Price (Ksh)") /// 
	graphregion(color(white) fcolor(white))	tit("Ages 9-13", size(medsmall)) name(p3, replace) 
histogram price_antimal if patient_age>=4 & patient_age<=8, xline(20 30 50) xlab(50 "50" 100 "100" 150 "150" 200 "200" 250 "250+")  xtit("Antimalarial Price (Ksh)") /// 
	graphregion(color(white) fcolor(white))	tit("Ages 4-8", size(medsmall)) name(p2, replace)
histogram price_antimal if patient_age<=3, xline(10 15 25) xlab(50 "50" 100 "100" 150 "150" 200 "200" 250 "250+") /// 
	xtit("Antimalarial Price (Ksh)") graphregion(color(white) fcolor(white)) tit("Ages 3 and Under", size(medsmall)) name(p1, replace)
gr combine p1 p2 p3 p4, col(2) row(2)  graphregion(color(white) fcolor(white)) xsize(6) ysize(7) saving("${output}\Figure A3", replace)
		
*********************
****CLOSE LOGFILE****
*********************

log close
