
*************************************************************************************************************************************
**** TITLE: "PRICE SUBSIDIES, DIAGNOSTIC TESTS, AND TARGETING OF MALARIA TREATMENT: EVIDENCE FROM A RANDOMIZED CONTROLLED TRIAL" ****
**** DATE: OCTOBER 2016																				      						 ****	
**** REPLICATION FILE																					  						 ****	
*************************************************************************************************************************************


* *************************************************************
* PROGRAM: ACT_WebAppendixH_REPLICATION
* PURPOSE: This code replicates all tables found in Web Appendix H
*	of "Price Subsidies, Diagnostic Tests, and Targeting of 
* 	Malaria Treatment: Evidence from a Randomized Controlled Trial"
* *************************************************************

* CHOOSE 'head_lit' TO MAKE #L TABLES, 'B_knowledge_correct' TO MAKE #K TABLES
  global het "head_lit"
* global het "B_knowledge_correct"


* FILE NAMES
	global all_main ACT_AllMain_FINAL_pub 
	global all_ill_prob ACT_IllLvlMainWithMalProbs_FINAL_pub
	global hh_follow_all ACT_HHFollowUp_All_FINAL_pub
	global chem_main_long ACT_PharmLogPos_FINAL_pub
	global baselines_mal ACT_BaselineMal_FINAL_pub
	global pharma_txn ACT_NonProjectTxns_FINAL_pub
	

* enter the path to the replication datasets below
	global finaldata "YOUR PATH"
	global dofiles "YOUR PATH"
	global output "YOUR PATH"
	global logfiles "YOUR PATH"

**********************
****CREATE LOGFILE****
**********************

log using "${logfiles}\ACT_WebAppendixH", smcl replace

cd "$finaldata"
	
* SET ADDITIONAL CONTROLS FOR SPECS
	global cont "B_head_age_imputed B_head_age_missing"

clear
set more off 
set mem 500m
set matsize 1000

* **************************************************************************************
* TABLE 2 :: IMPACT OF ACT AND RDT SUBSIDY ON CARE SEEKING, ACCESS
* **************************************************************************************
use $all_ill_prob, clear
	set more off
	keep if  first_ep==1 
 	keep if ex_post==0 & rdt_any==0
	
g act_any= act40 | act60 | act100

foreach var in act_any act40 act60 act100 {
	g het_`var'= ${het}*`var'
	}

mat t= J(75,13,.)
local rowst=1
foreach group in all {
local col=1
	foreach out in took_act took_act_chem took_act_hc care_chem care_hc care_nothing took_maltest took_antibio took_subst {
	local row=`rowst'
	
* SPECIFICATION 1 (POOLED)
reg `out' act_any het_act_any $het i.totstrata ex_post $cont if `group'==1, clu(househo)
		foreach v in act_any het_act_any $het  {
			mat t[`row',`col']= _b[`v']
				local ++row
			mat t[`row',`col']= _se[`v']
				local ++row
			}

* SPECIFICATION 2
	reg `out' act40 act60 act100 het_act40 het_act60 het_act100 $het i.totstrata ex_post $cont if `group'==1, clu(househo)
		foreach v in act40 act60 act100 het_act40 het_act60 het_act100 $het {
			mat t[`row',`col']= _b[`v']
				local ++row
			mat t[`row',`col']= _se[`v']
				local ++row
			}
			
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
use $all_ill_prob, clear
	keep if ex_post==0 & rdt_any==0 & took_act==1
	keep if first_ep==1
	drop if act500
	g group=2
	tempfile endline
		save "`endline'"

use $chem_main_long, clear
	keep if ex_post==1 & used_act==1
	drop if rdt_any==1
	drop if act500 
	ren LOG_patient_age LOG_patient_age1
	ren used_act_adult adult
	g group=1
	tempfile tomerge
		save "`tomerge'"
	
use $all_main, clear
	set more off
	drop if act500
	drop if rdt_any==1 | ex_post==0
	keep if used_act==1
	replace LOG_mal_prob21=. if rdt_pos==. | ex_post==0
	ren used_act_adult adult
	g group=0
	append using "`tomerge'"	
	append using "`endline'"

egen test= mean(${het}), by(househo)
	replace ${het} = test if group!=0
	drop test
		
replace all=1
g young= adult==0 

foreach var in act60 act100 {
	g het_`var'= `var'*${het}
	}
	
g rdt_posA= rdt_pos if group==0
g rdt_posB= rdt_pos if group==1
g mal_probA= LOG_mal_prob21 if group==0
g mal_probB= LOG_mal_prob2 if group==1
g mal_probC= mal_prob2 if group==2

mat t=J(60,10,.)
local col=1
  foreach take in rdt_posA mal_probA mal_probC {	
	local row=1
	
	foreach type in all {	
		xi: reg `take' act60 act100 het_act60 het_act100 $het if `type'==1, clu(househo)
			foreach var in act60 act100 het_act60 het_act100 $het {
				mat t[`row',`col']= _b[`var']
					local ++row
				mat t[`row',`col']= _se[`var']
				local ++row
				}
	qui test act60=act100==0	
	mat t[`row',`col']= r(p)
		local ++row
	mat t[`row',`col']= r(p)
		local ++row
	qui sum `take' if e(sample) & act40 & rdt_any==0 & ${het}==0
		mat t[`row',`col']= r(mean)
			local ++row
		mat t[`row',`col']= e(N)
			local ++row
	}	
	local ++col
	}	
mat li t


* **************************************************************************************
* TABLE 4 :: MECHANISMS TABLE
* **************************************************************************************
use $all_main, clear
	set more off
	drop if act500 | rdt_any==1
	
foreach var in act60 act100 {
	g het_`var'= `var'*${het}
	}
	
		g used_act_young= used_act_baby+used_act_kid+used_act_teen
		g frac_adult= used_act_adult if used_act==1
		g rdt_pos_young= rdt_pos if ex_post & used_act_young==1
		g rdt_pos_adult= rdt_pos if ex_post & used_act_adult==1
		g finalout= rdt_pos if ex_post & used_act==1
		
local col=1	
mat t=J(25,8,.)
foreach out in used_act_young used_act_adult frac_adult rdt_pos_young rdt_pos_adult finalout {
local cme "i.totstrata $cont"
	if "`out'"=="rdt_pos_adult" | "`out'"=="rdt_pos_young" | "`out'"=="frac_adult" local cme ""
	if "`out'"=="finalout" local cme "used_act_adult used_act_kid used_act_teen"
	
	local row=1
	reg `out' act60 act100 het_act60 het_act100 $het `cme', r
		foreach price in act60 act100 het_act60 het_act100 $het {
			mat t[`row',`col']= _b[`price']
				local ++row
			mat t[`row',`col']= _se[`price']
				local ++row
			}
			
			test act60=act100=0
			mat t[`row',`col']= r(p)
				local ++row
			mat t[`row',`col']= r(p)
				local ++row
			
			qui sum `out' if e(sample) & act40 & ${het}==0
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
use $all_ill_prob, clear
	set more off
	keep if  first_ep==1
 	drop if act500
 	keep if rdt_any==1 | ex_post==0

foreach p in act40 act60 act100 {
	g R`p'= rdt_any*`p'
	}
	
foreach var in Ract40 Ract60 Ract100 act60 act100 rdt_any {
	g het_`var'= ${het}*`var'
	}		

mat t= J(58,13,.)
local rowst=1
foreach group in all {
local col=1
	foreach out in care_chem care_hc care_nothing took_maltest took_rdt took_micro took_act took_antibio took_subst {
	local row=`rowst'
* SPECIFICATION 1
	reg `out' rdt_any act60 act100 het_rdt_any het_act60 het_act100 $het i.totstrata $cont if `group'==1, clu(househo)
		foreach v in rdt_any het_rdt_any $het {
			mat t[`row',`col']= _b[`v']
				local ++row
			mat t[`row',`col']= _se[`v']
				local ++row
			}
			
		qui sum `out' if `group'==1 & rdt_any==0 & e(sample) 
			mat t[`row',`col']= r(mean)
				local ++row
			mat t[`row',`col']= e(N)
				local ++row
* SPECIFICATION 2
	reg `out' Ract40 Ract60 Ract100 act60 act100 het_Ract40 het_Ract60 het_Ract100 het_act60 het_act100 $het i.totstrata $cont if `group'==1, clu(househo)
		foreach v in Ract40 Ract60 Ract100 het_Ract40 het_Ract60 het_Ract100 $het {
			mat t[`row',`col']= _b[`v']
				local ++row
			mat t[`row',`col']= _se[`v']
				local ++row
			}
			
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
set more off
use $all_main, clear

drop if act500
	
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
	
foreach var in rdt_any Ract40 Ract60 Ract100 act60 act100 {
	g het_`var'= ${het}*`var'
	}		
	
local rowct=1
local col=1
mat t=J(40,8,.)	
foreach group in all {
foreach out in sought_treat rdt_pos_trt rdt_pos_act {
	ren `out' `out'_all
	local row=`rowct'

	local e1 ""
		if "`out'"=="sought_treat" local e1 "i.totstrata $cont"
		
	reg `out'_`group' rdt_any act60 act100 het_rdt_any het_act60 het_act100 $het `e1', r
		foreach cov in rdt_any het_rdt_any $het {
			mat t[`row',`col']= _b[`cov']
				local ++row
			mat t[`row',`col']= _se[`cov']
				local ++row
			}
			
	reg `out'_`group' Ract40 Ract60 Ract100 act60 act100 het_Ract40 het_Ract60 het_Ract100 het_act60 het_act100 $het `e1', r
		foreach cov in Ract40 Ract60 Ract100 het_Ract40 het_Ract60 het_Ract100 $het {
			mat t[`row',`col']= _b[`cov']
				local ++row
			mat t[`row',`col']= _se[`cov']
				local ++row
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
		
	sum `out'_`group' if rdt_any==1	& ${het}==0	
			mat t[`row',`col']= r(mean)
				local ++row
				local ++row
	sum `out'_`group' if rdt_any==1	& ${het}==1	
			mat t[`row',`col']= r(mean)
				local row= `row'+4
	

	foreach h in 0 1 {
	foreach val in act40 act60 act100 {		
		sum `out'_`group' if rdt_any==1	& `val'==1 & ${het}==`h'
			mat t[`row',`col']= r(mean)
				local ++row
				local ++row
				}
				}
			local ++row
	ren `out'_all `out'
	local ++col
	}				
	local rowct=`row'+1
	local col=1
	}

mat li t

clear

* CHOOSE 'head_lit' TO MAKE #L TABLES, 'B_knowledge_correct' TO MAKE #K TABLES
* global het "head_lit"
  global het "B_knowledge_correct"


* FILE NAMES
	global all_main ACT_AllMain_FINAL_pub 
	global all_ill_prob ACT_IllLvlMainWithMalProbs_FINAL_pub
	global hh_follow_all ACT_HHFollowUp_All_FINAL_pub
	global chem_main_long ACT_PharmLogPos_FINAL_pub
	global baselines_mal ACT_BaselineMal_FINAL_pub
	global pharma_txn ACT_NonProjectTxns_FINAL_pub
	

* enter the path to the replication datasets below
	global finaldata "C:\Users\hbadani\Box Sync\Shared data\IPA Studies\119 - Dupas, Cohen, Schaner - Price Subsidies, Diagnostic Tests, and Targeting of Malaria Treatment\workingfiles\finaldata"
	global dofiles "C:\Users\hbadani\Box Sync\Shared data\IPA Studies\119 - Dupas, Cohen, Schaner - Price Subsidies, Diagnostic Tests, and Targeting of Malaria Treatment\workingfiles\dofiles"
	global output "C:\Users\hbadani\Box Sync\Shared data\IPA Studies\119 - Dupas, Cohen, Schaner - Price Subsidies, Diagnostic Tests, and Targeting of Malaria Treatment\workingfiles\output"
	global logfiles "C:\Users\hbadani\Box Sync\Shared data\IPA Studies\119 - Dupas, Cohen, Schaner - Price Subsidies, Diagnostic Tests, and Targeting of Malaria Treatment\workingfiles\logfiles"

cd "$finaldata"
	
* SET ADDITIONAL CONTROLS FOR SPECS
	global cont "B_head_age_imputed B_head_age_missing"

clear
set more off 
set mem 500m
set matsize 1000

* **************************************************************************************
* TABLE 2 :: IMPACT OF ACT AND RDT SUBSIDY ON CARE SEEKING, ACCESS
* **************************************************************************************
use $all_ill_prob, clear
	set more off
	keep if  first_ep==1 
 	keep if ex_post==0 & rdt_any==0
	
g act_any= act40 | act60 | act100

foreach var in act_any act40 act60 act100 {
	g het_`var'= ${het}*`var'
	}

mat t= J(75,13,.)
local rowst=1
foreach group in all {
local col=1
	foreach out in took_act took_act_chem took_act_hc care_chem care_hc care_nothing took_maltest took_antibio took_subst {
	local row=`rowst'
	
* SPECIFICATION 1 (POOLED)
reg `out' act_any het_act_any $het i.totstrata ex_post $cont if `group'==1, clu(househo)
		foreach v in act_any het_act_any $het  {
			mat t[`row',`col']= _b[`v']
				local ++row
			mat t[`row',`col']= _se[`v']
				local ++row
			}

* SPECIFICATION 2
	reg `out' act40 act60 act100 het_act40 het_act60 het_act100 $het i.totstrata ex_post $cont if `group'==1, clu(househo)
		foreach v in act40 act60 act100 het_act40 het_act60 het_act100 $het {
			mat t[`row',`col']= _b[`v']
				local ++row
			mat t[`row',`col']= _se[`v']
				local ++row
			}
			
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
use $all_ill_prob, clear
	keep if ex_post==0 & rdt_any==0 & took_act==1
	keep if first_ep==1
	drop if act500
	g group=2
	tempfile endline
		save "`endline'"

use $chem_main_long, clear
	keep if ex_post==1 & used_act==1
	drop if rdt_any==1
	drop if act500 
	ren LOG_patient_age LOG_patient_age1
	ren used_act_adult adult
	g group=1
	tempfile tomerge
		save "`tomerge'"
	
use $all_main, clear
	set more off
	drop if act500
	drop if rdt_any==1 | ex_post==0
	keep if used_act==1
	replace LOG_mal_prob21=. if rdt_pos==. | ex_post==0
	ren used_act_adult adult
	g group=0
	append using "`tomerge'"	
	append using "`endline'"

egen test= mean(${het}), by(househo)
	replace ${het} = test if group!=0
	drop test
		
replace all=1
g young= adult==0 

foreach var in act60 act100 {
	g het_`var'= `var'*${het}
	}
	
g rdt_posA= rdt_pos if group==0
g rdt_posB= rdt_pos if group==1
g mal_probA= LOG_mal_prob21 if group==0
g mal_probB= LOG_mal_prob2 if group==1
g mal_probC= mal_prob2 if group==2

mat t=J(60,10,.)
local col=1
  foreach take in rdt_posA mal_probA mal_probC {	
	local row=1
	
	foreach type in all {	
		xi: reg `take' act60 act100 het_act60 het_act100 $het if `type'==1, clu(househo)
			foreach var in act60 act100 het_act60 het_act100 $het {
				mat t[`row',`col']= _b[`var']
					local ++row
				mat t[`row',`col']= _se[`var']
				local ++row
				}
	qui test act60=act100==0	
	mat t[`row',`col']= r(p)
		local ++row
	mat t[`row',`col']= r(p)
		local ++row
	qui sum `take' if e(sample) & act40 & rdt_any==0 & ${het}==0
		mat t[`row',`col']= r(mean)
			local ++row
		mat t[`row',`col']= e(N)
			local ++row
	}	
	local ++col
	}	
mat li t


* **************************************************************************************
* TABLE 4 :: MECHANISMS TABLE
* **************************************************************************************
use $all_main, clear
	set more off
	drop if act500 | rdt_any==1
	
foreach var in act60 act100 {
	g het_`var'= `var'*${het}
	}
	
		g used_act_young= used_act_baby+used_act_kid+used_act_teen
		g frac_adult= used_act_adult if used_act==1
		g rdt_pos_young= rdt_pos if ex_post & used_act_young==1
		g rdt_pos_adult= rdt_pos if ex_post & used_act_adult==1
		g finalout= rdt_pos if ex_post & used_act==1
		
local col=1	
mat t=J(25,8,.)
foreach out in used_act_young used_act_adult frac_adult rdt_pos_young rdt_pos_adult finalout {
local cme "i.totstrata $cont"
	if "`out'"=="rdt_pos_adult" | "`out'"=="rdt_pos_young" | "`out'"=="frac_adult" local cme ""
	if "`out'"=="finalout" local cme "used_act_adult used_act_kid used_act_teen"
	
	local row=1
	reg `out' act60 act100 het_act60 het_act100 $het `cme', r
		foreach price in act60 act100 het_act60 het_act100 $het {
			mat t[`row',`col']= _b[`price']
				local ++row
			mat t[`row',`col']= _se[`price']
				local ++row
			}
			
			test act60=act100=0
			mat t[`row',`col']= r(p)
				local ++row
			mat t[`row',`col']= r(p)
				local ++row
			
			qui sum `out' if e(sample) & act40 & ${het}==0
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
use $all_ill_prob, clear
	set more off
	keep if  first_ep==1
 	drop if act500
 	keep if rdt_any==1 | ex_post==0

foreach p in act40 act60 act100 {
	g R`p'= rdt_any*`p'
	}
	
foreach var in Ract40 Ract60 Ract100 act60 act100 rdt_any {
	g het_`var'= ${het}*`var'
	}		

mat t= J(58,13,.)
local rowst=1
foreach group in all {
local col=1
	foreach out in care_chem care_hc care_nothing took_maltest took_rdt took_micro took_act took_antibio took_subst {
	local row=`rowst'
* SPECIFICATION 1
	reg `out' rdt_any act60 act100 het_rdt_any het_act60 het_act100 $het i.totstrata $cont if `group'==1, clu(househo)
		foreach v in rdt_any het_rdt_any $het {
			mat t[`row',`col']= _b[`v']
				local ++row
			mat t[`row',`col']= _se[`v']
				local ++row
			}
			
		qui sum `out' if `group'==1 & rdt_any==0 & e(sample) 
			mat t[`row',`col']= r(mean)
				local ++row
			mat t[`row',`col']= e(N)
				local ++row
* SPECIFICATION 2
	reg `out' Ract40 Ract60 Ract100 act60 act100 het_Ract40 het_Ract60 het_Ract100 het_act60 het_act100 $het i.totstrata $cont if `group'==1, clu(househo)
		foreach v in Ract40 Ract60 Ract100 het_Ract40 het_Ract60 het_Ract100 $het {
			mat t[`row',`col']= _b[`v']
				local ++row
			mat t[`row',`col']= _se[`v']
				local ++row
			}
			
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
set more off
use $all_main, clear

drop if act500
	
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
	
foreach var in rdt_any Ract40 Ract60 Ract100 act60 act100 {
	g het_`var'= ${het}*`var'
	}		
	
local rowct=1
local col=1
mat t=J(40,8,.)	
foreach group in all {
foreach out in sought_treat rdt_pos_trt rdt_pos_act {
	ren `out' `out'_all
	local row=`rowct'

	local e1 ""
		if "`out'"=="sought_treat" local e1 "i.totstrata $cont"
		
	reg `out'_`group' rdt_any act60 act100 het_rdt_any het_act60 het_act100 $het `e1', r
		foreach cov in rdt_any het_rdt_any $het {
			mat t[`row',`col']= _b[`cov']
				local ++row
			mat t[`row',`col']= _se[`cov']
				local ++row
			}
			
	reg `out'_`group' Ract40 Ract60 Ract100 act60 act100 het_Ract40 het_Ract60 het_Ract100 het_act60 het_act100 $het `e1', r
		foreach cov in Ract40 Ract60 Ract100 het_Ract40 het_Ract60 het_Ract100 $het {
			mat t[`row',`col']= _b[`cov']
				local ++row
			mat t[`row',`col']= _se[`cov']
				local ++row
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
		
	sum `out'_`group' if rdt_any==1	& ${het}==0	
			mat t[`row',`col']= r(mean)
				local ++row
				local ++row
	sum `out'_`group' if rdt_any==1	& ${het}==1	
			mat t[`row',`col']= r(mean)
				local row= `row'+4
	

	foreach h in 0 1 {
	foreach val in act40 act60 act100 {		
		sum `out'_`group' if rdt_any==1	& `val'==1 & ${het}==`h'
			mat t[`row',`col']= r(mean)
				local ++row
				local ++row
				}
				}
			local ++row
	ren `out'_all `out'
	local ++col
	}				
	local rowct=`row'+1
	local col=1
	}

mat li t
	
*********************
****CLOSE LOGFILE****
*********************

log close
