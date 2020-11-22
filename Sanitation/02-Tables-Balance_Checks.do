/***************************************************************
PROJECT: 		ENCOURAGING SANITATION INVESTMENT IN THE 
				DEVELOPING WORLD: A CLUSTER-RANDOMIZED TRIAL

AUTHORS: 		Raymond Guiteras, James Levinsohn &
				Mushfiq Mobarak
				
CODE AUTHORS: 	Laura Feeney (IPA) & Derek Wolfson (IPA)

PURPOSE: 		Creates Table S1 from Supplementary Materials
****************************************************************/

/* *************************************************
0 - SETUP AND OPTIONS
***************************************************/
set more off
use "${REPO_DATA}/BD-SAN-FINAL.dta", clear

/* *************************************************
1 - RUN BALANCE CHECKS AND OUTPUT TABLE
***************************************************/
mat drop _all
#d ;
gl vars "cen_hh_s_femD cen_hh_s_age cen_hh_s_edu cen_h_s_muslimD cen_h_s_bengaliD cen_hh_s_agriD
	cen_h_i_land_noout eligible  cen_h_i_mealsD cen_h_diarr_hh1wkD
	 cen_h_s_phoneD bl_h_electricityD cen_h_w_tubepipe_always
	bl_any_own bl_any_access bl_hyg_access bl_hyg_own bl_any_od_adults";
gl vil_vars "vil_pop vil_med_land vil_share_landless vil_share_almostlandless vil_share_electricity";
#d cr

bys vid: egen treat_cat_1_median = median(treat_cat_1)
la val treat_cat_1_median treat_cat_1
bys vid: gen VIL=1 if _n==1

foreach var in $vil_vars { // keeping only one observation per village so summary stats work properly
	replace `var' = . if VIL!=1
	}

loc rnames
loc rnames3
foreach var in $vars $vil_vars{
	local lbl : var label `var'
	local rnames `"`rnames' "`lbl'" "SD or SE" "'
	
	local rnames3 `"`rnames3' "`lbl'" "'
	}
	
	
loc cnames1
loc cnames3
loc ceq
levelsof treat_cat_1, local(LEVELS)
foreach l of local LEVELS {

	
	if `l'==0 continue
	local lbl : lab treat_cat_1 `l'
	local cnames1 `"`cnames1' "Mean SD" "Diff SE" "'
	
	local cnames3 `"`cnames3' "`lbl'" "'
	
	local ceq `"`ceq' "`lbl'" "`lbl'" "'
	}
local cnames1 `""Mean (SD)" `cnames1' "F-stat" "P-val" "N" "'
local ceq `""Control" `ceq' "F-test" "F-test" " " "'
	
	
mat drop _all

foreach i in $vars $vil_vars {

if inlist("`i'","vil_pop","vil_med_land","vil_share_landless","vil_share_almostlandless","vil_share_electricity") local treatment treat_cat_1_median
else local treatment treat_cat_1

	* # of obersvations across all treatments, by covariate
	qui tabstat `i', stat(n) save
	qui tabstatmat n
	mat n = n \ .

	forval t = 0/4 {
		* Mean and Std Deviation by treatment, by covariate
		qui tabstat `i' if `treatment'==`t', stat(mean sd)save
		qui tabstatmat Mean`t'		

		if `t'==0 continue 
		
		* Difference between treatment and control, by covariate
		di "`i'"
		di "`:var lab `i''"
		di "`:lab treat_cat_1 `t''"
		if inlist(`t',2,4) loc cluster cid
		if inlist(`t',1,3) | inlist("`i'","vil_pop","vil_med_land","vil_share_landless","vil_share_almostlandless","vil_share_electricity") loc cluster vid
		reg `i' i.`treatment' if inlist(`treatment',0,`t'), cl(`cluster')
		mat reg = r(table)
		mat Diff`t' = reg[1..2, 2] // coefficient and std error of regression
		mat p = reg[4,2]  // p value from the regression above 
		
		*calculate # of stars. 1 = 3 stars 1%, 2 = 2 stars 5%, 3 = 1 star 10%
		scalar star = 4 - ((p[1,1]<.1) + (p[1,1]<.05) + (p[1,1]<.01))
		if star==4 scalar star=0 // change the 4 to a 0 for no stars
		mat stars`t' = star \ 0 	// star and 0 for std error 
		}
	
	local cluster vid
	*calculate f-stat and pvalue for f-stat for test of joint significance of covariate `i'
	qui reg `i' i.`treatment', cl(`cluster')
	mat fpval = Ftail(`e(df_m)', `e(df_r)', `e(F)') \ . // blank below to keep matrix conformatibility
	mat f = `e(F)' \ . 

	*all stats for variable `i'
	mat all = Mean0, Mean1, Diff1, Mean2, Diff2, Mean3, Diff3, Mean4, Diff4, f, fpval, n
	*append all stats for variable `i' to matrix of all variables
	mat ALL = nullmat(ALL) \ all
	
	****to have xml_tab put *s on the last column for significant coefs, manually creating stars matrix****
	mat starblank3 = 0, 0, 0 \ 0, 0, 0
	mat starblank = 0 \ 0
	mat StarsAll = starblank, starblank, stars1, starblank, stars2, starblank, stars3, starblank, stars4, starblank3
	mat ALL_STARS = nullmat(ALL_STARS) \ StarsAll
	}
	
#d ;
xml_tab ALL using "${REPO_OUT}\Tables\Table_S1", replace long 
	title("Summary Statistics - Balance Check")   
	sheet(mean by 2x2, color(1)) rnames(`rnames') cnames(`cnames1') ceq(`ceq') 
	line(COL_NAMES 2 LAST_ROW 13) 
	cwidth(0 170, 1 42, 2 42, 3 42, 4 42, 5 42, 6 42, 7 42, 8 42, 9 42, 10 42, 11 42, 12 42) 
	font("Garamond" 11) format((S2100) (N2202) (N2202) (N2202) (N2202) (N2202) (N2202) (N2202) (N2202) 
	(N2202) (N2202) (N2202) (N2200)) 
	note("Note: *** p<0.01, ** p<0.05, * p<0.1. Column 1 = Control, Column 2/3 = LPP Only, Columns 4/5 = LPP + Subsidy, 
	Columns 6/7 = Supply Only, Columns 8/9 = LPP + Subsidy + Supply. Columns 1, 2, 4, 6 and 8 show mean and standard deviation 
	of household and village indicators by treatment type. Columns 3, 5, 7 and 9 display the difference from 
	control and standard error of the estimated difference. For LPP Only and Supply Only, standard errors are 
	clustered by village to account for village-level randomization. For LPP+Subsidy and LPP+Subsidy+Supply, 
	standard errors are clustered at the neighborhood level to account for neighborhood-level randomization. 
	For the village-level variables, we use the median cluster-level treatment within a village to determine 
	treatment status, since a village may have neighborhoods belonging to both LPP+Subsidy and LPP+Subsidy+Supply. 
	Columns 10-11 display the result of an F-test of joint significance for all treatment indicators. Column 12 
	displays the total sample number for each indicator.") ;
#d cr


