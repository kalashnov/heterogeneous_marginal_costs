/***************************************************************
PROJECT: 		ENCOURAGING SANITATION INVESTMENT IN THE 
				DEVELOPING WORLD: A CLUSTER-RANDOMIZED TRIAL

AUTHORS: 		Raymond Guiteras, James Levinsohn &
				Mushfiq Mobarak
				
CODE AUTHORS: 	Laura Feeney (IPA) & Derek Wolfson (IPA)

PURPOSE: 		Run all .do files necessary for reproduction of 
				results featured in the Science paper.
****************************************************************/

/* **************************************
0 - SETUP
****************************************/
clear
set more off
cap log close
cap file close _all

**MANAGE DIRECTORIES
global REPO_DIR 	"Y:\BD_SAN_SCIENCE_REPLICATION\Code" //CHANGE THIS TO WHERE YOUR REPRODUCTION FOLDER IS
global REPO_LOG		"$REPO_DIR\Logs"
global REPO_DATA 	"$REPO_DIR\Datasets"
global REPO_CODE 	"$REPO_DIR\Code\"
global REPO_OUT		"$REPO_DIR\Output"

/* **************************************
1 - FIGURES
****************************************/

do "${REPO_CODE}\01-Figures.do"
//CREATES ALL MAIN TEXT AND SUPPLMENTARY FIGURES
//DEPENDENCIES: ${REPO_CODE}\01-Figures-DataPrep.do

/* **************************************
2 - TABLES
****************************************/

do "${REPO_CODE}\02-Tables-Balance_Checks.do"
//CREATES TABLE S1

do "${REPO_CODE}\02-Tables-Eligibles_Latrine.do"
//CREATES TABLES S2 & S4 

do "${REPO_CODE}\02-Tables-Eligibles_OD.do"
//CREATES TABLES S3 & S5

do "${REPO_CODE}\02-Tables-Ineligibles_Latrine.do"
//CREATES TABLES S6 & S7

do "${REPO_CODE}\02-Tables-Ineligibles_OD.do"
//CREATES TABLES S8 & S9
