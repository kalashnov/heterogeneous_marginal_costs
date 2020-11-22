PROJECT: 		ENCOURAGING SANITATION INVESTMENT IN THE 
				DEVELOPING WORLD: A CLUSTER-RANDOMIZED TRIAL

AUTHORS: 		Raymond Guiteras, James Levinsohn &
				Mushfiq Mobarak
				

***********************
1 -- INTRODUCTION
***********************
The files included in this repository reproduce the analysis from 
the paper titled "Encouraging Sanitation Investment in the Developing 
World: A Cluster Randomized Trial".  

For this analysis, we used Stata 13.  The included do-files will 
reproduce all figures and tables found in the main paper and the
supplementary materials.  

The do-files depends on one global value called "REPO_DIR".  This
global is defined on line 23 of 00-Master.do.  Change this global
to the directory where BD_SAN_SCIENCE_REPLICATION folder is located on
your computer.  Run the 00-Master.do file to reproduce all analysis.

See the comments in the 00-Master.do file and the other .do files 
that are called within that program for the specific estimation 
procedures and data processing techniques.

Please direct all questions to Dr. Ahmed Mushfiq Mobarak at 
ahmed.mobarak@yale.edu.

***********************
2 -- INCLUDED FILES
***********************
Below are the files necessary to run the replication.
1 - Code/Do-files
    - 00-Master.do
    - 01-Figures.do
    - 01-Figures-DataPrep.do
    - 02-Tables-Balance_Checks.do
    - 02-Tables-Eligibles_Latrine.do
    - 02-Tables-Eligibles_OD.do
    - 02-Tables-Ineligibles_Latrine.do
    - 02-Tables-Ineligibles_OD.do
2- Dataset
    - BD-SAN-FINAL.dta

***********************
3 -- FOLDER STRUCTURE
***********************
In order to run the code to produce output tables and figures, the folder 
structure needs to be created as follows:
 - User’s working directory is represented as “C:/User/…/”; this path can be 
   set to whatever you determine is best
 - Data folder
   - “C:/User/…/Datasets”
     - Data folder must contain the dataset "BD-SAN-FINAL.dta".
 - Do-files folder
   - “C:/User/…/Code”
     - Do-files folder must contain all do-files included in Section (2) 
       above.
 - Output folder
   - “C:/User/…/Output/”
     - Tables folder
       - “C:/User/…/Output/tables”
     - Figures folder
       - “C:/User/…/Output/figures”
 - Log files folder
   - “C:/User/…/Logs/”

***********************
4 -- ADDITIONAL NOTES
***********************
The user-generated commands which are used in this code are:

1 - outreg2
2 - xml_tab
3 - tabstatmat
4 - labmask
5 - tabout

You can install these using the ssc command in Stata.


***********************
5 -- KNOWN ISSUES
***********************
In testing with Innovations for Poverty Action there was an issue with calls 
of outreg2 and xml_tab.  If this is a problem on your machine, append capture 
to the front of all the outreg2 and xml_tab commands to get the code to run.

For example:
	replace
		outreg2 ...
	with
		capture outreg2 ...
		