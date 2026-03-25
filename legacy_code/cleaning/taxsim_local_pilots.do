//net from https://taxsim.nber.org/stata
//net install taxsimlocal35,all
set more off

clear



global path "/Users/xinyuc/Documents/pilots"

cd "$path"



foreach dataset in "53-2011mean" "53-2011median" "53-2012mean" "53-2012median" {

	import delimited "$path/data/data_for_taxsim/data_for_taxsim_`dataset'.csv", clear
	
	
	taxsimlocal35, full wages replace 

	save "$path/clean_data/taxsim_output/taxsim_out_`dataset'.dta", replace
			
	
	clear

}


