*! cto_read.ado - Stata module to import and minimally clean SurveyCTO data
*! Author: Michael Rozelle <michael.rozelle@wur.nl>
*! Version 0.0.2  Modified:  March 2023

// Drop the cto_read program if it already exists
cap program drop cto_read

// Define the cto_read program, which takes three arguments
program define cto_read, rclass
// instrument, then dataset, then dofile
syntax, ///
	INSTname(string) ///
	DATAname(string) ///
	DOfile(string) ///
	[RESHAPEfile(string) ///
	IDENTIFIERS(namelist) ///
	AMERICAN ///
	SAVEfolder(string)]
 
version 17

// Create a quiet environment to prevent unnecessary output
qui { 

// Reset any previously defined frames
frames reset

if "`identifiers'" == "" {
	
	local identifying_vars
	
}
else {
	
	local identifying_vars `identifiers'
	
}

if "`american'" == "" {
	
	local datestyle DMY
	
}
else {
	
	local datestyle MDY
	
}

if "`reshapefile'" != "" {
	
	local want_reshape = 1
	
}
else {
	
	local want_reshape = 0
	
}

*===============================================================================
* 	Import XLSforms
*===============================================================================

/* We're going to work in two frames - there's the "survey" sheet containing the 
questions, enablement conditions, and so on, as well as the "choices" sheet,
which gives us all the value labels. Rather than open and close a million datasets,
frames let us work on both these levels simultaneously.
*/

*===============================================================================
* 	The Data
*===============================================================================\

// Create a new frame called "rawdata" to hold the raw data
frame create rawdata 
frame change rawdata

// Import the raw data from the CSV file specified by the "dataname" variable
import delimited "`dataname'", clear bindquote(strict)

// Create a new frame called "qs" to hold the survey questions
frame create qs 
frame change qs 

// Import the "survey" sheet from the instrument Excel file specified by the "instname" variable
import excel "`instname'", firstrow clear sheet(survey) 

// Loop over a list of variables and ensure that they exist in the dataset
foreach v in type name calculation relevant repeat_count {
	cap confirm variable `v'
	cap tostring `v', replace
	if (_rc) {
		local missvars `missvars' `v'
		continue
	}
	else {
		local keepvars `keepvars' `v'
	}
}

cap confirm variable label 

if !_rc {
	
	rename label labelEnglishen
	clonevar labelStata = labelEnglishen
	
}
else {
	
	cap tostring labelStata, replace
	
}

// Keep only the variables that exist in the dataset
keep `keepvars' labelEnglishen labelStata

// Display a warning if any variables are missing
if "`missvars'" != "" {
	noisily display as result "You are possibly missing the variable(s): `missvars'!"
}

// Replace any dollar signs in the "labelEnglishen", "labelStata", "repeat_count", and "relevant" variables with pound signs
foreach v of varlist labelEnglishen labelStata repeat_count relevant {
	replace `v' = subinstr(`v', "$", "#", .)
}

// Replace any missing Stata labels with the English labels
replace labelStata="" if labelStata=="."
replace labelStata = labelEnglishen if missing(labelStata)

*===============================================================================
* 	Choices
*===============================================================================

// Create a new frame called "choices" to hold the value labels
frame create choices 
frame change choices 

// Import the "choices" sheet from the instrument Excel file specified by the "instname" variable
import excel "`instname'", firstrow clear sheet(choices)

// Rename the "listname" variable to "list_name" for consistency
cap rename listname list_name 

// Keep only the "list_name", "name", and "label" variables
keep list_name name label 

// Remove any empty rows from the dataset
missings dropobs, force 

// Remove any rows where the "name" variable is not a number (i.e. non-standard labeling)
drop if !regexm(name, "^[0-9]+$") 

// Create a new variable called "order" to retain the original order of variables in the instrument
gen order = _n 

// Create a clone of the "name" variable for programming purposes
clonevar name1 = name 

// Replace any minus signs in the "name" variable with underscores, to match how SurveyCTO handles value labels
replace name = subinstr(name, "-", "_", 1) 

// Replace any dollar signs in the "label" variable with pound signs, to prevent Stata from parsing them as macros
replace label = subinstr(label, "$", "#", .)

// Remove any double quotes from the "label" variable
replace label = subinstr(label, `"""', "", .)

// Remove any spaces from the "list_name" variable
replace list_name = subinstr(list_name, " ", "", .)

// Create a local macro called "brek" containing a line break character
local brek = char(10) 

// Remove any line breaks from the "name" and "name1" variables
foreach var of varlist name name1 {
	replace `var' = subinstr(`var', "`brek'", "", .)
}

// Loop above gets rid of any line breaks where we don't want them

// Loop over the unique values of the "list_name" variable and generate a Stata value label for each one
local counter = 1
levelsof list_name, local(list) clean separate( )
foreach listo in `list' {
	local brek = char(10)
	local cmd "label define `listo' /// `brek'"
	sum order if list_name=="`listo'"
	if "`r(min)'"=="" {
		continue
	}
	else {
		local min = `r(min)'
		local max = `r(max)'
		forvalues i = `min'/`max' {
			if `i' ==`max' {
				local brek
			}
			levelsof name1 if order==`i', local(vals) clean
			levelsof label if order==`i', local(label) clean
			local cmd `"`cmd' `vals' "`label'" /// `brek'"'
		}
	local lab`counter' "`cmd'"
	local ++counter 
	}
}

// Count the number of value label locals generated by the loop above
local labels_counter = `counter'-1

*===============================================================================
* 	Some Tidying
*===============================================================================

// Switch back to the "questions" frame
frame change qs 

// Create a new variable called "order" to retain the original order of variables in the instrument
gen order = _n

// Create a local macro called "brek" containing a line break character
local brek = char(10) 

// Remove any line breaks, dollar signs, and double quotes from the "labelEnglishen", "labelStata", and "relevant" variables
foreach var of varlist labelEnglishen labelStata relevant {
	replace `var' = subinstr(`var', "`brek'", "", .)
	replace `var' = subinstr(`var', "$", "#", .) 
	replace `var' = subinstr(`var', `"""', "", .)
	/* Stata will intepret dollar signs as globals, and as a result, we won't 
	be able to see the questions being referenced for constraints (or even 
	inside the question itself). By changing the dollar sign to a #, we can 
	still see these references in the variable labels and notes */
}

// Replace any full stops in variable names with an empty string
replace name = subinstr(name, ".", "", .) 

// Convert all variable names to lower case
replace name = lower(name) 

// Split the "type" variable into two variables, "type" and "type2"
split type 
// Note that "type2" will contain the value label name for categorical variables.

*===============================================================================
* 	Repeat Groups
*===============================================================================

/* 
These next few loops tell us which groups each question belongs to. This helps
 us tell if a question exists within a loop, or a specific block of questions 
 with an enablement condition, etc.
*/

frame create repeat_groups


foreach jump in begin end { // identify the begin and end repeat sections
	count if type=="`jump'_repeat"
	local repeat_groups = `r(N)' // number of repeat sections
	levelsof order if type=="`jump'_repeat", local(`jump') separate( ) // the rows where those sections are in the instrument
	tokenize "``jump''", parse( ) // create numbered macros containing where those rows are
	forvalues num = 1/`repeat_groups' {
		local `jump'_`num' ``num'' // this will be useful in the next loop
	}
}

frame repeat_groups { 
	
	set obs `repeat_groups'
	gen repeat_group = _n
	gen start = .
	gen end = .
	gen repeat_var = ""
	gen repeat_var_pos = .
	gen repeat_var_nested = 0
	gen function = ""
	gen repetitions = .
	gen repeat_count =""
	
}

forvalues i = 1/`repeat_groups' {
	local taken_`i' = 0 // this local will indicate whether an "end_repeat" has already been assigned to a specific repeat group
	gen dataset_`i' = "global shape_`i' " // this variable, much later on, will contain the full list of variables required to reshape the repeat group to long format
}

forvalues i = `repeat_groups'(-1)1 { // for each begin_repeat

	forvalues j = `repeat_groups'(-1)1 { // for each end_repeat 
		local true = (`end_`j'' > `begin_`i'') & `taken_`j''==0 // evaluates to 1 if the end_repeat is after the begin_repeat and hasn't already been taken
		if `true' {
			local k = `j' // potential match. But we will try lower values of repeat blocks to check if there is another end_repeat that is closer
		}
	}
	
	local rvar 
	local rvar2
	local taken_`k' = 1 // we have assigned this end_repeat
	gen repeat_group_`i' = 1  in `begin_`i''/`end_`k'' // gen dummy=1 if question is inside repeat block
	bysort repeat_group_`i' (order): gen repetitions_`i' = repeat_count[1] // repeat counts should always be the value of a particular variable, so use "calculate" in the form where possible
	cap destring repetitions_`i', replace
	levelsof repetitions_`i', clean local(darep)
	
	if regexm("`darep'", "#{")==1 {
		
		moss repetitions_`i', match("{(.*)}") regex prefix(XYZ) // isolate the part of the string between the two curly brackets (the variable name!)
		local end = XYZpos1 - 1
		gen function_`i' = substr(repetitions_`i', 1, `end') // the function acting on the repeat count variable
		replace function_`i' = "" if function_`i'=="#{"
		drop repetitions_`i'
		rename XYZmatch1 repetitions_`i' // now we have a variable containing the name of the variable whose maximum value in the actual data will determine the maximum number of loops observed in the survey
		replace repetitions_`i' = lower(repetitions_`i') // SurveyCTO always exports variable names as lower case
		drop XYZ*
		levelsof function_`i', local(rvar2) clean
		
	}
		
	frame repeat_groups {
		replace start = `begin_`i'' in `i'
		replace end = `end_`k'' in `i'
		frame qs: levelsof repetitions_`i', local(rvar) clean
		replace repeat_var = "`rvar'" in `i'
		frame qs: noisily levelsof repeat_count if order== `begin_`i'', clean local(repeat_count)
		replace repeat_count = "`repeat_count'" in `i'
		
		if "`repeat_count'"== "." | "`repeat_count'"== "" {
			
			replace repeat_var = "20" in `i'
			replace repetitions = 20 in `i'
			
		}
		
		else if "`rvar2'" != "" {
			replace function = "`rvar2'" in `i'
			frame qs: sum order if name=="`rvar'"
			replace repeat_var_pos = `r(max)' in `i'
		}
		
		else if regexm("`rvar'", "[A-Za-z]")==0 {
			replace repeat_var = "`rvar'" in `i'
			replace repetitions = `rvar' in `i'
		}
		
		else {
			
			frame qs: sum order if name=="`rvar'"
			replace repeat_var_pos = `r(max)' in `i'
			
		}
		
	}
		
	sort order
}

frame repeat_groups {
	
	gen nested = .
	local rpt_grp_chkr = `repeat_groups' - 1
		
	forvalues check_row = 1/`rpt_grp_chkr' {
		
		tempvar potential_nest
		gen `potential_nest' = _n - `check_row'
		
		gen nest_`check_row' = .
		replace nest_`check_row' = `potential_nest' if end[_n] < end[_n-`check_row'] & !missing(end[_n-`check_row'])
		replace nested = end[_n] < end[_n-`check_row'] & !missing(end[_n-`check_row']) if nested !=1 // indicates whether repeat group was nested
		
	}

	
	forvalues i = 1/`repeat_groups' {
		levelsof start in `i', clean local(start)
		levelsof end in `i', clean local(end)
		replace repeat_var_nested = `i' if `start' <= repeat_var_pos & repeat_var_pos <= `end' // variable to show the loop in which the repeat var is found
		levelsof repeat_var in `i', clean local(rep_var)
		levelsof function in `i', clean local(function)
		
		sum repeat_var_nested in `i'
		if `r(max)'!=0 { // if repeat variable is inside a loop in the survey
			local rep_var2 `rep_var'
			sum repetitions in `r(max)' // how many times was the loop containing the repeat variable looped over?
			
			forvalues g = 1/`r(max)' { // for every repeat of the repeat variable
				
				local rep_var `rep_var2'_`g'
				
				if regexm("`rep_var'", "[a-z]")==1 & "`function'"=="" { // if the repeat count is just a variable name with no calculation
				
					frame rawdata : cap destring `rep_var', replace // destring the repeat variable, in case it is a string
					frame rawdata: sum `rep_var'
					if `r(N)' > 0 {
						replace repetitions = `r(max)' in `i' if `r(max)'>repetitions[`i'] | missing(repetitions[`i']) // the maximum number of repeats of this group
					}
					else {
						replace repetitions = 0 in `i' if missing(repetitions)
					}
				
				}	
					
				else if regexm("`rep_var'", "[a-z]")==0 { // what if it's not a variable at all, and it's in fact just a fixed number of repetitions?
				
					replace repetitions = `rep_var' in `i' // then `rep_var' will just contain a number
					
				}
					
				else if "`function'" == "count-selected(#{" { // if the function on the repeat count variable is the number of options selected at that question
					
					frame rawdata: cap tostring `rep_var', replace
					frame rawdata: cap gen XYZ = wordcount(`rep_var')
					frame rawdata: if _rc {
						gen XYZ = 0
					}
					frame rawdata: sum XYZ
					replace repetitions = `r(max)' in `i' if `r(max)'>repetitions[`i'] | missing(repetitions[`i'])
					frame rawdata: drop XYZ
					
				}
				
				else if "`function'" == "min(#{" { // if the function on the repeat count variable is the minimum of a variable and a constant
				
					levelsof repeat_count in `i', clean local(reps)
					local vnum = subinstr("`reps'", "min(#{`rep_var'},", "", .)
					local vnum = ustrregexra("`vnum'", "\D", "")
					replace repetitions = `vnum' in `i' if `r(max)'>repetitions[`i'] | missing(repetitions[`i'])
					
				}
				
				else if "`function'" == "max(#{" { // if the function on the repeat count variable is the maximum of a variable and a constant
					levelsof repeat_count in `i', clean local(reps)
					local vnum = subinstr("`reps'", "max(#{`rep_var'},", "", .)
					local vnum = ustrregexra("`vnum'", "\D", "")
					replace repetitions = `vnum' in `i' if missing(repetitions[`i'])
					
				}
				
				
				else if "`function'" == "count(#{" { // if the function on the repeat count variable is the worst surveycto function of them all, the repeat count of a previous section
				
					frame qs: sum order if name=="`rep_var2'"
					local culprit = `r(max)'
					sum repetitions if start == `culprit'
					replace repetitions = `r(max)' in `i' if `r(max)'>repetitions[`i'] | missing(repetitions[`i'])
				
				}
			}
		}
		
		else {
		
			if regexm("`rep_var'", "[a-z]")==1 & "`function'"=="" { // if the repeat count is just a variable name with no calculation
			
				frame rawdata : cap destring `rep_var', replace // destring the repeat variable, in case it is a string
				frame rawdata: sum `rep_var'
				if `r(N)' > 0 {
						replace repetitions = `r(max)' in `i' // the maximum number of repeats of this group
					}
					else {
						replace repetitions = 0 in `i'
					}
				
			}
						
			else if regexm("`rep_var'", "[a-z]")==0 { // what if it's not a variable at all, and it's in fact just a fixed number of repetitions?
			
				replace repetitions = `rep_var' in `i' // then `rep_var' will just contain a number
			}
						
			else if "`function'" == "count-selected(#{" { // if the function on the repeat count variable is the number of options selected at that question
				
				frame rawdata: cap tostring `rep_var', replace
				frame rawdata: gen XYZ = wordcount(`rep_var')
				frame rawdata: sum XYZ
				replace repetitions = `r(max)' in `i'
				frame rawdata: drop XYZ
				
			}
			
			else if "`function'" == "min(#{" { // if the function on the repeat count variable is the minimum of a variable and a constant
				
					levelsof repeat_count in `i', clean local(reps)
					local vnum = subinstr("`reps'", "min(#{`rep_var'},", "", .)
					local vnum = ustrregexra("`vnum'", "\D", "")
					replace repetitions = `vnum' in `i'
					
				}
				
			else if "`function'" == "count(#{" { // if the function on the repeat count variable is the worst surveycto function of them all, the repeat count of a previous section
				
				frame qs: sum order if name=="`rep_var'"
				local culprit = `r(max)'
				sum repetitions if start == `culprit'
				replace repetitions = `r(max)' in `i'
				
			}
			
		}
}
	
} 

drop repeat_count

cap egen repeated = anymatch(repeat_group_*), values(1) // variable to capture whether a question is repeated
if _rc {
	
	gen repeated = 0
	
}



*------------------------------------------------------------------
*	General Question Blocks
*------------------------------------------------------------------

foreach jump in begin end {
	count if type=="`jump'_group"
	local q_groups = `r(N)'
	levelsof order if type=="`jump'_group", local(`jump') separate( )
	tokenize "``jump''", parse( )
	forvalues num = `q_groups'(-1)1{
		local `jump'_`num' ``num''
	}
}

gen group=.
gen conditions=relevant // survey logic for the question

/* 
Now, we're working on defining the enablement conditions for each question.
This takes into account the conditions for the individual question as well as 
those for the question block the question is in.
*/

forvalues i = 1/`q_groups' {
	local taken_`i' = 0 // this local will indicate whether an "end_repeat" has already been assigned to a specific repeat group
}

forvalues i = `q_groups'(-1)1 { // for each begin_group
	forvalues j = `q_groups'(-1)1 { // for each end_group
		local true = (`end_`j'' > `begin_`i'') & `taken_`j''==0 // evaluates to 1 if the end_group is after the begin_group and hasn't already been taken
		if `true' {
			local k = `j' // potential match. But we will try lower values of repeat blocks to check if there is another end_group that is closer
		}
	}
	local taken_`k' = 1 // we have assigned this end_group
	gen group_`i' = 1  in `begin_`i''/`end_`k'' // gen dummy=1 if question is inside group block
	levelsof relevant in `begin_`i'', local(group_relevant)
	if `r(r)' > 0 {
		replace conditions = conditions + "; " + `group_relevant' if group_`i'==1 
	}
}

gen unlock = conditions
replace unlock = subinstr(unlock, ";", "", 1) if regexm(unlock, "^;")
replace unlock = subinstr(unlock, "()", "", .)
replace unlock = strtrim(unlock)
replace unlock = subinstr(unlock, "and", "", 1) if regexm(unlock, "^and")
replace unlock = strtrim(unlock)
replace unlock = substr(unlock, 1, length(unlock) - 3) if substr(unlock, -3, 3) ==  "and"
replace unlock = strtrim(unlock)
replace unlock = subinstr(unlock, "$", "#", .)
replace unlock = "" if unlock=="and"
replace unlock = "none" if missing(unlock)

*------------------------------------------------------------------
*	Question Types
*------------------------------------------------------------------

gen preloaded=regexm(calculation, "^pulldata") // If the variable is preloaded, we may want to be alerted to that fact
gen note = type=="note" // Notes are useless to us, good to identify them here

label define question_type_M 1 "String" 2 "Select One" 3 "Select Multiple" ///
	4 "Numeric" 5 "Date" 6 "Datetime" -111 "Group Boundary" -222 "Note" ///
	-333 "Geopoint" -555 "Other" 
gen question_type=.
label values question_type question_type_M
replace question_type = 1 if inlist(type, "text", "deviceid", "image") | preloaded==1
replace question_type = 2 if word(type, 1)=="select_one"
replace question_type = 3 if word(type, 1)=="select_multiple"
replace question_type = 4 if !inlist(type, "date", "text") & missing(question_type)
replace question_type = 5 if inlist(type, "date", "today")
replace question_type = 6 if inlist(type, "start", "end", "submissiondate")
replace question_type= 7 if type == "geopoint"
replace question_type = -111 if inlist(type, "begin_group", "end_group", ///
	"begin_repeat", "end_repeat")
replace question_type=-222 if note==1
replace question_type = -333 if type == "text audit"
replace question_type=-555 if missing(question_type)

/* 
The above tries to assign a question type to every row of the instrument. 
If the question type is negative (see value label) the dofile will just skip 
that variable. In the majority of cases this should be benign, and you'll know
pretty quickly if a variable needs attention.
*/

frame copy qs qs_preserved

*===============================================================================
* 	Variables
*===============================================================================

compress

gen looper=""

drop if question_type <0

sort order
local v = 1
local brek = char(10)
local tab = char(9)
levelsof name, local(variables) separate ( )

quietly foreach var in `variables' { // for each question in the instrument

	local quest if name=="`var'"
	
	local reshape_1 // reset this
	
	local reshape_2 // reset that
	
	local cmd_`v' // this macro will contain the import commands for this particular question
	
	sum question_type `quest'
	local type = `r(max)' // question type
	
	sum order `quest'
	local order = `r(max)' // order of questions in the survey
	
	cap levelsof type2 `quest', local(vallabel) clean // value label
	
	levelsof unlock `quest', local(unlock) clean // relevance conditions
	
	sum repeated `quest'
	local repeated = `r(max)' // dummy for whether variable is in any repeat groups
	
	levelsof labelStata `quest', local(stlabel) // Stata label
	
	levelsof labelEnglishen `quest', local(enlabel) // How question appeared in the survey
	
	if !missing("`vallabel'") {
		frame choices: count if list_name == "`vallabel'"
		if `r(N)'==0 {
			local type = 1
		}
	}
	
	if `type'<=0 {
		continue
	}

	else {
		
		if `type'==1 {
			gen cmd_`v' = `"cap tostring \`var', replace `brek' `tab'cap replace \`var' = "" if \`var' == ".""'
		}
		
		if `type'==2 {
			gen cmd_`v' = `"cap destring \`var', replace `brek' `tab'cap label values \`var' `vallabel'"'
		}
		
		if `type'==4 {
			gen cmd_`v' = `"cap destring \`var', replace"'
		}
		
		if `type'==5 {
			gen cmd_`v' = `"cap local micvar \`var'___m `brek'`tab' cap gen \`micvar'=date(\`var', "`datestyle'", 2025) `brek'`tab'cap drop \`var' `brek'`tab'cap rename \`micvar' \`var' `brek'`tab'cap format \`var' %td `brek'`tab' cap label variable \`var' `stlabel'"'
		}
		
		if `type'==6 {
			gen cmd_`v' = `"cap local micvar \`var'___m `brek'`tab'cap gen \`micvar'=clock(\`var', "`datestyle'hms", 2025) `brek'`tab'cap drop \`var' `brek'`tab'cap rename \`micvar' \`var `brek'`tab'cap format \`var' %tc `brek'`tab'cap label variable \`var' `stlabel'"'
		}
		
		if `type' == 7 {
			gen cmd_`v' = `"cap foreach aspect in latitude longitude altitude accuracy { `brek'`tab'cap destring \`var'\`aspect' `brek'`tab'cap label variable \`var'\`aspect' "\`aspect' of \`var'" `brek'}"'
		}
	
		
		local start_`v' `"*=============================================================================== `brek' * 	`var' `brek' *=============================================================================== `brek' `brek'"'
		
		if `repeated'==1 { // if question is in at least one repeat group
		
			local m = 1 
			
			forvalues i = 1/`repeat_groups' { // for all repeat groups in survey
			
				count `quest' & repeat_group_`i'==1 // check which repeat groups question belongs to
				
				if `r(N)'==1 { // if repeat group is relevant for this question
					local q_rep_groups `q_rep_groups' `i' // list of repeat groups the question belongs to, starting with the first and ending with the last occurring
					local final_repeat = `i'
					frame repeat_groups { 
						sum repetitions in `i'
						local numrpts = `r(max)'
						
						if `numrpts'==0 {
							
							continue
							
						}
						
						else {
							
							local repeats_`m' = `numrpts'
							
						}
						
					}
					local ++m
				}
				
			}
			
			local varlist `var'
			local m = `m' - 1
			
			forvalues i = 1/`m' { // number of repeat groups this variable is in
			
				local newvarlist "" // reset `newvarlist'
				local oldvarlist `varlist' // make `oldvarlist' the penultimate looped variables for labelling purposes
				
				foreach gen in `varlist' { // the variables created by this particular survey question
				
					forvalues k = 1/`repeats_`i'' { // the maximum number of repeats in this repeat group
					
						local newvarlist `newvarlist' `gen'_`k'
						
					}
					
				}
			
				local varlist `newvarlist'
			
			}
			
			cap confirm variable cmd_`v' 
			if _rc & `type' !=3 {
				gen cmd_`v' = ""
			}
	
			
			if !inlist(`type', 3, 7) {
				gen loop_`v' = `"foreach var in `varlist' { `brek' `tab'cap label variable \`var' `stlabel' `brek' `tab'cap notes \`var': "`enlabel'" `brek' `tab'cap notes \`var': "relevance conditions: `unlock'" `brek' `tab'"' + cmd_`v' + `"`brek'}"'
				foreach part in `oldvarlist' {
					local reshape_1 `reshape_1' `part'_
				}
				replace dataset_`final_repeat' = dataset_`final_repeat' + "`reshape_1' "
			}
			
			else if `type' == 7 {
				gen loop_`v' = ""
				local varlist1 : subinstr local varlist "`var'" "`var'altitude", all
				local varlist2 : subinstr local varlist "`var'" "`var'accuracy", all
				local varlist3 : subinstr local varlist "`var'" "`var'latitude", all
				local varlist4 : subinstr local varlist "`var'" "`var'longitude", all
				local varlist `varlist1' `varlist2' `varlist3' `varlist4'
				replace loop_`v' = loop_`v' + `"foreach var in `varlist' { `brek'`tab'cap label variable \`var' "#{`var'}: GPS information" `brek' `tab'cap notes \`var': "#{`var'}: GPS information" `brek'`tab'cap destring \`var', replace `brek'`tab'cap notes \`var': "relevance conditions: `unlock'" `brek'} `brek' `brek'"'
				
				local word : word 1 of `varlist'
				local length = strlen("`word'") - 1
				local word = substr("`word'", 1, `length')
				local reshape_2 `word'
				foreach element in accuracy latitude longitude {
					local rs_var_add : subinstr local word "altitude" "`element'"
					local reshape_2 `reshape_2' `rs_var_add'
				}
				if "`reshape_2'" != "_" {
					replace dataset_`final_repeat' = dataset_`final_repeat' + "`reshape_2' "
				}
			}
			
			else if `type'==3 {
				local f = 1
				
				gen loop_`v'=""
				frame choices: levelsof order if list_name=="`vallabel'"
				frame choices: foreach row in `r(levels)' {

					levelsof name if order==`row', local(value) clean
					levelsof label if order==`row', local(lab) clean
					local varlist`f' : subinstr local varlist "`var'" "`var'_`value'", all
					
					frame qs {
						replace loop_`v' = loop_`v' + `"foreach var in `varlist' { `brek'`tab' cap label variable \`var' `stlabel' `brek'`tab'cap tostring \`var', replace `brek'`tab'cap replace \`var' = "" if \`var' == "." `brek'} `brek'"'
						replace loop_`v' = loop_`v' + `"foreach var in `varlist`f'' { `brek'`tab'cap label variable \`var' "#{`var'}: `lab'" `brek' `tab'cap notes \`var': "#{`var'}: `lab'" `brek'`tab'cap destring \`var', replace `brek'`tab'cap notes \`var': "relevance conditions: `unlock'" `brek'} `brek' `brek'"'
						local reshape_2 : word 1 of `varlist`f''
						local length = strlen("`reshape_2'") - 1
						local reshape_2 = substr("`reshape_2'", 1, `length')
						if "`reshape_2'" != "_" {
							replace dataset_`final_repeat' = dataset_`final_repeat' + "`reshape_2' "
						}
						
					}

					local ++f
				}
			
			}
			
		}
		
		else {
			
			if `type'==1 {
				gen loop_`v' = `"cap label variable `var' `stlabel' `brek' cap notes `var': `enlabel' `brek' cap notes `var': "relevance conditions: `unlock'" `brek' cap tostring `var', replace `brek' cap replace `var' = "" if `var' == "."`brek'"'
			}
			
			if `type'==2 {
				gen loop_`v' = `"cap label variable `var' `stlabel' `brek' cap notes `var': `enlabel' `brek' cap notes `var': "relevance conditions: `unlock'" `brek' cap destring `var', replace `brek' cap label values `var' `vallabel'"'
			}
			
			if `type'==4 {
				gen loop_`v' = `"cap label variable `var' `stlabel' `brek' cap notes `var': `enlabel' `brek' cap notes `var': "relevance conditions: `unlock'" `brek' cap destring `var', replace"'
			}
			
			if `type'==5 {
				gen loop_`v' = `"local micvar `var'___m `brek'cap gen \`micvar'=date(`var', "`datestyle'", 2025) `brek'cap drop `var' `brek'cap rename \`micvar' `var' `brek'cap format `var' %td `brek' cap label variable `var' `stlabel' `brek' cap notes `var': `enlabel' `brek' cap notes `var': "relevance conditions: `unlock'" `brek'"'
			}
			
			if `type'==6 {
				gen loop_`v' = `"local micvar `var'___m `brek' cap gen \`micvar'=clock(`var', "`datestyle'hms", 2025) `brek'cap drop `var' `brek'cap rename \`micvar' `var' `brek'cap format `var' %tc `brek' cap label variable `var' `stlabel' `brek' cap notes `var': `enlabel' `brek' cap notes `var': "relevance conditions: `unlock'" `brek'"'
			}
			
			if `type' == 7 {
				gen loop_`v' = `"foreach element in accuracy latitude longitude {`brek'`tab'cap label variable `var'\`element' `stlabel' `brek' cap notes `var'_\`element': `enlabel' `brek' cap notes `var'_\`element': "relevance conditions: `unlock'" `brek' cap destring `var'_\`element', replace`brek'}"'
			}

			if `type'==3 {
			gen loop_`v' = `"cap label variable `var' `stlabel' `brek' tostring `var', replace `brek' cap replace `var' = "" if `var' == ".", replace"'
			frame choices: levelsof order if list_name=="`vallabel'"
			frame choices: foreach row in `r(levels)' {
				levelsof name if order==`row', local(value) clean
				levelsof label if order==`row', local(lab) clean
				frame qs: replace loop_`v' = loop_`v' + `"`brek' `brek' cap label variable `var'_`value' "#{`var'}: `lab'" `brek' cap notes `var'_`value': "#{`var'}: `lab'" `brek' cap notes `var'_`value': "relevance conditions: `unlock'""'
				}
			}
			
		}
		
		replace looper = looper + "`start_`v''" + loop_`v' + "`brek' `brek'"
		
	}
	
	cap drop `quest' & _n!=1
	cap drop cmd_`v'
	cap drop loop_`v'
	
	local ++v

}


local v = `v'-1


*===============================================================================
* 	Open File
*===============================================================================

if "`savefolder'" != "" cap mkdir "`savefolder'"

// Now we're about to write the instructions to a dofile. Buckle up
cap file close myfile
file open myfile using "`dofile'", write text replace

*===============================================================================
* 	Write Variables
*===============================================================================

/* 
The code will start to look messy here - but trust the process. We're going to 
automatically generate a dofile that does everything your instrument tells it to.
It will be structured a bit like this current dofile - you can change the details
here if you so wish.

The general order of events will be:
1. Title and dofile information
2. Setup
3. Establishing key macros
4. Importing the data
5. Defining all variable labels
6. Cleaning each variable systematically, one by one.
*/

file write myfile ///
	"/*" ///
	_n "Title: Import Dofile for `macval(instname)'" ///
	_n "Date Created: `c(current_date)'" ///
	_n "Author: `c(username)'" ///
	_n "Note: " ///
	_n "*/" _n(3) ///
	"*===============================================================================" ///
	_n "* 	Setup" _n /// 
	"*===============================================================================" ///
	_n(3) "clear all" _n "version 17" _n "set more off" _n "set maxvar 30000" ///
	_n "cap log close" _n "set trace off" _n "set linesize 200" _n(3) ///
	"*===============================================================================" ///
	_n "* 	Macros" _n /// 
	"*===============================================================================" ///
	_n(3) ///
	"local" _tab `"today = date(c(current_date), "`datestyle'")"' _n ///
	"local" _tab `"todaystr=string(\`today', "%td")"' _n(3) ///
	"*===============================================================================" ///
	_n "* 	Import" _n /// 
	"*===============================================================================" _n(2) ///
	`"import delimited "`macval(dataname)'", clear bindquote(strict)"' _n(3) ///
	"*===============================================================================" ///
	_n "* 	Labels" _n /// 
	"*===============================================================================" _n(3) 
	
forvalues i = 1/`labels_counter' {
	file write myfile `"`lab`i''"' _n(2)
}

file write myfile ///
"*===============================================================================" ///
_n "* 	Clean" _n /// 
"*===============================================================================" _n(3) 


file write myfile (looper) _n(2)


file write myfile ///
	"*===============================================================================" ///
	_n "* 	Survey Version" _n /// 
	"*===============================================================================" ///
	_n(2) `"destring formdef_version, replace"' _n ///
	`"label variable formdef_version "survey version""' _n(2) ///
	"*===============================================================================" ///
	_n "* 	Submission Date" _n /// 
	"*===============================================================================" ///
	_n(2) `"cap gen SUBmissiondate = clock(submissiondate, "`datestyle'hms", 2025)"' _n ///
		`"cap drop submissiondate"' _n ///
		`"cap rename SUBmissiondate submissiondate"' _n /// 
		`"cap format submissiondate %tc"' _n ///
		`"cap label variable submissiondate "time of survey submission""' _n(2)
		
		

if `want_reshape' == 1 {
	
	display regexm("`instname'", "[^\\/]+$")
	local file_short = regexs(0)
	
	frame repeat_groups {
	
		reshape long nest_, i(repeat_group) j(inside)
		bysort repeat_group (inside) : drop if (nested == 0 & _n != 1) | ///
			(nested == 1 & missing(nest_))
		bysort repeat_group (inside): replace inside = _n
		
		levelsof repeat_group if missing(nest_), clean local(standalone)
		levelsof repeat_group if !missing(nest_), clean local(nesteds)
		
		cap file close myfile2
		file open myfile2 using "`reshapefile'", write text replace
		file write myfile2 ///
			"/*" ///
			_n "Title: Reshape Dofile for `file_short'" ///
			_n "Date Created: `c(current_date)'" ///
			_n "Author: `c(username)'" ///
			_n "Note: " ///
			_n "*/" _n(3) ///
			"*===============================================================================" ///
			_n "* 	Handy Macros" _n /// 
			"*===============================================================================" ///
			_n(2) ///
			"*------------------------------------------------------------------" _n ///
			"*	Reshapable Variables" _n ///
			"*------------------------------------------------------------------" _n(2)
			
	}
			
	forvalues i = 1/`repeat_groups' {
		file write myfile2 (dataset_`i') _n(2)
	}
	
	frame repeat_groups {
		
		file write myfile2 ///
		"*===============================================================================" ///
		_n "* 	Reshaping" _n /// 
		"*===============================================================================" ///
		_n(2) "frame rename default survey" _n ///
		`"label data "Survey-level data from `file_short'""' _n(2) ///
		"local frgetvars"
	
		foreach g in `standalone' {
				
			levelsof start if repeat_group == `g', clean local(row)
			frame qs_preserved: levelsof name if _n == `row', clean local(desc)
			
			file write myfile2 ///
				_n(2) ///
				"*------------------------------------------------------------------" _n ///
				"*	Reshape to level of '`desc''" _n ///
				"*------------------------------------------------------------------" _n(2) ///
				"local group_name `desc'" _n(2) ///
				"foreach var in \$shape_`g' {" _n ///
				_tab "cap local X_\`var' : variable label \`var'1" _n "}" _n(2) ///
				"reshape long \$shape_`g', i(key) j(\`group_name'_key)" _n(2) ///
				"foreach var of varlist \$shape_`g' {" _n ///
				_tab `"label variable \`var' "\`X_\`var''""' _n "}" _n(2) ///
				"frame put \$shape_`g' `identifying_vars' \`group_name'_key, into(\`group_name')" _n ///
				"drop \$shape_`g' \`group_name'_key" ///
				_n "bysort key: keep if _n == 1" _n(2) ///
				"cwf \`group_name'" _n "drop if missing(`desc'_index)" _n ///
				`"renvars *_, postsub("_" "")"' _n ///
				"frlink m:1 key, frame(survey)" _n ///
				`"if "\`frgetvars'" != "" {"' _n(2) ///
				_tab `"frame survey: ds"' _n _tab `"local vars_in_data \`r(varlist)'"' _n ///
				_tab "local vars_to_get : list vars_in_data in frgetvars" _n ///
				_tab "frget \`vars_to_get', from(survey)" _n(2) ///
				"}" _n(2) ///
				"isid key \`group_name'_key" _n ///
				"drop survey" _n ///
				`"label data "`desc'-level data from `file_short'""' _n
				
			if "`savefolder'" != "" {
				
				file write myfile2 ///
					`"save "`macval(savefolder)'/\`group_name'.dta", replace"' _n(2) 
				
			}

			file write myfile2 `"cwf survey"' _n
					
			
		}
		
		foreach g in `nesteds' {
	
			levelsof nest_ if repeat_group == `g', clean local(within)
			if `r(r)' >= 3 {
				
				continue
				
			}
			else if `r(r)' == 2 {
				
				continue
				
			}
			else if `r(r)' == 1 {
				
				levelsof nest_ if repeat_group == `g', clean local(within)
				levelsof start if repeat_group == `within', clean local(w_row)
				levelsof start if repeat_group == `g', clean local(row)
				frame qs_preserved: levelsof name if _n == `row', clean local(desc)
				frame qs_preserved: levelsof name if _n == `w_row', clean local(w_desc)
				
				file write myfile2 ///
					_n(2) ///
					"*------------------------------------------------------------------" _n ///
					"*	Reshape to level of '`desc'' from within '`w_desc''" _n ///
					"*------------------------------------------------------------------" _n(2) ///
					"local group_name `desc'" _n ///
					"local prev_group_name `w_desc'" _n ///
					"local all_invars" _n(2) ///
					"foreach var in \$shape_`g' {" _n ///
					_tab "cap local X_\`var' : variable label \`var'1" _n "}" _n(2) ///
					"reshape long \$shape_`g', i(key) j(\`group_name'_key)" _n(2) ///
					"foreach var of varlist \$shape_`g' {" _n ///
					_tab `"cap label variable \`var' "\`X_\`var''""' _n "}" _n(2) ///
					"frame put \$shape_`g' `identifying_vars' \`group_name'_key, into(\`group_name')" _n ///
					"drop \$shape_`g' \`group_name'_key" _n "bysort key: keep if _n == 1" _n(2) ///
					"cwf \`group_name'" _n(2) ///
					"// reformat the reshape variables so that they can be reshaped long once more" _n ///
					"foreach var of varlist \$shape_`g' {" _n ///
					_tab `"if regexm("\`var'", "[0-9]+_\$") {"' _n ///
					_tab(2) `"local invar = regexr("\`var'", "[0-9]+_\$", "")"' _n ///
					_tab(2) "local all_invars \`all_invars' \`invar'" _n ///
					_tab "}" _n(2) ///
					"}" _n(2) ///
					"// get rid of duplicates in the reshape vars" _n ///
					"local all_invars: list uniq all_invars" _n(2) ///
					"// find value labels and tell Stata where to find the reshape value" _n ///
					"foreach var in \`all_invars' {" _n ///
					_tab "cap local X_\`var' : variable label \`var'1_" _n ///
					_tab "local reshapevars \`reshapevars' \`var'@_" _n ///
					"}" _n(2) ///
					"// an id for each element being reshaped" _n ///
					"bysort key: gen reshape_id = _n" _n(2) ///
					"// reshape to long again" _n ///
					"reshape long \`reshapevars', i(reshape_id key) j(\`prev_group_name'_key)" _n(2) ///
					"// get rid of the spare underscore" _n ///
					`"renvars *__, postsub("_" "")"' _n(2) ///
					"// label the variables" _n ///
					"foreach var of varlist \`all_invars' {" _n ///
					_tab `"cap label variable \`var' "\`X_\`var''""' _n ///
					"}" _n(2) ///
					"// get rid of the spare underscore again" _n ///
					`"renvars *_, postsub("_" "")"' _n(2) ///
					"// if it's missing this, then it shouldn't be in the dataset" _n ///
					"drop if missing(`desc'_index)" _n(2) ///
					"frlink m:1 `w_desc'_key key, frame(`w_desc')" _n ///
					`"if "\`frgetvars'" != "" {"' _n(2) ///
					_tab `"frame `w_desc': ds"' _n _tab `"local vars_in_data \`r(varlist)'"' _n ///
					_tab "local vars_to_get : list vars_in_data in frgetvars" _n ///
					_tab "frget \`vars_to_get', from(`w_desc')" _n(2) /// 
					"}" _n(2) ///
					"// check ids are intact" _n ///
					"isid key `desc'_key `w_desc'_key" _n(2) ///
					"drop `w_desc' reshape_id" _n ///
					`"label data "`desc'-level data by `w_desc' from `file_short'""' _n
					
			}
			
			if "`savefolder'" != "" {
				
				file write myfile2 ///
					`"save "`macval(savefolder)'/\`group_name'.dta", replace"' _n(2)
				
			}

			file write myfile2 `"cwf survey"' _n
			
		}
		
		if "`savefolder'" != "" {
				
				file write myfile2 ///
					`"save "`macval(savefolder)'/survey.dta", replace"' _n(2)
				
		}
		file close myfile2
		
		
	}
	
}
	
	
	
	
file close myfile
	
	
}

end
