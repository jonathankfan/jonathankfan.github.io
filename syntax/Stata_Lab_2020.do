
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/*SET WORKING DIRECTORY*/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/

/*Before we start, let's set the working directory for this session*/
global projectfolder "C:\"
display "$projectfolder"

/*To run a block of code, highlight it and press CTRL+D to execute it, or press the triangle button in the taskbar; but be careful, if you hit CTRL+D without any text highlighted, it will execute the entire DO file, similar to SAS; so I like to place a string of random letters to stop execution and prevent this from happening; I also do this for error debugging to see if a DO file will execute successfully up to a certain point; just comment it out when you are ready to run the whole DO file*/
*dsfadfas

/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/*ABOUT*/
/*This lab was developed for use in the University of Toronto DLSPH Software Training Week courses*/
/*Data analyses are derived for teaching purposes only; do not use any of the findings for publication, or to guide any scientific or policy decisions*/
/*Syntax examples are created for teaching purposes only; they have not been vetted and should be double checked before using for actual projects*/
/*Last modified: April 2020*/
/*Author: Jonathan Fan*/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/

/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/*OPTIONAL: CREATE FOLDER STRUCTURE*/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/

/*You can create subfolders automatically, if desired, and set your export paths to these directories for better organization*/
/*This is useful beyond Stata, for general folder creation in Windows*/
*capture noisily mkdir "$projectfolder\subfolder1"
*capture noisily mkdir "$projectfolder\subfolder1\subfolder2"

/*To delete specific files, use this command:*/
*capture noisily rm "$projectfolder\subfolder1\subfolder2\file1.txt"

/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/*OPTIONAL: CREATE LOG FILE*/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/

/*You can create a log file to save everything that happens in the main command window*/
/*The <log close> command closes any existing log files before starting a new one; <capture noisily> is just used as a try-fail syntax in case there are no log files open; <log close at the end will tell Stata to stop logging>*/

/*capture noisily log close*/
/*log using "C:\logname.log", replace*/
/*dis "Hello world!"*/
/*log close*/

/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/*EXERCISE 1*/
/*Play around with basic coding functions in Stata to give you a toolset for later exercises*/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/

/**************************************************/
/*The clear command is required to unload any existing data, since Stata loads one datafile per session; the command does nothing if there is no data loaded*/
/**************************************************/

	clear

/**************************************************/
/*Find and load pre-installed example datasets*/
/**************************************************/

	/*these datasets are pre-installed and can be used to test code*/
	sysuse dir

	/*Let's use the nlsw88 survey data*/
	sysuse nlsw88, clear

/**************************************************/
/*Browse data*/
/**************************************************/

	/*The first thing I like to do is visually inspect the data; you can open up a browser window or list a few observations*/
	/*three things to note: strings are listed in RED; numeric variables are listed in BLACK; numeric variables with value labels are in BLUE (but stored in their underlying numeric format)*/
	/*the beauty of Stata is that one dataset is loaded into memory and can be viewed in real-time as you make edits; this is why Stata is excellent for iterative data coding and exploratory analyses (in SAS, you have to type <proc print> every time the dataset is modified)*/
	browse

/**************************************************/
/*List some observations*/
/**************************************************/

	/*************************/
	/*List the first 5 observations using Stata's range notation; and list again with abbreviators to display data on one line per row*/
	/*************************/
	list in 1/5
	list in 1/5, noobs ab(5) string(3) separator(0)

	/*************************/
	/*List the first and last observations; this is helpful for checking ranges in the data or missing values (which are treated as high values); first, we sort the variable*/
	/*************************/
	sort wage
	list wage in 1/10
	list wage in -10/-1

	/**************************************************/
	/*Do the same, but reverse sort*/
	/**************************************************/
	gsort -wage
	list wage in 1/10
	list wage in -10/-1

/**************************************************/
/*Describing data*/
/**************************************************/

	/*************************/
	/*Count observations*/
	/*************************/
	count

	/*************************/
	/*You can also run a codebook of all variables*/
	/*************************/
	describe
	codebook

	/*************************/
	/*Inspect data ranges and values; you can look for min and max values, formats, missing data*/
	/*************************/
	summarize
	summarize, d
	tab collgrad, missing
	tabstat wage, statistics(n mean min p25 p50 p75 max)

/**************************************************/
/*Confirm the structure of the data; this is importance since your unit of analysis will dictate how you will aggregate the data; upon visual inspection, it looks like there is one row per ID, but let's confirm*/
/**************************************************/

	/*************************/
	/*First, install a user-written package, if not already installed; <capture noisily> just skips through any errors*/
	/*************************/
	capture noisily ssc install unique

	/*************************/
	/*Yes, it looks like one row per car make, so make is the unique identifier*/
	/*************************/
	unique id

	/*************************/
	/*We can also do the same with the duplicates command; since there are no duplicates (i.e., dup==0), then ID is unique per observation*/
	/*************************/
	duplicates tag id, generate(dup)
	tab dup, missing
	drop dup

	/*************************/
	/*We can also generate sequence indicators to do the same; if sequence==1, then that means there is only one unique observation per ID; if sequence>1, then there are multiple rows of data per ID*/
	/*egen is a great command; see <help egen> for more information*/
	/*************************/
	egen sequence=seq(), by(id)
	tab sequence

	/*************************/
	/*Finally, we can do it by cell references*/
	/*************************/
	sort id
	count if id[_n]==id[_n-1]

/**************************************************/
/*Creating variables*/
/**************************************************/

	/*************************/
	/*Let's focus on wage; wage is continuous, but let's dichotomize (even though binning is sinning)*/
	/*Median wage is 6.2, let's split here*/
	/*************************/
	sum wage, d

	/*************************/
	/*After running analytic commands, the results are sometimes stored in a local variable; we can extract this for later use*/
	/*************************/
	return list
	global medianwage=r(p50)
	dis $medianwage

	/*************************/
	/*Note: Stata treats missing values as a "high" value; thus, it's good practice to cap off the top range by excluding missing, even if there aren't any missing values (FYI, SAS treats missing as a "low" value)*/
	/*************************/
	generate wage_high=0 if wage<=$medianwage
	replace wage_high=1 if wage>$medianwage & wage!=.
	tab wage wage_high, missing

	/*************************/
	/*Create a value label*/
	/*************************/
	label define wage_highf 0 "<=$medianwage" 1 ">$medianwage"
	label values wage_high wage_highf
	tab wage_high, missing

	/*************************/
	/*We can also create quantiles using the xtile command*/
	/*************************/
	xtile wage_quantiles=wage, nquantiles(4)
	tab wage wage_quantiles

	/*************************/
	/*Our median split variable matches up with the quantiles*/
	/*************************/
	tab wage_quantiles wage_high

/**************************************************/
/*Let's close this example and create a dummy dataset with 10 observations to play around with*/
/**************************************************/

	clear
	set obs 10

/**************************************************/
/*Create a bunch of variables using various functions*/
/**************************************************/

	/*************************/
	/*create new variables*/
	/*************************/
	generate var1=1
	generate var2=1 in 1/5
	generate var3=1 if missing(var2)
	generate var4=1 if !missing(var2)
	list

	/*************************/
	/*perform operations*/
	/*************************/
	generate var5=var1+var2 /*this does not work if any of the variables are missing; be careful when using to generate scale items!*/
	egen var6=rowtotal(var1 var2) /*this works even if a variable is missing; be careful when using to generate scale items!*/
	generate var7=5*5
	list

	/*************************/
	/*string variables*/
	/*************************/
	generate var8="Hello"
	generate var9="Hello" in 1/5
	replace var9="There" if var9==""
	capture noisily replace var9=5 /*this does not work, since var9 is a string variable*/
	generate var10="Place this observation in once cell" in 5
	list

	/*************************/
	/*sequence variables, which are useful*/
	/*************************/
	generate var11=_n
	generate var12=_N
	egen var13=seq()
	list

	/*************************/
	/*OR operator*/
	/*************************/
	generate var14=1 if (var11==2 | var11==4)
	list var11 var14

	/*************************/
	/*alternative syntax for OR operator; equivalent to SAS <if var11 in(2,4) then var15=1;>*/
	/*************************/
	generate var15=1 if inlist(var11,2,4)
	list var11 var15

	/*************************/
	/*************************/
	/*AND operator*/
	generate var16=1 if var8=="Hello" & var9=="There"
	list var8 var9 var16

	/*************************/
	/*we can use <encode> to convert string to numeric and automatically encode the labels, if any*/
	/*************************/
	encode var9, generate(var9_encoded)
	list var9 var9_encoded
	list var9 var9_encoded, nolabel
	browse var9 var9_encoded

	/*************************/
	/*we can also go backwards*/
	/*************************/
	decode var9_encoded, generate(var9_decoded)
	browse var9 var9_encoded var9_decoded

/**************************************************/
/*Manually edit a variable; type <edit>, click on an empty column, and type in a number; click on an empty column, and type in text*/
/*Be careful with this; I would never do this in practice; instead, code it within the do file for reproducibility; see example for var10 above*/
/**************************************************/

	edit

	/*switch back to browse mode*/
	browse

/**************************************************/
/*Drop a variable*/
/**************************************************/

	drop var7
	list

	drop var1-var3
	list

	drop var*
	list

/**************************************************/
/*Clear the memory*/
/**************************************************/

	clear
	list

/**************************************************/
/*Use Stata as an overpriced calculator*/
/**************************************************/

	display 5*5
	display exp(1)
	display exp(5)
	dis ln(exp(5))
	dis 5^20
	dis _pi
	dis td(5june2019)-td(1jan2019)

	/*16, not 1!*/
	dis 8/2*(2+2)
	dis 8/(2*(2+2))

/**************************************************/
/*(Pseudo) random numbers*/
/*These can be used to generate scrambled identifiers or for sampling*/
/*The seed command ensures reproducibility, if desired*/
/**************************************************/

	clear
	set obs 100

	/*************************/
	/*generate uniformly distributed random variables*/
	/*************************/
	generate random1=runiform()
	generate random2=runiform()
	list in 1/5

	/*************************/
	/*notice how each call of runiform produces different results; but we can set the seed for reproducibility*/
	/*************************/
	set seed 12321312
	generate random3=runiform()
	set seed 12321312
	generate random4=runiform()
	list in 1/5

	/*************************/
	/*we can generate random integers*/
	/*************************/
	generate randominteger=runiformint(1,100)
	sum randominteger, d
	list in 1/5

	/*************************/
	/*generate scrambled identifier*/
	/*************************/
	generate id=_n
	generate randomsort=runiform()
	sort randomsort
	generate xid=_n
	list in 1/5

/**************************************************/
/*Random sampling*/
/*See:
https://www.stata.com/support/faqs/statistics/random-samples/
https://stats.idre.ucla.edu/stata/faq/how-can-i-draw-a-random-sample-of-my-data/
*/
/**************************************************/

	/*************************/
	/*Create example dataset with a variable to check the results of sampling*/
	/*************************/

		clear
		set obs 10000
		generate agecat=1 in 1/2500
		replace agecat=2 in 2501/5000
		replace agecat=3 in 5001/7500
		replace agecat=4 in 7501/10000
		tab agecat

		set seed 12321312
		generate randomsort=runiform()
		sum randomsort, d

	/*************************/
	/*Extract 10% sample*/
	/*************************/

		sort randomsort
		generate sampled=1 in 1/1000
		replace sampled=0 if sampled==.
		tab sampled, missing

		list in 1/10

	/*************************/
	/*Check if equal proportion of age groups in sampled and non-sampled population; otherwise can run stratified sample*/
	/*************************/

		tab agecat sampled, missing
		tab agecat sampled, missing nofreq col

	/*************************/
	/*Quicker method*/
	/*************************/

		count
		sample 10
		count

		tab agecat, missing

/**************************************************/
/*Open the help files for various Stata commands*/
/**************************************************/

	help list
	help display
	help functions
	help random
	help reshape
	help merge

	help tab
	help regress
	help logistic
	help st
	help xt
	help sem
	help time

/**************************************************/
/*Some other useful commands*/
/**************************************************/

	/*************************/
	/*Set a timer to keep track of how much time is required to run a block of code; this is useful for programming, or to create efficiencies*/
	/*We can also use the sleep command to pause the program; this is useful for loops to give you a chance to review output before it washes away*/
	/*************************/

		timer clear 1
		timer on 1
		sleep 5000
		timer off 1
		timer list 1

	/*************************/
	/*Change the delimiter to semi-colons like SAS; this is useful to replicate the behaviour of SAS or if you are copying in a single command that span multiple lines*/
	/*************************/

		/*Change to semi-colons*/
		#delimit ;
		dis 5+5;
		dis 5+5
			+2;

		/*Change back to default carriage return*/
		#delimit cr

	/*************************/
	/*Check the working directory to help you track down any random files, as some working files may end up here*/
	/*************************/

		cd

	/*************************/
	/*Pre-installed ICD lookups; see <help icd9> or <help icd10> for more uses*/
	/*************************/

		icd9 search "fract"
		icd10 search "fract"

		icd9 search "coronav"
		icd10 search "coronav"

		icd9 search "yoga"
		icd10 search "space"

/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/*EXERCISE 2*/
/*Play around with advanced coding functions in Stata*/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/

/**************************************************/
/*Load multiple datasets temporarily (remember that you work with one dataset at a time in Stata)*/
/**************************************************/

	sysuse auto, clear
	list in 1/10

	preserve
	sysuse nlsw88, clear
	list in 1/10
	restore

	list in 1/10

/**************************************************/
/*Merging data*/
/**************************************************/

	/*************************/
	/*Let's use the nlsw88 survey data and inspect (note that it is long in terms of occupation)*/
	/*************************/

		sysuse nlsw88, clear

		sort occupation
		list idcode age occupation tenure in 1/10

	/*************************/
	/*Load the wide-format data to inspect (note: wide in terms of occupation)*/
	/*************************/

		sysuse nlswide1, clear

		tab occ
		desc

	/*************************/
	/*Load the long data, and merge in the wide data*/
	/*************************/

		sysuse nlsw88, clear

		generate occ=occupation
		merge m:1 occ using "C:\Program Files\Stata16\ado\base/n/nlswide1.dta"
		tab _merge
		tab occupation if _merge==1
		tab occupation _merge
		drop if _merge==2

		list idcode age occupation tenure* in 1/10

/**************************************************/
/*Collapse data*/
/**************************************************/

	/*************************/
	/*Let's use the nlsw88 survey data (long in terms of occupation)*/
	/*************************/

		sysuse nlsw88, clear
		list in 1/10

		generate count=1
		collapse (mean) tenure union (sum) count, by(occupation)

		list

	/*************************/
	/*Load wide data; confirm the same summary values (slightly off, since based on slightly different counts)*/
	/*************************/

		sysuse nlswide1, clear
		list occ tenure88 union88 count88

/**************************************************/
/*Reshape data*/
/*This is another key feature of Stata*/
/**************************************************/

	/*************************/
	/*Load long data, and reshape into the wide data*/
	/*************************/
	sysuse bplong, clear

	/*************************/
	/*For each patient, there are multiple records of BP, identified by the variable "when"; also, there is only one value for sex and age per patient (i.e., this data is "constant"")*/
	/*************************/
	list in 1/10, noobs sepby(patient)

	/*************************/
	/*Apply the reshape command*/
	/*Note: you do not have to specify sex and age as these are constant; but if they are not constant, it will give you an error message and require you to either reshape, or aggregate separately before reshaping*/
	/*************************/
	reshape wide bp, i(patient) j(when)

	/*************************/
	/*Now the data is collapsed across multiple BP records to the level of the patient; bp1 and bp2 contain the previously long variables*/
	/*************************/
	list in 1/5, noobs separator(0)

	/*************************/
	/*Compare with the pre-installed version*/
	/*************************/
	sysuse bpwide, clear
	list in 1/5, noobs separator(0)

/**************************************************/
/*Storage of decimals; this will frustrate you later*/
/*Stata works in binary. Stata stores data in float precision by default. Stata performs all calculations in double precision. Sometimes the combination results in surprises until you think more carefully about what happened.*/
/*See <help precision>*/
/**************************************************/

	clear
	set obs 1

	/*************************/
	/*Stata stores in float format by default (which is good since you want the most precision)*/
	/*Note: to convert to string, we have to use the force option to truncate the extra decimal places*/
	/*************************/
	generate decimal_float=1.1
	count if decimal_float==1.1
	tostring decimal_float, generate(string_from_float)
	tostring decimal_float, generate(string_from_float) force
	describe
	list, ab(20)

	/*************************/
	/*But note that Excel uses double precision; so to replicate excel, use double*/
	/*Note: to convert to string, no force option is needed as there are no extra decimals to truncate*/
	/*************************/
	generate double decimal_double=1.1
	count if decimal_double==1.1
	tostring decimal_double, generate(string_from_double)
	describe
	list, ab(20)

/**************************************************/
/*Convert across variable types*/
/*This is useful for merging lookup tables or retaining the code structure (e.g., National Occupational Classification, ICD)*/
/**************************************************/

	clear
	set obs 20

	generate double code=_n*100+(_n/10)
	list

	/*************************/
	/*convert numeric to string*/
	/*************************/
	tostring code, generate(code_string)
	list, ab(20)

	/*************************/
	/*convert numeric to string, with leading zeros*/
	/*change the format number to change the number of leading zeros, depending upon the string length (including decimals)*/
	/*************************/
	tostring code, generate(code_string_leading) format(%06.1f)
	list, ab(20)

	/*************************/
	/*convert numeric to string, with trailing zeros*/
	/*************************/
	generate code_string_trailing=string(code,"%12.2f")
	list, ab(20)

	/*************************/
	/*convert numeric to string, with leading and trailing zeros*/
	/*************************/
	generate code_string_leading_trailing=string(code,"%012.2f")
	list, ab(30)

	/*************************/
	/*extract a portion of a string variable; note this does not round the variable, and preserves the hierarchical structure of the code*/
	/*************************/
	generate code_1d=substr(code_string_leading,1,1)
	generate code_2d=substr(code_string_leading,1,2)
	generate code_3d=substr(code_string_leading,1,3)
	generate code_4d=substr(code_string_leading,1,4)
	generate code_decimals=substr(code_string_leading,6,2)
	list code code_*d, ab(20)

	/*************************/
	/*round the variable; this would not be appropriate for a hierarchical code*/
	/*************************/
	generate code_rounded=round(code)
	list code code_rounded, ab(20)

	/*************************/
	/*extract a portion of a numeric variable (without needing to convert to string first)*/
	/*************************/
	generate code_floor_1d=floor(code/1000)
	generate code_floor_2d=floor(code/100)
	generate code_floor_3d=floor(code/10)
	generate code_floor_4d=floor(code/1)
	list code code_floor*, ab(20)

		/*note: if you have a negative number, this will not work as anticipated, so multiple by (-1) before operations, and then again by (-1) after operations*/
		generate code_negative=code*-1
		generate code_negative_floor_1d=floor(code_negative/1000)
		generate code_negative_floor_2d=floor(code_negative/100)
		generate code_negative_floor_3d=floor(code_negative/10)
		generate code_negative_floor_4d=floor(code_negative/1)
		list code code_negative code_negative_floor*, ab(20)

		/*or use the truncate option*/
		capture drop code_negative*
		generate code_negative=code*-1
		generate code_negative_floor_1d=trunc(code_negative/1000)
		generate code_negative_floor_2d=trunc(code_negative/100)
		generate code_negative_floor_3d=trunc(code_negative/10)
		generate code_negative_floor_4d=trunc(code_negative/1)
		list code code_negative code_negative_floor*, ab(20)

/**************************************************/
/*String commands*/
/*This is useful for searching for text, data mining, postal codes, etc*/
/*See <help regex> for more complicated commands*/
/**************************************************/

	sysuse auto, clear
	count

	keep make
	list in 1/10

	/*************************/
	/*change contents of string variables*/
	/*************************/
	generate make_upper=upper(make)
	generate make_lower=lower(make)
	generate make_proper=proper(make)
	list in 1/10

	/*************************/
	/*split strings*/
	/*************************/
	split make, generate(split_)
	generate maker=split_1
	generate model=split_2+" "+split_3
	list make split* maker model in 1/10

	/*************************/
	/*************************/
	/*extract a portion of the text contained in make, starting at position 1, and extracting the next 2 characters*/
	generate make_2d=substr(make,1,2)
	list make make_2d in 1/10

	/*************************/
	/*replace characters contained in make, searching for text1, replacing with text1, and doing so for all (.) or a given number (#) of times*/
	/*************************/
	generate make_cleaned=subinstr(make,"AMC","American Motors Corporation",.)
	list make make_cleaned in 1/10

	/*************************/
	/*search for text, including wildcards to allow for any type of word bounding on either side of the string*/
	/*************************/
	browse if strmatch(make,"*Honda*")
	generate is_honda=1 if strmatch(make,"*Honda*")
	tab make is_honda

/**************************************************/
/*Assign macro variables, which is basically placeholder text for some other value*/
/*Note: in SAS, these are defined as <%let macroname = ;>; however, a benefit of Stata is that these macro variables can be called anytime in the session and used for any purpose, whereas in SAS, sometimes there are issues, e.g., using a macro variable to define a variable name in a data step doesn't work*/
/**************************************************/

	/*************************/
	/*Locals are called by enclosing the assigned name with <`'> characters*/
	/*************************/
	local macroname1 5
	dis `macroname1'

	/*************************/
	/*Note: locals are only valid for the current session (if typed directly in the command box) or the current DOFILE execution (if run with CTRL+D); as a check, run this code in the DO file:*/
	/*************************/
	local macroname2 10
	dis `macroname2'

	/*************************/
	/*And then run this code directly in the command file; it doesn't exist beyond the "LOCAL" execution*/
	/*************************/
	dis `macroname2'

	/*************************/
	/*Globals are called by enclosing the assigned name with <${}> characters, or just the <$> character*/
	/*************************/
	global macroname3 20
	dis $macroname3

	/*************************/
	/*We can combine locals and globals*/
	/*Note the impact of using equal signs*/
	/*************************/
	local macroname1 1
	global macroname2 2
	global combined `macroname1' $macroname2
	dis "$combined"

	/*************************/
	/*We can also use macro variables when referencing the name of another macro variable; this is useful within loops*/
	/*Note: since these are nested, we have to enclose the macro with <${}> characters to resolve the macro as a whole*/
	/*************************/
	global macroname "Nothing"
	global macroname1 "Hello"
	global macroname12 "Hellothere"
	local i=1
	local ii=2
	dis "$macroname`i'`ii'"
	dis "${macroname`i'}`ii'"
	dis "${macroname`i'`ii'}"

	/*************************/
	/*Finally, note the use of equal signs*/
	/*************************/
	global macroname1 1 
	global macroname2 2
	global equalsign=$macroname1+$macroname2
	dis "$equalsign"
	global noequalsign $macroname1+$macroname2
	dis "$noequalsign"

	/*************************/
	/*Why use local or global? This is mostly a programming decision, as locals will not be carried over throughout the code; this prevents the use of stale or expired values in a loop, for example; to be the most generic, just use globals, but be aware that they will carry throughout the session*/
	/*************************/

/**************************************************/
/*Run a loop*/
/*TIP: anytime you have blocks of repeating code, consider a loop for efficiency*/
/*This is also useful for running analyses for a block of variables, as we will see later*/
/**************************************************/

	/*************************/
	/*Remember this loop syntax as the most generic, but other variations are discussed below FYI*/
	/*************************/

		foreach i in 1 2 3 4 5 one two three four five {
		dis "`i'"
		dis "Hello `i'"
		}

/**************************************************/
/*Other loop options; let's skip these for now, but included here for your reference*/
/**************************************************/

	/*************************/
	/*The <forvalues> method is efficient for numeric values, and uses the Stata syntax for defining ranges*/
	/*************************/

		forvalues i=1/5 {
		dis `i'
		dis "Hello `i'"
		}

		forvalues i=1(2)10 {
		dis `i'
		dis "Hello `i'"
		}

	/*************************/
	/*The <foreach in> method is the most generic and works with both numeric and string values; just remember this version*/
	/*************************/

		foreach i in 1 2 3 4 5 {
		dis `i'
		dis "Hello `i'"
		}

		/*Here, we use the <capture noisily> modifier as a try-fail option to display an error if it fails but keep the code running*/
		/*We also use the <as text> option to switch the color of the next line back to black, as errors will be displayed in red and will carry through subsequent loops unless reset*/
		/*Here, we also switch the naming of the local from <i> to <number>, but this can be named as whatever you like*/
		foreach number in one two three four five {
		capture noisily dis `number'
		dis as text "Hello `number'"
		}

	/*************************/
	/*We use <foreach of varlist> rather than <foreach in> to work with variables exclusively; this is useful for coding purposes, to ensure that loop operations are performed only for variables loaded in the data; if you place a list item that is not a loaded variable, then it will return an error*/
	/*************************/

		clear
		set obs 10
		generate var1=1
		generate var2=2

		capture noisily foreach var of varlist adsfadssa dsafadsfda {
		tab `var'
		}

		foreach var of varlist var1 var2 {
		tab `var'
		}

/**************************************************/
/*Advanced loops*/
/**************************************************/

	/*************************/
	/*First, let's create an empty dataset*/
	/*************************/

		clear
		set obs 10
		generate id=_n
		list

	/*************************/
	/*Use a loop to create variables*/
	/*************************/

		foreach i in 1 2 3 4 5 {
		generate newvar`i'=`i'
		}
		list

		/*Let's run the loop again, but notice how it gives an error if the variables already exist; so let's add a command to drop an existing variable if it exists*/
		foreach i in 1 2 3 4 5 {
		capture noisily drop newvar`i'
		generate newvar`i'=`i'
		}
		list

		/*Create string variables; just enclose the values in quotes*/
		foreach i in 6 7 8 9 10 {
		capture noisily drop newvar`i'
		generate newvar`i'="String `i'"
		}
		list

	/*************************/
	/*Nested loops; this is useful to loop through parallel lists; you can do it Inception style and go as deep as you want*/
	/*Also, let's incremenent a counter variable to keep track of how many loops were performed; this has many useful purposes for programming; increment it within the lowest nesting*/
	/*Finally, let's use the command <quietly> to supress the output in the main window*/
	/*************************/

		local icounter=0
		foreach iloop in newvar1 newvar2 newvar3 {
			foreach iiloop in 2 4 6 {
				local icounter=`icounter'+1
				dis "LOOP `iloop', SUBLOOP `iiloop', iteration `icounter':"
				quietly replace `iloop'=999 in `iiloop'
				global icountermax=`icounter'
			}
		}

		list
		dis "ITERATIONS PERFORMED IN LOOP: $icountermax"

	/*************************/
	/*Finally, let's use the <trace> command to display exactly what is being done with the loop and to help with diagnosing bugs*/
	/*This command will display the input lines and compiled output lines*/
	/*************************/

		set trace on

			local icounter=0
			foreach iloop in newvar1 newvar2 newvar3 {
				foreach iiloop in 2 4 6 {
					local icounter=`icounter'+1
					dis "LOOP `iloop', SUBLOOP `iiloop', iteration `icounter':"
					quietly replace `iloop'=999 in `iiloop'
					global icountermax=`icounter'
				}
			}

		set trace off

/**************************************************/
/*Programming*/
/*This is the same functionaly as a SAS %macro wrapper (confusing: SAS calls them macro programs, but there are also macro variables, which are the same as local and globals in Stata*/
/*A program is similar to a DO file, except you can nest multiple programs within one DO file*/
/*You can pass through stored macro variable text to both a program and a DO file can specify inputs directly into the program*/
/*In either case, these will help reduce repetitive blocks of code, reduce copy/paste errors, or reduce the number of edits you have to make for repetitive sections*/
/**************************************************/

	/*************************/
	/*Simple dummy wrapper to call all of the commands within the program (similar to DO file) with just one word command*/
	/*************************/

		/*define program*/
		capture program drop helloworld
		program define helloworld

			/*list your desired commands here to execute upon typing <helloworld> into the command box*/
			display "Hello, world!"
			display "Today is `c(current_date)'"
			display "Did you know? 5*5 is " 5*5

		end

		/*execute by typing into command box*/
		helloworld

	/*************************/
	/*Allowing for inputs via the <syntax> option*/
	/*see <help syntax>*/
	/*************************/

		/*define the program*/
		capture program drop helloname
		program define helloname
		syntax [varlist] [if] [in] [, firstname(string) yearborn(string) anything(string)]

			/*these are the commands*/

				local yeardiff=year(date("`c(current_date)'","DMY"))-`yearborn'
				local firstnameproper=proper("`firstname'")
				display "Hello `firstnameproper'! You were born in `yearborn', which was `yeardiff' years ago!"

				if "`anything'"=="" {
				display "You did not specify the <anything> option, fail!"
				}

				if "`anything'"!="" {
				display "You DID specify the <anything> option, good job! The text you entered was <`anything'>"
				}
			
		end

		/*execute the programs*/
		helloname, firstname(jon) yearborn(1900)
		helloname, firstname(jon) yearborn(1900) anything(sdfddsfds)

/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/*EXERCISE 3*/
/*In this exercise, you'll learn the fundamentals of data management and cleaning using a sample dataset*/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/

/********************************************************************************/
/*DOWNLOAD DATA*/
/*
CCHS 2012: Mental Health Component
http://sda.chass.utoronto.ca/cgi-bin/sda/hsda?harcsda3+cchs2012mh
Download -> Customized Subset (allow popups)
Choose "CSV file"
Check "Data definitions for Stata"
Select ALL variables (file is small, 32 MB)
Click "Continue"
Click "Create the files"
It should say "587 variables for 25113 cases in subset"
Right click and download the three files
*/
/********************************************************************************/

/********************************************************************************/
/*Import the CSV file*/
/*Note: I usually use the point and click interface to import files to allow for visual inspection*/
/********************************************************************************/

insheet using "$projectfolder\cchs_2012_subset.csv", clear
count
desc, f

/********************************************************************************/
/*Let's focus on the relationship between psychosocial work conditions (exposure) and mental health (outcome), adjusted for covariates*/
/*Keep variables of interest:*/
/*Survey identifiers and weights*/
/*Variables needed for inclusion criteria*/
/*Variables needed for outcomes*/
/*Variables needed for exposures*/
/*Variables needed for covariates*/
/********************************************************************************/

	keep ///
	caseid wts_m ///
	gen_08 lbsg31 ///
	dhhgage dhh_sex dhhgms edudr04 ///
	dis_10b dis_10d dis_10e dis_10h dis_10i dis_10j disdk6 ///
	wst_401 wst_402 wst_403 wst_404 wst_409 wst_405 wst_406 wstdski wstdaut wstdpsy

	desc, full

/********************************************************************************/
/*LABEL THE DATA USING SYNTAX COPIED FROM THE DOWNLOADED TEXT FILE*/
/*YOU CAN COPY AND PASTE MANUALLY, OR IMPORT USING THE DOWNLOADED LABEL FILE (ALTHOUGH IT SOMETIMES GIVES AN ERROR MESSAGE): <do "AA0ZZNPV.txt">*/
/********************************************************************************/

	/*NOTE: IF LABELS ALREADY EXIST, YOU CAN DROP USING <label drop _all>*/
	label drop _all

	/*ATTACH NAMES TO VARIABLES*/

		label variable wts_m "Weights: master"

		label variable gen_08 "Worked at job or business"
		label variable lbsg31 "Employment status in past 12 months (grouped)"

		label variable dhhgage "Age (grouped)"
		label variable edudr04 "Highest level of education attained by respondent: 4 levels (derived)"
		label variable dhh_sex "Sex"
		label variable dhhgms "Marital status (grouped)"

		label variable dis_10b "Distress feelings in past month: frequency felt nervous"
		label variable dis_10d "Distress feelings in past month: frequency felt hopeless"
		label variable dis_10e "Distress feelings in past month: frequency felt restless or fidgety"
		label variable dis_10h "Distress feelings in past month: frequency felt so depressed that nothing could cheer up"
		label variable dis_10i "Distress feelings in past month: frequency felt that everything was an effort"
		label variable dis_10j "Distress feelings in past month: frequency felt worthless"
		label variable disdk6 "Distress scale: k6: past month (derived)"

		label variable wst_401 "Job required learning new things"
		label variable wst_402 "Job required a high level of skill"
		label variable wst_403 "Job allowed freedom to decide how to do job"
		label variable wst_404 "Job required doing things over and over"
		label variable wst_409 "Had a lot of to say about what happened in your job"
		label variable wstdski "Derived work stress scale: decision latitude: skill discretion (derived)"
		label variable wstdaut "Derived work stress scale: decision latitude: decision authority (derived)"

	/*DEFINE LABELS FOR VARIABLE VALUES*/

		#delimit ;

		label define gen_08    1 "YES" 2 "NO" 6 "NOT APPLICABLE" 7 "DON'T KNOW" 
							   8 "REFUSAL" 9 "NOT STATED" ;
							   
		label define lbsg31    1 "EMPLOYEE" 2 "SELF-EMPLOYED" 6 "NOT APPLICABLE" 
							   7 "DON'T KNOW" 8 "REFUSAL" 9 "NOT STATED" ;

		label define dhhgage   1 "15 TO 19 YEARS" 2 "20 TO 24 YEARS" 
							   3 "25 TO 29 YEARS" 4 "30 TO 34 YEARS" 
							   5 "35 TO 39 YEARS" 6 "40 TO 44 YEARS" 
							   7 "45 TO 49 YEARS" 8 "50 TO 54 YEARS" 
							   9 "55 TO 59 YEARS" 10 "60 TO 64 YEARS" 
							   11 "65 TO 69 YEARS" 12 "70 TO 74 YEARS" 
							   13 "75 TO 79 YEARS" 14 "80 YEARS OR MORE" 
							   96 "NOT APPLICABLE" 97 "DON'T KNOW" 98 "REFUSAL" 
							   99 "NOT STATED" ;

		label define dhh_sex   1 "MALE" 2 "FEMALE" 6 "NOT APPLICABLE" 7 "DON'T KNOW" 
							   8 "REFUSAL" 9 "NOT STATED" ;

		label define dhhgms    1 "MARRIED" 2 "COMMON-LAW" 3 "WIDOWED" 
							   4 "DIVORCED OR SEPARATED" 5 "SINGLE" 
							   6 "NOT APPLICABLE" 7 "DON'T KNOW" 8 "REFUSAL" 
							   9 "NOT STATED" ;

		label define edudr04   1 "LESS THAN SECONDARY SCHOOL GRADUATION" 
							   2 "SECONDARY SCHOOL GRADUATION" 
							   3 "SOME POST-SECONDARY" 4 "POST-SECONDARY GRADUATION" 
							   6 "NOT APPLICABLE" 7 "DON'T KNOW" 8 "REFUSAL" 
							   9 "NOT STATED" ;

		label define dis_10b   1 "ALL OF THE TIME" 2 "MOST OF THE TIME" 
							   3 "SOME OF THE TIME" 4 "LITTLE OF TIME" 
							   5 "NONE OF THE TIME" 6 "NOT APPLICABLE" 7 "DON'T KNOW" 
							   8 "REFUSAL" 9 "NOT STATED" ;
		label define dis_10c   1 "ALL OF THE TIME" 2 "MOST OF THE TIME" 
							   3 "SOME OF THE TIME" 4 "LITTLE OF TIME" 
							   5 "NONE OF THE TIME" 6 "NOT APPLICABLE" 7 "DON'T KNOW" 
							   8 "REFUSAL" 9 "NOT STATED" ;
		label define dis_10d   1 "ALL OF THE TIME" 2 "MOST OF THE TIME" 
							   3 "SOME OF THE TIME" 4 "LITTLE OF TIME" 
							   5 "NONE OF THE TIME" 6 "NOT APPLICABLE" 7 "DON'T KNOW" 
							   8 "REFUSAL" 9 "NOT STATED" ;
		label define dis_10e   1 "ALL OF THE TIME" 2 "MOST OF THE TIME" 
							   3 "SOME OF THE TIME" 4 "LITTLE OF TIME" 
							   5 "NONE OF THE TIME" 6 "NOT APPLICABLE" 7 "DON'T KNOW" 
							   8 "REFUSAL" 9 "NOT STATED" ;
		label define dis_10f   1 "ALL OF THE TIME" 2 "MOST OF THE TIME" 
							   3 "SOME OF THE TIME" 4 "LITTLE OF TIME" 
							   5 "NONE OF THE TIME" 6 "NOT APPLICABLE" 7 "DON'T KNOW" 
							   8 "REFUSAL" 9 "NOT STATED" ;
		label define dis_10g   1 "ALL OF THE TIME" 2 "MOST OF THE TIME" 
							   3 "SOME OF THE TIME" 4 "LITTLE OF TIME" 
							   5 "NONE OF THE TIME" 6 "NOT APPLICABLE" 7 "DON'T KNOW" 
							   8 "REFUSAL" 9 "NOT STATED" ;
		label define dis_10h   1 "ALL OF THE TIME" 2 "MOST OF THE TIME" 
							   3 "SOME OF THE TIME" 4 "LITTLE OF TIME" 
							   5 "NONE OF THE TIME" 6 "NOT APPLICABLE" 7 "DON'T KNOW" 
							   8 "REFUSAL" 9 "NOT STATED" ;
		label define dis_10i   1 "ALL OF THE TIME" 2 "MOST OF THE TIME" 
							   3 "SOME OF THE TIME" 4 "LITTLE OF TIME" 
							   5 "NONE OF THE TIME" 6 "NOT APPLICABLE" 7 "DON'T KNOW" 
							   8 "REFUSAL" 9 "NOT STATED" ;
		label define dis_10j   1 "ALL OF THE TIME" 2 "MOST OF THE TIME" 
							   3 "SOME OF THE TIME" 4 "LITTLE OF TIME" 
							   5 "NONE OF THE TIME" 6 "NOT APPLICABLE" 7 "DON'T KNOW" 
							   8 "REFUSAL" 9 "NOT STATED" ;

		label define disdk6    0 "LOWEST RECORDABLE DISTRESS (K6)" 
							   24 "HIGHEST RECORDABLE DISTRESS (K6)" 
							   96 "NOT APPLICABLE" 97 "DON'T KNOW" 98 "REFUSAL" 
							   99 "NOT STATED" ;

		label define wst_401   1 "STRONGLY AGREE" 2 "AGREE" 
							   3 "NEITHER AGREE NOR DISAGREE" 4 "DISAGREE" 
							   5 "STRONGLY DISAGREE" 6 "NOT APPLICABLE" 
							   7 "DON'T KNOW" 8 "REFUSAL" 9 "NOT STATED" ;
		label define wst_402   1 "STRONGLY AGREE" 2 "AGREE" 
							   3 "NEITHER AGREE NOR DISAGREE" 4 "DISAGREE" 
							   5 "STRONGLY DISAGREE" 6 "NOT APPLICABLE" 
							   7 "DON'T KNOW" 8 "REFUSAL" 9 "NOT STATED" ;
		label define wst_403   1 "STRONGLY AGREE" 2 "AGREE" 
							   3 "NEITHER AGREE NOR DISAGREE" 4 "DISAGREE" 
							   5 "STRONGLY DISAGREE" 6 "NOT APPLICABLE" 
							   7 "DON'T KNOW" 8 "REFUSAL" 9 "NOT STATED" ;
		label define wst_404   1 "STRONGLY AGREE" 2 "AGREE" 
							   3 "NEITHER AGREE NOR DISAGREE" 4 "DISAGREE" 
							   5 "STRONGLY DISAGREE" 6 "NOT APPLICABLE" 
							   7 "DON'T KNOW" 8 "REFUSAL" 9 "NOT STATED" ;
		label define wst_409   1 "STRONGLY AGREE" 2 "AGREE" 
							   3 "NEITHER AGREE NOR DISAGREE" 4 "DISAGREE" 
							   5 "STRONGLY DISAGREE" 6 "NOT APPLICABLE" 
							   7 "DON'T KNOW" 8 "REFUSAL" 9 "NOT STATED" ;

		label define wstdski   
							   0 "LOWEST RECORDABLE WORK STRESS FROM SKILL DISCRETION" 
							   1 "..." 2 "..." 3 "..." 4 "..." 5 "..." 6 "..." 
							   7 "..." 8 "..." 9 "..." 10 "..." 11 "..." 
							   12 "HIGHEST RECORDABLE WORK STRESS FROM SKILL DISCRETION" 
							   96 "NOT APPLICABLE" 97 "DON'T KNOW" 98 "REFUSAL" 
							   99 "NOT STATED" ;
		label define wstdaut   
							   0 "LOWEST RECORDABLE WORK STRESS FROM DECISION AUTHORITY" 
							   1 "..." 2 "..." 3 "..." 4 "..." 5 "..." 6 "..." 
							   7 "..." 
							   8 "HIGHEST RECORDABLE WORK STRESS FROM DECISION AUTHORITY" 
							   96 "NOT APPLICABLE" 97 "DON'T KNOW" 98 "REFUSAL" 
							   99 "NOT STATED" ;

		#delimit cr

	/*APPLY ABOVE LABELS*/

		label values wts_m wts_m
		label values gen_08 gen_08
		label values lbsg31 lbsg31
		label values dhhgage dhhgage
		label values edudr04 edudr04
		label values dhh_sex dhh_sex
		label values dhhgms dhhgms
		label values dis_10b dis_10b
		label values dis_10d dis_10d
		label values dis_10e dis_10e
		label values dis_10h dis_10h
		label values dis_10i dis_10i
		label values dis_10j dis_10j
		label values disdk6 disdk6
		label values wst_401 wst_401
		label values wst_402 wst_402
		label values wst_403 wst_403
		label values wst_404 wst_404
		label values wst_409 wst_409
		label values wstdski wstdski
		label values wstdaut wstdaut

/********************************************************************************/
/*DISTRESS (OUTCOME)*/
/********************************************************************************/

/*FROM CCHS CODEBOOK FILE:

disdk6             Distress scale: k6: past month (derived)

Pre Question Text:
All respondents

Notes:
Based on DIS_10B, DIS_10D, DIS_10E,  DIS_10H,  DIS_10I,  DIS_10J.  See
documentation on derived variables.

VALUE  LABEL
0  LOWEST RECORDABLE DISTRESS (K6)
24  HIGHEST RECORDABLE DISTRESS (K6)
96  NOT APPLICABLE
97  DON'T KNOW
98  REFUSAL
99  NOT STATED

Data type: numeric
Missing-data codes: 96-99
Record/columns: 1/287-288

*/

/**************************************************/
/*Explore variables*/
/**************************************************/

tab1 dis_10b dis_10d dis_10e dis_10h dis_10i dis_10j disdk6, missing

/**************************************************/
/*We need to recode the data to remove missing values; the missing codes are 7, 8, 9*/
/**************************************************/

	/*scale items*/
	foreach var in dis_10b dis_10d dis_10e dis_10h dis_10i dis_10j {
	replace `var'=. if inlist(`var',7,8,9)
	tab `var', missing
	}

	/*summary score*/
	replace disdk6=. if disdk6==99
	tab disdk6, missing

/**************************************************/
/*Let's recreate the summary scale variable that was derived by Statistics Canada (disdk6)*/
/*First, we need to reverse score the data so that higher scores = worse distress*/
/*We also want to rescale the range of scores from 1-5 to 0-4, since this is how Statistics Canada calculates the summary score*/
/*We will use our trusty loop, and name the new variable with a "t" for temporary and "i" for inverse, per CCHS naming conventions*/
/*The general formula to reverse code is <top score - value + bottom score>*/
/*To rescale to 0-4, we can just subtract 1*/
/*Note: this is different than normalizing variables that have different ranges or response levels (there are other methods to do this)*/
/*Note: do not perform these operations on scale items with missing values; we only want to calculate the derived variable in cases where all variables are non-missing; if we wanted to allow some scale items to be missing, then there are other methods for imputation*/
/**************************************************/

	/*rescale and invert*/
	foreach var in dis_10b dis_10d dis_10e dis_10h dis_10i dis_10j {
	generate `var'_ti=(5-`var'+1)
	replace `var'_ti=`var'_ti-1
	}

	/*confirm conversion*/
	tab dis_10b dis_10b_ti, missing
	tab dis_10d dis_10d_ti, missing
	tab dis_10e dis_10e_ti, missing
	tab dis_10h dis_10h_ti, missing
	tab dis_10i dis_10i_ti, missing
	tab dis_10j dis_10j_ti, missing

/**************************************************/
/*create summary scale*/
/**************************************************/

	/*note: this operation will produce a missing value if any of the values is missing (which is what we want); the <egen rowtotal> function will work even if there are missing values; be careful with this, as you may need to impute missing values first*/
	generate distress_0to24=dis_10b_ti+dis_10d_ti+dis_10e_ti+dis_10h_ti+dis_10i_ti+dis_10j_ti
	tab distress_0to24, missing

	/*confirm the scores are the same; yes*/
	tab distress_0to24 disdk6, missing
	assert distress_0to24==disdk6

	/*create dichotomized variable based on threshold*/
	generate distress_yn=1 if distress_0to24>=11 & distress_0to24!=.
	replace distress_yn=0 if distress_0to24<11
	tab distress_0to24 distress_yn, missing

/********************************************************************************/
/*DECISION LATITUDE (EXPOSURE): COMPRISED OF FIVE ITEMS - RANGE 0 TO 20*/
/********************************************************************************/

/**************************************************/
/*Explore the values*/
/**************************************************/

tab1 wst_401 wst_402 wst_403 wst_404 wst_409, missing

/**************************************************/
/*We need to recode the data to remove missing values; the missing codes are 6, 7, 8, 9*/
/**************************************************/

	/*scale items*/
	foreach var in wst_401 wst_402 wst_403 wst_404 wst_409 {
	replace `var'=. if inlist(`var',6,7,8,9)
	tab `var', missing
	}

	/*summary scores*/
	replace wstdski=. if wstdski==96 | wstdski==99
	tab wstdski, missing

	replace wstdaut=. if wstdaut==96 | wstdaut==99
	tab wstdaut, missing

	/*additional step: job control is defined as the combination of skill discretion and decision authority; need to combine the two scales*/
	generate wstdskiaut=wstdski+wstdaut
	tab wstdskiaut, missing

/**************************************************/
/*Rescale the data*/
/**************************************************/

	/*these variables need rescaling; note, these variables are already scored such that higher scores = worse conditions*/
	foreach var in wst_401 wst_402 wst_403 wst_409 {
	generate `var'_t=`var'-1
	}

	/*only 404 needs additional inverting so that higher scores = worse conditions*/
	foreach var in wst_404 {
	generate `var'_ti=(5-`var'+1)
	replace `var'_ti=`var'_ti-1
	}

	/*confirm conversion*/
	tab wst_401 wst_401_t, missing
	tab wst_402 wst_402_t, missing
	tab wst_403 wst_403_t, missing
	tab wst_404 wst_404_ti, missing
	tab wst_409 wst_409_t, missing

	/*create summary scale*/
	generate low_job_control_0to20=wst_401_t+wst_402_t+wst_403_t+wst_404_ti+wst_409_t
	tab low_job_control_0to20, missing

	/*confirm the scores are the same; yes*/
	tab low_job_control_0to20 wstdskiaut, missing
	scatter low_job_control_0to20 wstdskiaut
	assert low_job_control_0to20==wstdskiaut

/**************************************************/
/*Job control is continuous, but let's dichotomize*/
/**************************************************/

	/*Check median value*/
	/*After running analytic commands, the results are sometimes stored in a local variable; we can extract this for later use*/
	sum low_job_control_0to20, d
	return list
	global medianjc=r(p50)
	dis $medianjc

	/*Note: Stata treats missing values as a "high" value; thus, it's good practice to cap off the top range by excluding missing, even if there aren't any missing values (FYI, SAS treats missing as a "low" value)*/
	generate low_job_control_yn=0 if low_job_control_0to20<=$medianjc
	replace low_job_control_yn=1 if low_job_control_0to20>$medianjc & low_job_control_0to20!=.
	tab low_job_control_0to20 low_job_control_yn, missing

	/*Create a value label*/
	label define low_job_control_ynf 0 "<=$medianjc" 1 ">$medianjc"
	label values low_job_control_yn low_job_control_ynf
	tab low_job_control_yn, missing

	/*We can also create quantiles using the xtile command*/
	xtile low_job_control_q4=low_job_control_0to20, nquantiles(4)
	tab low_job_control_0to20 low_job_control_q4

	/*Our median split variable matches up with the quantiles*/
	tab low_job_control_q4 low_job_control_yn

/********************************************************************************/
/*Let's quickly recode the covariates*/
/********************************************************************************/


	tab1 dhhgage dhh_sex dhhgms edudr04, missing
	tab1 dhhgage dhh_sex dhhgms edudr04, missing nol

	generate agecat=dhhgage

	generate female=1 if dhh_sex==2
	replace female=0 if dhh_sex==1
	tab dhh_sex female, missing

	generate marstat=dhhgms if !inlist(dhhgms,9)
	label values marstat dhhgms
	tab dhhgms marstat, missing

	generate educat=edudr04 if !inlist(edudr04,9)
	label values educat edudr04
	tab edudr04 educat, missing

/********************************************************************************/
/*Save cleaned file*/
/*Compress just saves space, if possible*/
/*Saveold allows the file to be opened by earlier versions of Stata*/
/*Replace will overwrite any existing file*/
/********************************************************************************/

	compress
	count
	saveold "$projectfolder\cchs_2012_subset_cleaned.dta", replace

/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/*EXERCISE 4*/
/*In this exercise, you'll learn the fundamentals of data analysis using a sample dataset*/
/*DISCLAIMER: THIS IS A DERIVED DATA ANALYSIS FOR EXAMPLE ONLY; CONTROL VARIABLES WERE SELECTED FOR CONVENIENCE; DO NOT USE THESE FINDINGS FOR PUBLICATION OR TO GUIDE ANY SCIENTIFIC OR POLICY DECISIONS*/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/

/********************************************************************************/
/********************************************************************************/
/*Summary*/
/********************************************************************************/
/********************************************************************************/

/*You can see that the majority of your time will be spent cleaning the data; once you have a cleaned dataset, actually running the analysis is relatively simple*/

/*Now we have two outcome variables: distress_0to24, distress_yn*/
/*Three exposure variables: low_job_control_0to20, low_job_control_q4, low_job_control_yn*/

/*We can test the following with linear regression:*/
/*Scenario 1A: continuous X -> continuous Y*/
/*Scenario 1B: binary X -> continuous Y*/
/*Scenario 1C: categorical X -> continuous Y*/

/*And the following with logistic regression:*/
/*Scenario 2A: continuous X -> binary Y*/
/*Scenario 2B: binary X -> binary Y*/
/*Scenario 2C: categorical X -> binary Y*/

/********************************************************************************/
/********************************************************************************/
/*LOAD CLEANED FILE FROM EXERCISE 3*/
/********************************************************************************/
/********************************************************************************/

	use "$projectfolder\cchs_2012_subset_cleaned.dta", replace
	count

/********************************************************************************/
/********************************************************************************/
/*APPLY COHORT INCLUSION CRITERIA*/
/********************************************************************************/
/********************************************************************************/

	/*WORKED IN PAST 12 MONTHS*/
	tab gen_08
	tab gen_08, nolabel
	keep if gen_08==1
	count

	/*EMPLOYEES ONLY (EXCLUDING SELF-EMPLOYED)*/
	tab lbsg31
	tab lbsg31, nolabel
	keep if lbsg31==1
	count

/********************************************************************************/
/********************************************************************************/
/*Explore the data*/
/********************************************************************************/
/********************************************************************************/

	/*caseid is the unique record identifier*/
	unique caseid

	describe
	codebook
	summarize
	list in 1
	browse

/********************************************************************************/
/********************************************************************************/
/*Survey set the data*/
/********************************************************************************/
/********************************************************************************/

	/*The CCHS is a cross-sectional survey with complex sampling design; as such, we have to declare the design features to produce the correct point estimates and standard errors*/
	/*Sampling design features include weights (which impact point estimates), and primary sampling units and strata (which impact standard errors)*/
	/*Statistics Canada provides weights and 500 bootstrap replicate weights (the bootstrap weights take care of the PSU and strata features)*/
	/*However, bootstrap replicates are not included in the public-use files (they are only available in the RDC)*/
	/*Thus, while we can account for survey weights (which will generally produce the same or similar point estimates as the RDC microdata files assuming no suppression of data), we cannot properly adjust the standard errors*/
	/*Instead, we use the default standard errors provided by "taylor linearization", which is basically just a robust sandwich estimator*/
	/*In practice, the point estimates and standard errors will be quite similar to the RDC estimates for large surveys*/
	/*All this is to say that we just need to specify the survey design features upfront, and let Stata handle the rest; although analyses using the public-use files will not be as accurate and precise as the RDC data (but it will be close)*/

	/*Specify survey weights*/
	svyset [pweight=wts_m]

	/*That's it; now, anytime you want weighted estimates, just use the <svy> prefix*/
	tab distress_yn female
	tab distress_yn female, col nofreq chi2
	svy: tab distress_yn female, col

	/*Note: with just survey weights and no PSU/STRATA (or bootstrap weights), the survey command will generally be equivalent to running a regular regression model with weights and robust standard error; but there is more on this topic*/

		/*Unweighted regression (non-representative point estimates)*/
		logistic distress_yn female

		/*Unweighted regression, but robust SE; see the change in confidence intervals*/
		logistic distress_yn female, vce(robust)

		/*Weighted regression, but non-robust SE; see the change in odds ratios*/
		logistic distress_yn female [pweight=wts_m]

		/*Weighted regression with robust SE see the change in odds ratios and confidence intervals*/
		logistic distress_yn female [pweight=wts_m], vce(robust)

		/*Same as SVY command, which incorporates both*/
		svy: logistic distress_yn female

/*FOR NOW, LET'S IGNORE SURVEY DESIGN FEATURES*/

/********************************************************************************/
/********************************************************************************/
/*Descriptive tables*/
/********************************************************************************/
/********************************************************************************/

	/*Produce a Table 1*/
	tab agecat low_job_control_yn, col chi2
	tab female low_job_control_yn, col chi2
	tab marstat low_job_control_yn, col chi2
	tab educ low_job_control_yn, col chi2
	tab distress_yn low_job_control_yn, col chi2
	tabstat distress_0to24, by(low_job_control_yn) statistics(n mean sd)
bysort female: tab agecat low_job_control_yn
	/*Note: easily calculate sensitivity, specificity, PPV and NPV*/
	/*Pretend these are test measures*/
	tab distress_yn low_job_control_yn
	tab distress_yn low_job_control_yn, nofreq row
	tab distress_yn low_job_control_yn, nofreq col

	/*internal consistency reliability for distress items*/
	alpha dis_10b_ti dis_10d_ti dis_10e_ti dis_10h_ti dis_10i_ti dis_10j_ti, detail item

	/*CFA with distress items*/
	factor dis_10b_ti dis_10d_ti dis_10e_ti dis_10h_ti dis_10i_ti dis_10j_ti
	rotate, blanks(.40)

	/*CFA with distress and work conditions*/
	factor dis_10b_ti dis_10d_ti dis_10e_ti dis_10h_ti dis_10i_ti dis_10j_ti wst_401_t wst_402_t wst_403_t wst_404_ti wst_409_t, factors(2)
	rotate, blanks(.40)

/********************************************************************************/
/********************************************************************************/
/*REGRESSION MODELS*/
/********************************************************************************/
/********************************************************************************/

/**************************************************/
/*Scenario 1A: continuous X -> continuous Y*/
/**************************************************/

	/*************************/
	/*Run an unadjusted model*/
	/*************************/
	regress distress_0to24 low_job_control_0to20

	/*************************/
	/*test significance of a covariate; use test for continuous; testparm for categorical with factor variable notation*/
	/*************************/
	test low_job_control_0to20

	/*************************/
	/*Run the same model but add a covariate; also, can you tell the difference here?; age is a categorical variable, but we need to either manually include dummy variables in the model, or use the built-in factor variable notation <i.agecat>, which does this automatically; use the factor variable notation, as this makes things much easier for post-estimation testing, as Stata will recognize the variables as a group; also, we can easily change the base reference category*/
	/*************************/
	regress distress_0to24 low_job_control_0to20 agecat
	regress distress_0to24 low_job_control_0to20 i.agecat
	regress distress_0to24 low_job_control_0to20 ib1.agecat
	regress distress_0to24 low_job_control_0to20 ib12.agecat

	/*************************/
	/*Run nested models*/
	/*************************/
	regress distress_0to24 low_job_control_0to20
	regress distress_0to24 low_job_control_0to20 i.agecat
	regress distress_0to24 low_job_control_0to20 i.agecat i.female
	regress distress_0to24 low_job_control_0to20 i.agecat i.female i.marstat
	regress distress_0to24 low_job_control_0to20 i.agecat i.female i.marstat i.educat

		/*************************/
		/*Note, the sample size differs due to missing data on covariates; if we want to retain a consistent sample, we can do the following:*/
		/*Run fully-saturated model and create indicator for whether observation is included (i.e., has full data on all variables)*/
		/*************************/
		quietly regress distress_0to24 low_job_control_0to20 i.agecat i.female i.marstat i.educat
		generate in_sample_final=1 if e(sample)
		tab in_sample_final, missing

		/*************************/
		/*Generate missing value indicator (note: it doesn't really matter if you include the outome and exposure variables here, as these are always included in the models)*/
		/*************************/
		generate has_full_data=1 if !missing(distress_0to24,low_job_control_0to20,agecat,female,marstat,educat)
		tab has_full_data, missing

		/*************************/
		/*Re-un nested models but with the same sample size*/
		/*************************/
		regress distress_0to24 low_job_control_0to20 if in_sample_final==1
		regress distress_0to24 low_job_control_0to20 agecat if in_sample_final==1
		regress distress_0to24 low_job_control_0to20 agecat female if in_sample_final==1
		regress distress_0to24 low_job_control_0to20 agecat female marstat if in_sample_final==1
		regress distress_0to24 low_job_control_0to20 agecat female marstat educat if in_sample_final==1

		/*************************/
		/*TIP: you can specify your covariates as a macro variable for convenience*/
		/*************************/
		global covariates i.agecat i.female i.marstat i.educat
		regress distress_0to24 low_job_control_0to20 $covariates if in_sample_final==1

		/*************************/
		/*TIP: you can loop through covariates and reduce the lines of code*/
		/*************************/
		foreach covariate in i.agecat i.female i.marstat i.educat {
		regress distress_0to24 low_job_control_0to20 `covariate' if in_sample_final==1
		}

	/*************************/
	/*Add some interactions*/
	/*Note: use the factor variable notation <i.variable>, otherwise it will treat the variable as continuous <c.variable> by default; although with binary variables, it won't matter anyway*/
	/*We need to include double hashtags in the model command to include the main effects PLUS interaction effects automatically (a single hashtag includes only the interaction effects)*/
	/*************************/

		regress distress_0to24 c.low_job_control_0to20#i.female i.marstat i.educat i.agecat /*excludes the main effects, which is not what you want*/
		regress distress_0to24 c.low_job_control_0to20 i.female c.low_job_control_0to20#i.female i.marstat i.educat i.agecat /*hierarchically well formulated if you include the main effects explicitly*/
		regress distress_0to24 c.low_job_control_0to20##i.female i.marstat i.educat i.agecat /*hierarchically well formulated, same as above*/
		testparm c.low_job_control_0to20#i.female /*for testparm, it's OK to test just the interaction coefficients*/

/**************************************************/
/*Scenario 2A: continuous X -> binary Y*/
/**************************************************/

	/*************************/
	/*Note: there are problems with comparing model estimates across nested logistic models; this relates to non-collapsibility of odds ratios, whereby model estimates may change even if the covariate is not a confounder; this is why you cannot test for mediation with logistic regrssion models (although methods do exist for mediation with categorical outcomes)*/
	/*************************/

		logistic distress_yn low_job_control_0to20
		logistic distress_yn low_job_control_0to20 i.agecat
		logistic distress_yn low_job_control_0to20 i.agecat
		logistic distress_yn low_job_control_0to20 i.agecat i.female i.marstat i.educat
		test low_job_control_0to20

	/*************************/
	/*How does the effect of job control on distress differ by sex?*/
	/*With linear models, the interaction terms are a direct estimate of additive interaction; but for logistic models, interactions are only on the multiplicative scale*/
	/*Let's visualise the interaction using margins command, which is well worth the price of Stata*/
	/*Margins are basically predicted values (or predicted probabilities for logistic models) calculated with covariates set at either their mean values, representative values, or averaged marginal effects; by default, margins estimates averaged marginal effects (standardization)*/
	/*You must use the factor variable notation so that marginal effects will know which group of dummy variables are related*/
	/*************************/

		/*************************/
		/*interaction model*/
		/*************************/
		logistic distress_yn c.low_job_control_0to20##i.female i.marstat i.educat i.agecat

		/*************************/
		/*test of multiplicative interaction; these are ratios of odds ratios*/
		/*************************/
		testparm c.low_job_control_0to20#i.female

		/*************************/
		/*average marginal effects*/
		/*************************/
		margins female, at(low_job_control_0to20=(0(1)20))
		marginsplot

		/*************************/
		/*marginal effects at means*/
		/*************************/
		margins female, at(low_job_control_0to20=(0(1)20)) atmeans
		marginsplot

		/*************************/
		/*marginal effects at values*/
		/*************************/
		margins female, at(low_job_control_0to20=(0(1)20) marstat=1 educat=1 agecat=1)
		marginsplot

		/*************************/
		/*discrete difference in slopes of the relationship between job control and distress, across males and females; this is the interaction effect on an additive scale, as generated from a logistic model*/
		/*the post command exports the model estimates to a set of local variables that can be called for use later, the coeflegend labels the output names for convenience*/
		/*************************/
		margins female, dydx(low_job_control_0to20)
		margins female, dydx(low_job_control_0to20) post coeflegend
		return list
		lincom _b[low_job_control_0to20:1.female]-_b[low_job_control_0to20:0bn.female]

		/*************************/
		/*we can run a linear probability model to compare with the interaction estimate above, since the interaction coefficients in linear models are direct estimates of interaction effect on the additive scale; the values are close*/
		/*************************/
		regress distress_yn c.low_job_control_0to20##i.female i.marstat i.educat i.agecat
		regress distress_yn c.low_job_control_0to20##i.female i.marstat i.educat i.agecat, coeflegend
		dis _b[1.female#c.low_job_control_0to20]
		margins female, at(low_job_control_0to20=(0(1)20))
		marginsplot

		/*************************/
		/*compare with stratified models*/
		/*************************/
		regress distress_yn c.low_job_control_0to20 i.marstat i.educat i.agecat if female==0
		regress distress_yn c.low_job_control_0to20 i.marstat i.educat i.agecat if female==1
		dis .0069962-.0054062

/********************************************************************************/
/********************************************************************************/
/*OTHER REGRESSION SCENARIOS*/
/********************************************************************************/
/********************************************************************************/

/**************************************************/
/*Scenario 1B: binary X -> continuous Y*/
/**************************************************/

regress distress_0to24 i.low_job_control_yn i.agecat i.female i.marstat i.educat
testparm i.low_job_control_yn

/**************************************************/
/*Scenario 1C: categorical X -> continuous Y*/
/**************************************************/

/*We can set the base category and automatically treat the categorical variable as a series of dummy variables (this is a key advantage of Stata)*/
regress distress_0to24 ib1.low_job_control_q4 i.agecat i.female i.marstat i.educat
testparm i.low_job_control_q4

/**************************************************/
/*Scenario 2B: binary X -> binary Y*/
/**************************************************/

logistic distress_yn i.low_job_control_yn i.agecat i.female i.marstat i.educat
testparm i.low_job_control_yn

/**************************************************/
/*Scenario 2C: categorical X -> binary Y*/
/**************************************************/

logistic distress_yn ib1.low_job_control_q4 i.agecat i.female i.marstat i.educat
testparm i.low_job_control_q4
testparm i.educat

/********************************************************************************/
/********************************************************************************/
/*How do you extract estimates?*/
/********************************************************************************/
/********************************************************************************/

	/*************************/
	/*run model and list what's available*/
	/*************************/
	logistic distress_yn ib1.low_job_control_q4 i.agecat i.female i.marstat i.educat
	return list
	ereturn list

	/*************************/
	/*extract model parameters*/
	/*************************/
	dis e(N)
	global samplesize=e(N)
	dis $samplesize

	/*************************/
	/*extract estimates from a matrix*/
	/*************************/
	matrix estimates=e(b)
	matrix list estimates
	global desiredcoefficient=estimates[1,2]
	dis $desiredcoefficient

	/*************************/
	/*extract estimates directly; the coeflegend option let's you know how to reference each estimate*/
	/*************************/
	logistic distress_yn ib1.low_job_control_q4 i.agecat i.female i.marstat i.educat
	logistic distress_yn ib1.low_job_control_q4 i.agecat i.female i.marstat i.educat, coeflegend

		dis exp(_b[2.low_job_control_q4])
		global desiredestimate=exp(_b[2.low_job_control_q4])
		dis $desiredestimate

		dis exp(_b[3.low_job_control_q4]-_b[2.low_job_control_q4])
		lincom _b[3.low_job_control_q4]-_b[2.low_job_control_q4]

/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/*EXERCISE 5*/
/*Extra examples*/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/

/**************************************************/
/*LOAD CLEANED FILE FROM EXERCISE 3*/
/**************************************************/

	use "$projectfolder\cchs_2012_subset_cleaned.dta", replace
	count
	keep if gen_08==1 & lbsg31==1 /*restrict to cohort inclusion criteria for all analyses below*/
/**************************************************/
/*Mlogit*/
/**************************************************/

	/*************************/
	/*Multinomial logistic; for this example, we reverse the direction of association just as an example*/
	/*************************/
	mlogit low_job_control_q4 distress_0to24, baseoutcome(1) rrr

/**************************************************/
/*SEM/path analysis/CFA*/
/**************************************************/

	/*************************/
	/*With a simple regression model, estimates are the same*/
	/*************************/
	regress distress_0to24 low_job_control_0to20
	sem (distress_0to24 <- low_job_control_0to20)

	/*************************/
	/*Example path model for mediation; let's just pretend that education is a continuous variable, reverse scored; exclude covariates for illustration*/
	/*************************/

		generate educati=4-educat+1
		/*examine education -> job control -> distress (for example purposes only)*/
		sem (distress_0to24 <- low_job_control_0to20 educati) (low_job_control_0to20 <- educati)
		estat teffects, compact
		dis .7796865*.1479965
		/*compare indirect effect magnitude from SEM product of coefficients with baron and kenny difference method (note no interactions in the model)*/
		regress distress_0to24 educati 
		regress distress_0to24 low_job_control_0to20 educati
		dis .1740357-.0597299

	/*************************/
	/*An "easy" way to do missing data imputation for regression models; the full sample will be used even if there is missing data on covariates*/
	/*************************/
	sem (distress_0to24 <- low_job_control_0to20 agecat female marstat educat)
	sem (distress_0to24 <- low_job_control_0to20 agecat female marstat educat), method(mlmv)
/**************************************************/
/*Survival analysis*/
/**************************************************/

	/*************************/
	/*Load sample dataset from the web; accessed April 2020*/
	/*************************/
	use "https://stats.idre.ucla.edu/stat/data/uis.dta", clear
	count

	/*************************/
	/*Declare structure of data*/
	/*************************/
	stset time, failure(censor)

	/*************************/
	/*Survival descriptives*/
	/*************************/
	sts list
	sts test treat, logrank
	sts graph
	sts graph, by(treat)

	/*************************/
	/*Semi-parametric regression*/
	/*************************/

		stcox treat age
		estat phtest
		stcurve, survival at1(treat=0) at2(treat=1)

		stcox treat age, tvc(treat)


	/*************************/
	/*Parametric regresion*/
	/*************************/
	streg treat age, distribution(weibull)

	/*************************/
	/*Flexible parametric regression*/
	/*************************/

		/*install user written command; <capture noisily> just skips through any errors*/
		capture noisily ssc install stpm2
		stpm2 treat age, df(2) tvc(treat) dftvc(3) scale(hazard) eform

		/*note the time-varying hazard ratios*/
		capture drop hr*
		predict hr, hrnumerator(treat 1) hrdenominator(treat 0) ci
		list hr* _t in 1/5
		twoway (line hr* _t if hr_uci<4, sort lcolor(black red red) lpattern(solid dash dash)), yline(1) yscale(log)

		/*note: these are survival curves predicted at specified values of covariates*/
		capture drop surv*
		predict surv0, survival at(treat 0 age 30)
		predict surv1, survival at(treat 1 age 30)
		twoway (line surv0 _t, sort) (line surv1 _t, sort)

		/*but you may want standardized survival curves using the user-written <stpm2_standsurv> command*/
		capture noisily ssc install stpm2_standsurv
		stpm2_standsurv, at1(treat 0) at2(treat 1) ci contrast(difference)
		twoway (line _at1 _t, sort) (line _at2 _t, sort)

/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/*EXERCISE 6*/
/*CORONAVIRUS EPIDEMIOLOGY*/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/
/****************************************************************************************************/

/**************************************************/
/*Let's import the data from the UofT COVID working group; big thanks to this group for aggregating the data*/
/*We can use the copy command to pull down a copy to local hard drive, but there are a few other ways you can do this*/
/*COVID-19 Canada Open Data Working Group. Epidemiological Data from the COVID-19 Outbreak in Canada. https://github.com/ishaberry/Covid19Canada. (Access Date).*/
/*NOTE: this import only works for as long as the URL remains consistent. If it changes, you will need to update the URL, or find/save a copy of the excel file manually, or find a new source of data and modify the variable names accordingly*/
/**************************************************/

/*NOTE: YOU WILL WANT TO RENAME YOUR FILES INDICATE THE DATE AND TIME OF EXTRACTION, AS YOU WILL BE RUNNING THIS CODE REPEATEDLY; YOU CAN DO THIS AUTOMATICALLY USING <c(current_date)> and <c(current_time)>*/
/*NOTE: THERE MAY BE A DIFFERENCE BETWEEN EXTRACTED DATE AND REPORTED DATE IN THE ACTUAL EXCEL FILE*/
global copyin "https://docs.google.com/spreadsheets/d/1D6okqtBS3S2NRC7GFVHzaZ67DuTw7LX49-fqSLwJyeo/export?format=xlsx"
global copyout "$projectfolder\covid_data_extracted_`c(current_date)'.xlsx"
copy "$copyin" "$copyout", replace

/**************************************************/
/*Import the data*/
/*here, I use the import command, but you can use the point and click interface to load data to see a preview*/
/*make sure you manually inspect the data; we need to start at row A4, since the first few rows are header rows; we also use the firstrow option to import the column names, and the clear option to clear any open data in the memory*/
/**************************************************/

import excel "$copyout", sheet("Cases") cellrange(A4) firstrow clear
describe, full

/**************************************************/
/*Manual data cleaning; drop extra rows and columns that were imported*/
/**************************************************/

drop Q-AA
drop if case_id==.
count

/*check for any exact duplicates; none*/
duplicates drop

/**************************************************/
/*EXPLORATORY DATA ANALYSIS*/
/**************************************************/

/*first thing you should do with new data is to visually inspect it and confirm the data structure*/
browse

/*data is unique at level of case_id*/
codebook case_id
capture noisily ssc install unique, replace
unique case_id

/*but we also have provincial_case_id*/
/*note that some provinces re-use the same ID, so we could use case_id to identify rows, or a combination of provincial_case_id and province (with provincial_case_id nested within province)*/
unique provincial_case_id
unique provincial_case_id province

/**************************************************/
/*LET'S FOCUS ON ONTARIO*/
/**************************************************/

	tab province, missing
	keep if province=="Ontario"
	count

/**************************************************/
/*CREATE VARIABLES*/
/**************************************************/

	/*************************/
	/*age is messy; lowest common grouping is <20; you can keep Not Reported as a distinct numeric code, or as a missing value code to automatically exclude from commands*/
	/*************************/

		tab age, missing
		generate agecat=1 if inlist(age,"<1","2","10-19","<10","<18","<20")
		replace agecat=2 if age=="20-29"
		replace agecat=3 if age=="30-39"
		replace agecat=4 if age=="40-49"
		replace agecat=5 if inlist(age,"50","50-59")
		replace agecat=6 if age=="60-69" | age=="61"
		replace agecat=7 if inlist(age,"70-79","80-89","90-99")
		replace agecat=999 if age=="Not Reported" /*you can either recode as . or as a distinct category*/

		capture label drop agecatf
		label define agecatf ///
			1 "<20" ///
			2 "20-29" ///
			3 "30-39" ///
			4 "40-49" ///
			5 "50-59" ///
			6 "60-69" ///
			7 "70-99" ///
			999 "Not reported"
		label values agecat agecatf

		tab agecat, missing
		tab age agecat, missing
		tab report_week agecat, missing
	/*************************/
	/*sex*/
	/*you can keep Not Reported as a distinct numeric code, or as a missing value code to automatically exclude from commands*/
	/*************************/

		generate female=1 if sex=="Female"
		replace female=0 if sex=="Male"
		replace female=9 if sex=="Not Reported"

		capture label drop femalef
		label define femalef 0 "0 Male" 1 "1 Female" 9 "9 Not Reported"
		label values female femalef

		tab female
		tab sex female, missing
		tab report_week sex, missing
	/*************************/
	/*province*/
	/*************************/

		tab province, missing
		generate prov="AB" if province=="Alberta"
		replace prov="BC" if province=="BC"
		replace prov="MB" if province=="Manitoba"
		replace prov="NB" if province=="New Brunswick"
		replace prov="NL" if province=="NL"
		replace prov="NT" if province=="NWT"
		replace prov="NS" if province=="Nova Scotia"
		replace prov="NU" if province=="Nunavut"
		replace prov="ON" if province=="Ontario"
		replace prov="PE" if province=="PEI"
		replace prov="QC" if province=="Quebec"
		replace prov="SK" if province=="Saskatchewan"
		replace prov="YT" if province=="Yukon"
		replace prov="RP" if province=="Repatriated"
		tab province prov, missing

		encode prov, generate(prov_id)
		tab prov_id, missing

		/*replace special characters for consistency*/
		tab health_region, missing
		replace health_region=upper(health_region)
		replace health_region=subinstr(health_region,"é","E",.)
		replace health_region=subinstr(health_region,"è","E",.)
		replace health_region=subinstr(health_region,"É","E",.)
		replace health_region=subinstr(health_region,"È","E",.)
		replace health_region=subinstr(health_region,"ô","O",.)
		replace health_region=subinstr(health_region,"Ô","O",.)
		replace health_region=subinstr(health_region,"Î","I",.)
		tab health_region, missing

		/*assign health region codes, based on mapping; for ON only*/
		/*you'll need to pull in mappings for other provinces*/
		/*note: the code for HURON PERTH needs to be checked, as these health regions were recently merged*/
		generate health_region_id=.
		replace health_region_id=3526 if health_region=="ALGOMA" & prov=="ON"
		replace health_region_id=3527 if health_region=="BRANT" & prov=="ON"
		replace health_region_id=3540 if health_region=="CHATHAM-KENT" & prov=="ON"
		replace health_region_id=3530 if health_region=="DURHAM" & prov=="ON"
		replace health_region_id=3558 if health_region=="EASTERN" & prov=="ON"
		replace health_region_id=3533 if health_region=="GREY BRUCE" & prov=="ON"
		replace health_region_id=3534 if health_region=="HALDIMAND-NORFOLK" & prov=="ON"
		replace health_region_id=3535 if health_region=="HALIBURTON KAWARTHA PINERIDGE" & prov=="ON"
		replace health_region_id=3536 if health_region=="HALTON" & prov=="ON"
		replace health_region_id=3537 if health_region=="HAMILTON" & prov=="ON"
		replace health_region_id=3538 if health_region=="HASTINGS PRINCE EDWARD" & prov=="ON"
		replace health_region_id=3593 if health_region=="HURON PERTH" & prov=="ON"
		replace health_region_id=3541 if health_region=="KINGSTON FRONTENAC LENNOX & ADDINGTON" & prov=="ON"
		replace health_region_id=3542 if health_region=="LAMBTON" & prov=="ON"
		replace health_region_id=3543 if health_region=="LEEDS GRENVILLE AND LANARK" & prov=="ON"
		replace health_region_id=3544 if health_region=="MIDDLESEX-LONDON" & prov=="ON"
		replace health_region_id=3546 if health_region=="NIAGARA" & prov=="ON"
		replace health_region_id=3547 if health_region=="NORTH BAY PARRY SOUND" & prov=="ON"
		replace health_region_id=3549 if health_region=="NORTHWESTERN" & prov=="ON"
		replace health_region_id=3551 if health_region=="OTTAWA" & prov=="ON"
		replace health_region_id=3575 if health_region=="SOUTHWESTERN" & prov=="ON"
		replace health_region_id=3553 if health_region=="PEEL" & prov=="ON"
		replace health_region_id=3555 if health_region=="PETERBOROUGH" & prov=="ON"
		replace health_region_id=3556 if health_region=="PORCUPINE" & prov=="ON"
		replace health_region_id=3557 if health_region=="RENFREW" & prov=="ON"
		replace health_region_id=3560 if health_region=="SIMCOE MUSKOKA" & prov=="ON"
		replace health_region_id=3561 if health_region=="SUDBURY" & prov=="ON"
		replace health_region_id=3562 if health_region=="THUNDER BAY" & prov=="ON"
		replace health_region_id=3563 if health_region=="TIMISKAMING" & prov=="ON"
		replace health_region_id=3595 if health_region=="TORONTO" & prov=="ON"
		replace health_region_id=3565 if health_region=="WATERLOO" & prov=="ON"
		replace health_region_id=3566 if health_region=="WELLINGTON DUFFERIN GUELPH" & prov=="ON"
		replace health_region_id=3568 if health_region=="WINDSOR-ESSEX" & prov=="ON"
		replace health_region_id=3570 if health_region=="YORK" & prov=="ON"
		replace health_region_id=3599 if health_region=="NOT REPORTED" & prov=="ON"
		replace health_region_id=9999 if health_region=="NOT REPORTED" & prov=="RP"

/**************************************************/
/*CREATE SUMMARY CASE COUNT VARIABLES - BY PROVINCE*/
/**************************************************/

	preserve

		/*since one row equals one case, collapse data to daily level to generate daily case count*/
		generate unique_case=1
		collapse (sum) cases_daily=unique_case, by(prov_id prov date_report)
		count

		/*calculate daily growth in cases; first, specify the data as time series so that growth calculations will respect the time structure and skip missing periods; the daily growth percentage is volatile and there are other/better methods that you can use to produce smoothed estimates, but this is just a quick number summary for example*/
		tsset prov_id date_report
		generate cases_daily_growth_past1d=((cases_daily-L.cases_daily)/L.cases_daily)*100 if prov_id[_n]==prov_id[_n-1]

		/*calculate cumulative cases to date*/
		sort prov_id date_report
		by prov_id: generate cases_cumulative=sum(cases_daily)

		/*display case count*/
		list, noobs separator(0) ab(20)

		/*extract latest report date (which may differ from date that the excel file was downloaded)*/
		sum date_report
		global maxreport: dis %td r(max)
		dis "$maxreport"

		/*save for later merging with map files or to copy into a spreadsheet*/
		save "$projectfolder\covid_data_daily_cases_by_province_extracted_`c(current_date)'_reported_$maxreport.dta", replace

		/*graphs*/
		twoway (bar cases_daily date_report) if prov=="ON", graphregion(fcolor(white)) title("COVID-19 Daily Cases, Ontario, $maxreport", size(medium)) note("Data source: COVID-19 Canada Open Data Working Group", size(vsmall)) xtitle("Date Reported") ytitle("Daily Cases")
		graph export "$projectfolder\covid_daily_cases_ontario_extracted_`c(current_date)'_reported_$maxreport.png", as(png) name("Graph") replace

	restore

/**************************************************/
/*CREATE SUMMARY CASE COUNT VARIABLES - BY PROVINCE AND HEALTH REGION*/
/**************************************************/

	preserve

		/*since one row equals one case, collapse data to daily level to generate daily case count*/
		generate unique_case=1
		collapse (sum) cases_daily=unique_case, by(prov_id prov health_region_id health_region date_report)
		count

		/*calculate daily growth in cases; first, specify the data as time series so that growth calculations will respect the time structure and skip missing periods; the daily growth percentage is volatile and there are other/better methods that you can use to produce smoothed estimates, but this is just a quick number summary for example*/
		tsset health_region_id date_report
		generate cases_daily_growth_past1d=((cases_daily-L.cases_daily)/L.cases_daily)*100 if prov_id[_n]==prov_id[_n-1]

		/*calculate cumulative cases to date*/
		sort prov_id health_region_id date_report
		by prov_id health_region_id: generate cases_cumulative=sum(cases_daily)

		/*display case count*/
		list, noobs sepby(prov_id health_region_id) ab(15)

		/*extract latest report date (which may differ from date that the excel file was downloaded)*/
		sum date_report
		global maxreport: dis %td r(max)
		dis "$maxreport"

		/*save for later merging with map files or to copy into a spreadsheet*/
		save "$projectfolder\covid_data_daily_cases_by_healthregion_extracted_`c(current_date)'_reported_$maxreport.dta", replace

		/*graphs*/
		twoway (bar cases_daily date_report) if prov=="ON" & health_region=="TORONTO", graphregion(fcolor(white)) title("COVID-19 Daily Cases, Toronto Region, $maxreport", size(medium)) note("Data source: COVID-19 Canada Open Data Working Group", size(vsmall)) xtitle("Date Reported") ytitle("Daily Cases")
		graph export "$projectfolder\covid_daily_cases_toronto_extracted_`c(current_date)'_reported_$maxreport.png", as(png) name("Graph") replace

		/*save current case count for maps below; we just need to keep the row of data that contains the total cumulative case count*/
		gsort prov_id health_region_id -date_report
		drop if prov_id[_n]==prov_id[_n-1] & health_region_id[_n]==health_region_id[_n-1]
		save "$projectfolder\covid_data_current_cases_by_healthregion_extracted_`c(current_date)'_reported_$maxreport.dta", replace

	restore

/**************************************************/
/*CREATE MAP OF CASE COUNTS BY HEALTH REGION 2018*/
/*Stata can convert digital boundary files ARCGIS format, downloaded here: https://www150.statcan.gc.ca/pub/82-402-x/2018001/data-donnees/boundary-limites/arcinfo/HR_000a18a-eng.zip*/
/*You will need to download the shape file and DBF file (you can use the automated code below to download and unzip) and place in the project directory (i.e., HR_000a18a_e.shp, HR_000a18a_e.dbf)*/
capture noisily copy "https://www150.statcan.gc.ca/pub/82-402-x/2018001/data-donnees/boundary-limites/arcinfo/HR_000a18a-eng.zip" "$projectfolder/HR_000a18a-eng.zip"
capture noisily unzipfile "$projectfolder/HR_000a18a-eng.zip"

	/*************************/
	/*first, let's install a user-written program to convert the ARCGIS map data and to generate the maps*/
	/*************************/
	capture noisily ssc install shp2dta
	capture noisily ssc install spmap
	/*************************/
	/*convert the SHP data to Stata format*/
	/*you need to specify the location of the SHP file that you downloaded above, and the output dataset that will hold the two converted files*/
	/*************************/
	shp2dta using "$projectfolder\HR_000a18a_e.shp", data("$projectfolder\health_regions_data.dta") coor("$projectfolder\health_regions_coordinates.dta") genid(id) genc(centroid) replace

	/*************************/
	/*Next, load the converted data; this will contain the health regions, with the underlying coordinates contained in a separate database that we will reference later with the spmap command*/
	/*************************/
	use "$projectfolder\health_regions_data.dta", clear
	list, noobs separator(0) ab(10) string(10)

	/*************************/
	/*Clean up the data*/
	/*************************/

		/*Extract province name from the HR_UID variable*/
		generate province_code=substr(HR_UID,1,2)

		/*Extract health region ID*/
		destring HR_UID, generate(health_region_id)

	/*************************/
	/*Let's keep Ontario only, but you can map for all of Canada*/
	/*************************/
	keep if province_code=="35"
	count

	/*************************/
	/*Note: HURON PERTH was merged into one health region, but the maps do not reflect this*/
	/*We need to update the health region ID in the map so that we can properly merge in cases, which are reported for huron perth*/
	/*Note: the maps will show the health region as two separate regions, but will have the same case count for both regions; so it may look like there are twice as many cases in the region as a whole; need to fix the maps by dropping the boundary between the two health regions*/
	/*************************/
	tab ENGNAME if inlist(health_region_id,3554,3539)
	replace health_region_id=3593 if inlist(health_region_id,3554,3539)

	/*************************/
	/*merge cases many to one, since some regions are collapsed for cases but not for the maps (as noted above)*/
	/*note: current case counts by health region are generated above*/
	/*************************/

		merge m:1 health_region_id using "$projectfolder\covid_data_current_cases_by_healthregion_extracted_`c(current_date)'_reported_$maxreport.dta"
		tab _merge, missing

		/*drop cases that have no region reported or were repatriated cases (we could draw these as an external shape)*/
		list if _merge==2, noobs separator(0)
		drop if _merge==2
		count

		/*extract latest report date (which may differ from date that the excel file was downloaded)*/
		sum date_report
		global maxreport: dis %td r(max)
		dis "$maxreport"

	/*************************/
	/*map cases*/
	/*************************/
	spmap cases_cumulative using "$projectfolder\health_regions_coordinates.dta" if province_code=="35", id(id) fcolor(Blues) clnumber(5) legend(symy(*1) symx(*1) size(*1) position(2)) title("COVID-19 Total Cases by Health Region, $maxreport", size(medium)) note("Data source: COVID-19 Canada Open Data Working Group" "Note: Boundaries for Huron-Perth are not updated; case counts apply to the region as a whole, not to each separate region", size(tiny))
	graph export "$projectfolder\map_of_covid_daily_cases_by_health_region_extracted_`c(current_date)'_reported_$maxreport.png", as(png) name("Graph") replace

	/*************************/
	/*display cases*/
	/*************************/
	list health_region_id health_region cases_cumulative, noobs sepby(health_region_id)
