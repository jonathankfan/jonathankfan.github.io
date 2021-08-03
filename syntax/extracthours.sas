
/**************************************************/
/**************************************************/
/*IMPORT DATA*/
/**************************************************/
/**************************************************/

	/*FIRST, SPECIFY FOLDER LOCATION FOR CSV FILE; OR LOAD SAS DATA FILE DIRECTLY BELOW*/
	%let rootpath=C:\;

	proc import 
	datafile="&rootpath.\hoursexample.csv"
	out=hoursexample
	dbms=csv
	replace;
	getnames=yes;
	guessingrows=max;
	run;

	proc print data=hoursexample;
	run;

/**************************************************/
/**************************************************/
/*LOAD DATA AND APPLY REGEX*/
/**************************************************/
/**************************************************/

	data hoursexample_cleaned;
	set hoursexample;

		/**************************************************/
		/*perform initial data cleaning*/
		/**************************************************/

			/*change to uppercase, then strip leading and trailing blanks*/
			workhours_cleaned=strip(upcase(workhours));

			/*CAN OPTIONALLY REMOVE SPECIAL CHARACTERS HERE*/
			*workhours_cleaned=compress(workhours,'()@$');

			/*extract type of characters contained for reference*/
			if anyalpha(workhours_cleaned)>0 and anydigit(workhours_cleaned)>0 then workhours_cleaned_type="alphanumeric";
			if anyalpha(workhours_cleaned)>0 and anydigit(workhours_cleaned)=0 then workhours_cleaned_type="alpha";
			if anyalpha(workhours_cleaned)=0 and anydigit(workhours_cleaned)>0 then workhours_cleaned_type="numeric";

		/**************************************************/
		/*search for numbers that could represent work hours (1 or 2 digits plus decimals)*/
		/**************************************************/

			/*
			set regex parameters to search for:
			"\.?" = optional (ocurring zero or one times) decimal at beginning (e.g., if ".5 hours"), plus
			"\d+" = one or more digits, plus
			"(\.\d+)*" = optional (ocurring zero or more times) decimal with more digits (note: change the "*" to "?" to only include the digits after the first decimal place (e.g., 12.3), and exclude the digits after any other decimal places that may have been included in error (e.g., 12.3.4)
			*/
			pattern=prxparse("/\.?\d+(\.\d+)*/"); /*specify the pattern to be searched; prxparse just assigns the desired regex pattern to the macro variable "pattern"*/
			string=workhours_cleaned; /*specify the name of the variable containing the text to be searched*/
			start=1; /*tell the function to search the entire string*/
			end=length(string); /*tell the function to search the entire string*/

			/*Using the above parameters, the prxnext function returns the position and length of a substring that matches a pattern and iterates over multiple matches within one string*/
			call prxnext(pattern,start,end,string,position,length);

			/*now, find up to 5 matches and store in new string variables called extractedhours1-extractedhours5 (can increase if needed)*/
			array extractedhours[5] $100;
			do i=1 to 5 while(position>0);
				extractedhours[i]=substr(string,position,length); /*for the given string variable, extract the portion of text starting at position determined by prxnext, and extract for length determined by prxnext*/
				call prxnext(pattern,start,end,string,position,length); /*call up the next matching position and length and restart loop*/
			end;

			/*count of how many work hours were actually extracted from the string text*/
			extractedhours_count=i-1;

			/*optional: can include logical bounds for validity checks (e.g., if hours greater than 100)*/
			*if extractedhours1>1 and extractedhours1<100 then extractedhours1_valid=1;

		/**************************************************/
		/*optional: search for numbers written out as a word (there's a way to modify the above code to search for words directly)*/
		/**************************************************/

		/*drop temporary variables*/
		drop pattern string start end position length i;

	run;

/**************************************************/
/**************************************************/
/*PRINT RESULTS*/
/**************************************************/
/**************************************************/

	title "print data";
	proc print data=hoursexample_cleaned;
	run;

/**************************************************/
/**************************************************/
/*VALIDATE RESULTS*/
/**************************************************/
/**************************************************/

	title "work hours were extracted from these string values; confirm these are valid";
	proc freq data=hoursexample_cleaned;
	where extractedhours_count>0;
	table workhours /missing;
	run;

	title "work hours were not extracted from these string values; if some contain valid work hours, then create new regex rules, add to regex syntax, and repeat cycle until all values are correctly classified";
	proc freq data=hoursexample_cleaned;
	where extractedhours_count=0;
	table workhours /missing;
	run;

/**************************************************/
/**************************************************/
/*EXPORT RESULTS*/
/**************************************************/
/**************************************************/

	/*dataset with coded variables*/

		data hours_coded;
		set hoursexample_cleaned;
		where extractedhours_count>0;
		run;

		proc export data=hours_coded 
		dbms=xlsx 
		outfile="&rootpath.\hours_coded.xlsx" 
		replace;
		run;
		
	/*dataset with uncoded variables that requires manual cleaning*/

		data hours_uncoded;
		set hoursexample_cleaned;
		where extractedhours_count=0;
		run;

		proc export data=hours_uncoded 
		dbms=xlsx 
		outfile="&rootpath.\hours_uncoded.xlsx" 
		replace;
		run;
