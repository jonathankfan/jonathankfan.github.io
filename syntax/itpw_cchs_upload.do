
/****************************************************************************************************/
/****************************************************************************************************/
/*CONTENTS*/
/****************************************************************************************************/
/****************************************************************************************************/

/*
ABOUT
SET WORKING DIRECTORY
IMPORT DATA
INCLUSION CRITERIA
CREATE ADDITIONAL VARIABLES
ANALYSIS: RELATIONSHIP BETWEEN JOB CONTROL (BINARY TREATMENT VARIABLE) AND MENTAL HEALTH OUTCOMES
ANALYSIS: RELATIONSHIP BETWEEN JOB CONTROL (CATEGORICAL TREATMENT VARIABLE) AND MENTAL HEALTH OUTCOMES
*/

/****************************************************************************************************/
/****************************************************************************************************/
/*ABOUT*/
/*
This syntax generates propensity scores and IPTW weights for a published paper (with some modifications) that examined the relationship between psychosocial work conditions and mental health outcomes among workers, using data from the CCHS 2012 Mental Health survey.
The analysis below uses files from the Public Use Microdata Files. The original syntax files and raw data are located at the Statistics Canada Research Data Centre (Toronto), given privacy considerations.
The paper was published by Fan et al. (2019) in the Annals of Work Exposures and Health (2019;63(5):546-559).
See here for published paper: https://raw.githubusercontent.com/jonathankfan/jonathankfan.github.io/main/publications/Fan.2019.ANNWEH.pdf
*/
/****************************************************************************************************/
/****************************************************************************************************/

/****************************************************************************************************/
/****************************************************************************************************/
/*SET WORKING DIRECTORY AND LOG FILE FOR OUTPUTS*/
/****************************************************************************************************/
/****************************************************************************************************/

global projectfolder "C:\"

/*
capture noisily log close
log using "$projectfolder\logname.log", replace
*/

/****************************************************************************************************/
/****************************************************************************************************/
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
/****************************************************************************************************/
/****************************************************************************************************/

/****************************************************************************************************/
/****************************************************************************************************/
/*LOAD DATA*/
/****************************************************************************************************/
/****************************************************************************************************/

/********************************************************************************/
/*Load data*/
/********************************************************************************/

use "$projectfolder\cchs2012_mh_old.dta", clear
count

/*rename all variables to lowercase*/
rename *, lower

foreach var of varlist * {
quietly levelsof `var', local(temp)
local count: word count `temp'
if `count'>50 {
dis "`var': `count'"
}
}

/********************************************************************************/
/*Specify inclusion criteria*/
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
/*SPECIFY SURVEY DESIGN WEIGHTS*/
/********************************************************************************/

svyset [pweight=wts_m]

/********************************************************************************/
/*CREATE ADDITIONAL VARIABLES*/
/********************************************************************************/

/*depression in past 12 months: yes/no*/
generate dep_yn=0 if depddy==2
replace dep_yn=1 if depddy==1
tab dep_yn depddy

/*Job control (combination of skill discretion and decision authority) - continuous*/
generate jobcontrol=wstdski+wstdaut

/*Job control (combination of skill discretion and decision authority) - binary*/
sum jobcontrol, d
generate jobcontrol_binary=1 if jobcontrol>=8 & jobcontrol!=.
replace jobcontrol_binary=0 if jobcontrol<8 & jobcontrol!=.
tab jobcontrol jobcontrol_binary

/*Job control (combination of skill discretion and decision authority) - categorical, based on quantiles of the data*/
generate jobcontrol_q4=1 if jobcontrol>=0 & jobcontrol<5
replace jobcontrol_q4=2 if jobcontrol>=5 & jobcontrol<7
replace jobcontrol_q4=3 if jobcontrol>=7 & jobcontrol<9
replace jobcontrol_q4=4 if jobcontrol>=9 & jobcontrol!=.
tab jobcontrol_q4
tab jobcontrol jobcontrol_q4, missing

/****************************************************************************************************/
/****************************************************************************************************/
/*ANALYSIS: RELATIONSHIP BETWEEN JOB CONTROL (BINARY TREATMENT VARIABLE) AND MENTAL HEALTH OUTCOMES*/
/****************************************************************************************************/
/****************************************************************************************************/

/**************************************************/
/*first, specify propensity score model (treatment as the outcome)*/
/**************************************************/

	/*NOTE: use survey weights to generate pscore; could also incorporate survey weights as a covariate in the model*/
	/*https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5802372*/
	/*https://academic.oup.com/biostatistics/article/20/1/147/4780267*/
	/*https://journals.sagepub.com/doi/full/10.1177/0193841X20938497*/
	svyset [pweight=wts_m]
	svy: logistic jobcontrol_binary wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe
	capture drop pscore
	predict pscore, pr

	/*unweighted - use this option if not interested in generalizing to study population of CCHS survey
	logistic jobcontrol_binary wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe
	capture drop pscore
	predict pscore, pr
	*/

		/*note: accuracy measures/c-statistics/auc are not useful for assessing peformance of the propensity score, as it is not a prediction score (see: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4213057); also, inclusion of non-confounders of the outcome relationship could increase c-statistic without decreasing bias in treatment-outcome relationship, and in the extreme case of a randomized trial, balance will be achieved with a specific propensity score model despite the c-statistic equal to only 0.5*/
		logistic jobcontrol_binary wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe
		lroc
		roctab jobcontrol_binary pscore
		generate pr_true=1 if pscore>=.5 & pscore!=.
		replace pr_true=0 if pscore<.5
		tab pr_true jobcontrol_binary, chi2
		tab pr_true jobcontrol_binary, col nofreq chi2
		tab pr_true jobcontrol_binary, row nofreq chi2
		dis (4704+2194)/10535
		drop pr_true

		/*confirm that propensity scores for each treatment per individual add up to 1*/
		generate temp=pscore+(1-pscore)
		tab temp
		drop temp

		/*check region of common support, i.e., positivity assumption*/
		/*if there are regions outside of common support, then we could use pscore matching with calipers or restrict IPTW analysis to region of common support (or to pscores within 0.1 to 0.9), to reduce bias (see: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5564952)*/
		/*note: the teffects and mmws overlap plots will produce the same*/
		hist pscore, by(jobcontrol_binary)
		kdensity pscore if jobcontrol_binary==0, addplot(kdensity pscore if jobcontrol_binary==1)

			/*violations of positivity: these observations have low job control (low probability of having high job control) - individuals are all similar, but low common support, as we want some individuals with high job control to match with those who have low job control*/
			gsort -pscore
			brow jobcontrol_binary pscore wstdpsy wstdsoc wstdphy wstdjin geo_prv dhh_sex dhhgms dhhghsz dhhgdwe if pscore<0.2
			tab pscore jobcontrol_binary if pscore<0.2

			/*violations of positivity: these observations have high job control (high probability of having high job control) - individuals are all similar, but low common support, as we want some individuals with low job control to match with those who have high job control*/
			gsort -pscore
			brow jobcontrol_binary pscore wstdpsy wstdsoc wstdphy wstdjin geo_prv dhh_sex dhhgms dhhghsz dhhgdwe if pscore>0.95 & pscore!=.
			tab pscore jobcontrol_binary if pscore>0.95 & pscore!=.

		/*check mean of pscore by treatment, within strata of pscore*/
		xtile pscore_q5=pscore, nq(5)
		tabstat pscore, by(pscore_q5) statistics(n min max)

			/*the distributions look equivalent within strata of pscore*/
			kdensity pscore if jobcontrol_binary==0, addplot(kdensity pscore if jobcontrol_binary==1)
			kdensity pscore if jobcontrol_binary==0 & pscore_q5==1, addplot(kdensity pscore if jobcontrol_binary==1 & pscore_q5==1)
			kdensity pscore if jobcontrol_binary==0 & pscore_q5==2, addplot(kdensity pscore if jobcontrol_binary==1 & pscore_q5==2)
			kdensity pscore if jobcontrol_binary==0 & pscore_q5==3, addplot(kdensity pscore if jobcontrol_binary==1 & pscore_q5==3)
			kdensity pscore if jobcontrol_binary==0 & pscore_q5==4, addplot(kdensity pscore if jobcontrol_binary==1 & pscore_q5==4)
			kdensity pscore if jobcontrol_binary==0 & pscore_q5==5, addplot(kdensity pscore if jobcontrol_binary==1 & pscore_q5==5)
			bysort pscore_q5: tabstat pscore, by(jobcontrol_binary) statistics(n min mean max)

/**************************************************/
/*next, generate itpw weights using the propensity score*/
/**************************************************/

	/**************************************************/
	/*itpw weights - unstabilized*/
	/**************************************************/

		/*use 1/1-pr for control group, which the same as using the reverse scored outcome to generate pr followed by 1/pr*/
		/*note, this formula same as below, but in one line: generate iptw=(jobcontrol_binary/pscore) + ((1-jobcontrol_binary)/(1-pscore))*/
		capture drop iptw
		generate iptw=1/pscore if jobcontrol_binary==1
		replace iptw=1/(1-pscore) if jobcontrol_binary==0
		sum iptw, d
		/*check total weighted population - this version does not sum to study population*/
		dis `r(sum)'

		/*in addition to using the survey weights in the propensity score model, multiply the IPTW weights by the survey weights*/
		/*https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5802372*/
		/*https://academic.oup.com/biostatistics/article/20/1/147/4780267*/
		/*https://journals.sagepub.com/doi/full/10.1177/0193841X20938497*/
		replace iptw=iptw*wts_m

		/*apply weights for use in survey regression commands (to create IPTW weighted population)*/
		svyset [pweight=iptw]

	/**************************************************/
	/*itpw weights - stabilized version*/
	/**************************************************/

		/*this calculates the baseline prevalence of the treatment variable*/
		logistic jobcontrol_binary
		predict pscore_baseline, pr
		sum pscore_baseline, d
		tab jobcontrol_binary

		/*multiply the weights by the baseline prevalence of treatment and control in the overall sample (marginal probability of treatment)*/
		/*use 1/1-pr for control group, which would be same as using the reverse scored outcome to generate pr followed by 1/pr*/
		capture drop iptw_stab
		generate iptw_stab=pscore_baseline/pscore if jobcontrol_binary==1
		replace iptw_stab=(1-pscore_baseline)/(1-pscore) if jobcontrol_binary==0
		sum iptw_stab, d
		/*check total weighted population - this version sums to study population*/
		dis `r(sum)'

			scatter iptw iptw_stab

		/*apply weights*/
		*svyset [pweight=iptw_stab]

	/**************************************************/
	/*itpw weights - ATT version*/
	/**************************************************/

		/*multiply the weights by the pscore (treated subjects are assigned a weight of 1, and controls are weighted by the odds of receiving treatment; this standardizes the treated and control populations to the reference treated population)*/
		capture drop iptw_att
		generate iptw_att=pscore/pscore if jobcontrol_binary==1
		replace iptw_att=pscore/(1-pscore) if jobcontrol_binary==0
		sum iptw_att, d
		/*check total weighted population - this version sums to study population*/
		dis `r(sum)'

			scatter iptw iptw_att

		/*apply weights*/
		*svyset [pweight=iptw_att]

	/**************************************************/
	/*can trim extreme weights*/
	/**************************************************/

		sum iptw, d
		capture drop iptw_trimmed
		generate iptw_trimmed=iptw
		replace iptw_trimmed=`r(p1)' if iptw_trimmed<`r(p1)'
		replace iptw_trimmed=`r(p99)' if iptw_trimmed>`r(p99)' & iptw_trimmed!=.
		sum iptw_trimmed, d
		scatter iptw iptw_trimmed

		/*apply weights*/
		*svyset [pweight=iptw_trimmed]

/**************************************************/
/*assess balance of covariates across treatment groups - balance is achieved*/
/**************************************************/

	/*unweighted*/

	tabstat wstdpsy wstdsoc wstdphy wstdjin, statistics(mean sd) by(jobcontrol_binary)
		qqplot3 wstdpsy, by(jobcontrol_binary)
		graph box wstdpsy, over(jobcontrol_binary)
		graph box wstdsoc, over(jobcontrol_binary)
		graph box wstdphy, over(jobcontrol_binary)
		graph box wstdjin, over(jobcontrol_binary)
	tab geo_prv jobcontrol_binary, col nofreq chi2
	tab dhh_sex jobcontrol_binary, col nofreq chi2
	tab dhhgms jobcontrol_binary, col nofreq chi2
	tab dhhghsz jobcontrol_binary, col nofreq chi2
	tab dhhgdwe jobcontrol_binary, col nofreq chi2

	/*check standardized differences, which allow comparison of means without regard to units or scales, and without regard to sample size, and since we are not concerned with the population from which the sample was drawn*/
	*stddiff wstdpsy wstdsoc wstdphy wstdjin geo_prv dhh_sex dhhgms dhhghsz dhhgdwe, by(jobcontrol_binary)

		/*unmatched*/

			stddiffi 3.733827 2.127126 3.140643 2.058106

			quietly sum wstdsoc if jobcontrol_binary==0
			local mean0=r(mean)
			local sd0=r(sd)

			quietly sum wstdsoc if jobcontrol_binary==1
			local mean1=r(mean)
			local sd1=r(sd)

				dis (`mean1'-`mean0')/((((`sd1'^2)+(`sd0'^2))/2)^(1/2))

		/*within strata of pscore*/

			quietly sum wstdsoc if jobcontrol_binary==0 & pscore_q5==1
			local mean0=r(mean)
			local sd0=r(sd)

			quietly sum wstdsoc if jobcontrol_binary==1 & pscore_q5==1
			local mean1=r(mean)
			local sd1=r(sd)

				dis (`mean1'-`mean0')/((((`sd1'^2)+(`sd0'^2))/2)^(1/2))

			quietly sum wstdsoc if jobcontrol_binary==0 & pscore_q5==3
			local mean0=r(mean)
			local sd0=r(sd)

			quietly sum wstdsoc if jobcontrol_binary==1 & pscore_q5==3
			local mean1=r(mean)
			local sd1=r(sd)

				dis (`mean1'-`mean0')/((((`sd1'^2)+(`sd0'^2))/2)^(1/2))

			quietly sum wstdsoc if jobcontrol_binary==0 & pscore_q5==5
			local mean0=r(mean)
			local sd0=r(sd)

			quietly sum wstdsoc if jobcontrol_binary==1 & pscore_q5==5
			local mean1=r(mean)
			local sd1=r(sd)

				dis (`mean1'-`mean0')/((((`sd1'^2)+(`sd0'^2))/2)^(1/2))

		/*weighted with iptw*/

			stddiffi 3.530568 2.279335 3.483204 2.080622

			quietly sum wstdsoc [aweight=iptw] if jobcontrol_binary==0
			local mean0=r(mean)
			local sd0=r(sd)

			quietly sum wstdsoc [aweight=iptw] if jobcontrol_binary==1
			local mean1=r(mean)
			local sd1=r(sd)

				dis (`mean1'-`mean0')/((((`sd1'^2)+(`sd0'^2))/2)^(1/2))

	/*balance within blocks - similar to matching*/

		tabstat wstdpsy wstdsoc wstdphy wstdjin if pscore_q5==1, statistics(mean) by(jobcontrol_binary)
		tab geo_prv jobcontrol_binary if pscore_q5==1, col nofreq chi2
		tab dhh_sex jobcontrol_binary if pscore_q5==1, col nofreq chi2
		tab dhhgms jobcontrol_binary if pscore_q5==1, col nofreq chi2
		tab dhhghsz jobcontrol_binary if pscore_q5==1, col nofreq chi2
		tab dhhgdwe jobcontrol_binary if pscore_q5==1, col nofreq chi2

		tabstat wstdpsy wstdsoc wstdphy wstdjin if pscore_q5==5, statistics(mean) by(jobcontrol_binary)
		tab geo_prv jobcontrol_binary if pscore_q5==5, col nofreq chi2
		tab dhh_sex jobcontrol_binary if pscore_q5==5, col nofreq chi2
		tab dhhgms jobcontrol_binary if pscore_q5==5, col nofreq chi2
		tab dhhghsz jobcontrol_binary if pscore_q5==5, col nofreq chi2
		tab dhhgdwe jobcontrol_binary if pscore_q5==5, col nofreq chi2

	/*itpw weighted*/
	tabstat wstdpsy wstdsoc wstdphy wstdjin [aweight=iptw], statistics(mean sd) by(jobcontrol_binary)
		qqplot3 wstdpsy [pw=iptw], by(jobcontrol_binary)
		graph box wstdpsy [aweight=iptw], over(jobcontrol_binary)
		graph box wstdsoc [aweight=iptw], over(jobcontrol_binary)
		graph box wstdphy [aweight=iptw], over(jobcontrol_binary)
		graph box wstdjin [aweight=iptw], over(jobcontrol_binary)
	svy: tab geo_prv jobcontrol_binary, col
	svy: tab dhh_sex jobcontrol_binary, col
	svy: tab dhhgms jobcontrol_binary, col
	svy: tab dhhghsz jobcontrol_binary, col
	svy: tab dhhgdwe jobcontrol_binary, col

/**************************************************/
/*models*/
/**************************************************/

	/*unadjusted*/
	logistic dep_yn jobcontrol_binary

	/*regression adjusted with no pscore*/
	logistic dep_yn jobcontrol_binary wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe

	/*direct pscore adjustment*/
	logistic dep_yn jobcontrol_binary pscore, robust

	/*direct pscore adjustment using strata*/
	logistic dep_yn jobcontrol_binary i.pscore_q5, robust

	/*double robust pscore with covariates used for pscore model*/
	logistic dep_yn jobcontrol_binary pscore wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe, robust

	/*iptw adjustment*/
	/*use robust estimator to account for the fact that the weights are estimated using estimated pscore; could also use bootstrap methods*/
	logistic dep_yn jobcontrol_binary [pweight=iptw], robust
	logistic dep_yn jobcontrol_binary [pweight=iptw_stab], robust
	logistic dep_yn jobcontrol_binary [pweight=iptw_trimmed], robust

		/*treatment effects on the probability scale (linear probability model)*/
		/*compare with linear probability model from teffects*/
		svy: logistic dep_yn jobcontrol_binary, nolog
		margins, by(jobcontrol_binary)
		margins, over(jobcontrol_binary)
		margins, dydx(jobcontrol_binary)
		dis .0579262-.0444154

		/*iptw using teffects - note, same as margins based on logit model, or linear probability model*/
		regress dep_yn jobcontrol_binary [pweight=iptw], robust
		teffects ipw (dep_yn) (jobcontrol_binary wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe, logit), aequations

	/*same as above, using means, demonstrating model independence*/
	regress dep_yn jobcontrol_binary [pweight=iptw], robust
	svy: mean dep_yn, over(jobcontrol_binary)
	lincom _b[1]-_b[0]
	tabstat dep_yn [aweight=iptw], statistics(mean sd) by(jobcontrol_binary)
	dis .0541224-.038796

	/*iptw using mmws command - see help files for examples*/

		mmws jobcontrol_binary, pscore(pscore) iptw figure replace /*common*/
			dis `r(suppmin1)'
			dis `r(suppmax1)'

		/*in addition to using the survey weights in the propensity score model, multiply the IPTW weights by the survey weights*/
		/*https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5802372*/
		/*https://academic.oup.com/biostatistics/article/20/1/147/4780267*/
		/*https://journals.sagepub.com/doi/full/10.1177/0193841X20938497*/
		replace _mmws=_mmws*wts_m
		replace _iptw=_iptw*wts_m
		scatter _mmws _iptw

		brow iptw _iptw
		corr iptw _iptw
		scatter iptw _iptw
		/*weights will not be equal if "common" option used, as weights will only be created for observations with scores that are in region of common support*/
		capture noisily assert iptw==_iptw

		/*weights are the same as iptw manual*/
		logistic dep_yn i.jobcontrol_binary [pweight=_mmws], robust
		logistic dep_yn i.jobcontrol_binary [pweight=_iptw], robust
		margins jobcontrol_binary
		dis .0541224-.038796
		marginsplot, plotopts(connect(i))
		margins jobcontrol_binary, pwcompare(effects) mcompare(bonferroni)

	/*cem - matching on covariates with no pscore*/
	/*note: should coarsen manually for nominal variables like geo_prv*/

		/*run matching algorithm*/
		cem wstdpsy wstdsoc wstdphy wstdjin geo_prv dhh_sex (#0) dhhgms (#0) dhhghsz dhhgdwe (#0) if jobcontrol_binary!=., treatment(jobcontrol_binary)
		tab cem_weights
		logistic dep_yn jobcontrol_binary [iweight=cem_weights], robust

			/*double robust with covariates used for matching*/
			logistic dep_yn jobcontrol_binary wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe [iweight=cem_weights], robust

		/*cem with pscore matching*/
		cem pscore if jobcontrol_binary!=., treatment(jobcontrol_binary)
		tab cem_weights
		tab jobcontrol_binary cem_matched, missing
		scatter pscore cem_strata
		logistic dep_yn jobcontrol_binary i.cem_strata, robust
		logistic dep_yn jobcontrol_binary [iweight=cem_weights], robust

			/*double robust with covariates used for matching*/
			logistic dep_yn jobcontrol_binary wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe [iweight=cem_weights], robust

	/*iptw adjustment - double robust with covariates used for pscore model*/
	svy: logistic dep_yn jobcontrol_binary wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe, nolog

	/*
	/*in region of common support*/
	sum pscore if jobcontrol_binary==0
	local min_control=r(min)
	local max_control=r(max)
	sum pscore if jobcontrol_binary==1
	local min_treated=r(min)
	local max_treated=r(max)
	local min_region=max(`min_control',`min_treated')
	local max_region=min(`max_control',`max_treated')
	dis `min_region'
	dis `max_region'
	tab jobcontrol_binary if pscore<`min_region'
	tab jobcontrol_binary if pscore>`max_region' & pscore!=.
	tab jobcontrol_binary if pscore>=`min_region' & pscore<=`max_region'

	/*use psmatch2 for matching but supply pscore*/
	tabstat pscore if _support==0, by(jobcontrol_binary) statistics(n min max)
	tabstat pscore if _support==1, by(jobcontrol_binary) statistics(n min max)
	tabstat pscore if _support==., by(jobcontrol_binary) statistics(n min max)

	/*psmatch2 gives the same pscore, but also handles matching and the outcome model*/
	psmatch2 jobcontrol_binary, logit outcome(dep_yn) pscore(pscore) ate common
	psmatch2 jobcontrol_binary wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe, logit outcome(dep_yn) ate
	psmatch2 jobcontrol_binary wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe, logit outcome(dep_yn) ate neighbor(1)
	dis `r(att)'
	dis `r(ate)'
	corr pscore _pscore
	*/

/****************************************************************************************************/
/****************************************************************************************************/
/*ANALYSIS: RELATIONSHIP BETWEEN JOB CONTROL (CATEGORICAL TREATMENT VARIABLE) AND MENTAL HEALTH OUTCOMES*/
/****************************************************************************************************/
/****************************************************************************************************/

/**************************************************/
/*first, specify propensity score model (treatment as the outcome)*/
/**************************************************/

	/*use mlogit for categorical treatment variable - note, predict commands use the same baseoutcome model (no need to run differently for each outcome), only need to change the predict syntax*/
	/*NOTE: use survey weights to generate pscore; could also incorporate survey weights as a covariate in the model*/
	/*https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5802372*/
	/*https://academic.oup.com/biostatistics/article/20/1/147/4780267*/
	/*https://journals.sagepub.com/doi/full/10.1177/0193841X20938497*/
	mlogit jobcontrol_q4 wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe, baseoutcome(1) rrr
	svyset [pweight=wts_m]
	svy: mlogit jobcontrol_q4 wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe, baseoutcome(1) rrr
	capture drop pscore_1
	predict pscore_1, pr equation(1)
	sum pscore_1, d
	capture drop pscore_2
	predict pscore_2, pr equation(2)
	sum pscore_2, d
	capture drop pscore_3
	predict pscore_3, pr equation(3)
	sum pscore_3, d
	capture drop pscore_4
	predict pscore_4, pr equation(4)
	sum pscore_4, d

	/*can also use this command*/
	*predict pscore_*, pr
	*desc pscore_*, f
	*predict pscore_1 pscore_2 pscore_3 pscore_4, pr

		/*confirm that propensity scores for each treatment per individual add up to 1*/
		generate temp=pscore_1+pscore_2+pscore_3+pscore_4
		tab temp
		drop temp

		/*check region of common support, i.e., positivity assumption*/
		/*if there are regions outside of common support, then we could use pscore matching with calipers or restrict IPTW analysis to region of common support (or to pscores within 0.1 to 0.9), to reduce bias (see: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5564952)*/
		/*note: the teffects and mmws overlap plots will produce the same*/
		hist pscore_1, by(jobcontrol_q4)
		hist pscore_2, by(jobcontrol_q4)
		hist pscore_3, by(jobcontrol_q4)
		hist pscore_4, by(jobcontrol_q4)

			/*probability of having low job control - same as teffects overlap plot*/
			kdensity pscore_1 if jobcontrol_q4==1, addplot(kdensity pscore_1 if jobcontrol_q4==2 || kdensity pscore_1 if jobcontrol_q4==3 || kdensity pscore_1 if jobcontrol_q4==4)

			/*probability of having moderate job control - same as teffects overlap plot*/
			kdensity pscore_2 if jobcontrol_q4==1, addplot(kdensity pscore_2 if jobcontrol_q4==2 || kdensity pscore_2 if jobcontrol_q4==3 || kdensity pscore_2 if jobcontrol_q4==4)

			/*probability of having moderate job control - same as teffects overlap plot*/
			kdensity pscore_3 if jobcontrol_q4==1, addplot(kdensity pscore_3 if jobcontrol_q4==2 || kdensity pscore_3 if jobcontrol_q4==3 || kdensity pscore_3 if jobcontrol_q4==4)

			/*probability of having high job control - same as teffects overlap plot*/
			kdensity pscore_4 if jobcontrol_q4==1, addplot(kdensity pscore_4 if jobcontrol_q4==2 || kdensity pscore_4 if jobcontrol_q4==3 || kdensity pscore_4 if jobcontrol_q4==4)

				/*violations of positivity: these observations have low job control (low probability of having high job control) - individuals are all similar, but low common support, as we want some individuals with high job control to match with those who have low job control*/
				gsort -pscore_4
				brow jobcontrol_q4 pscore_4 wstdpsy wstdsoc wstdphy wstdjin geo_prv dhh_sex dhhgms dhhghsz dhhgdwe if pscore_4<0.05
				tab pscore_4 jobcontrol_q4 if pscore_4<0.05

				/*violations of positivity: these observations have high job control (high probability of having high job control) - individuals are all similar, but low common support, as we want some individuals with low job control to match with those who have high job control*/
				gsort -pscore_4
				brow jobcontrol_q4 pscore_4 wstdpsy wstdsoc wstdphy wstdjin geo_prv dhh_sex dhhgms dhhghsz dhhgdwe if pscore_4>0.8 & pscore_4!=.
				tab pscore_4 jobcontrol_q4 if pscore_4>0.8 & pscore_4!=.

/**************************************************/
/*next, generate itpw weights using the propensity score*/
/**************************************************/

	/**************************************************/
	/*itpw weights - unstabilized*/
	/**************************************************/

		/*use 1/1-pr for control group, which would be same as using the reverse scored outcome to generate pr followed by 1/pr*/
		/*note, this formula same as below, but in one line: generate iptw=(jobcontrol_binary/pscore) + ((1-jobcontrol_binary)/(1-pscore))*/
		capture drop iptw_q4
		generate iptw_q4=1/pscore_1 if jobcontrol_q4==1
		replace iptw_q4=1/pscore_2 if jobcontrol_q4==2
		replace iptw_q4=1/pscore_3 if jobcontrol_q4==3
		replace iptw_q4=1/pscore_4 if jobcontrol_q4==4
		sum iptw_q4, d
		/*check total weighted population - this version does not sum to study population*/
		dis `r(sum)'	

		/*in addition to using the survey weights in the propensity score model, multiply the IPTW weights by the survey weights*/
		/*https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5802372*/
		/*https://academic.oup.com/biostatistics/article/20/1/147/4780267*/
		/*https://journals.sagepub.com/doi/full/10.1177/0193841X20938497*/
		replace iptw_q4=iptw_q4*wts_m

		/*apply weights for use in survey regression commands (to create IPTW weighted population)*/
		svyset [pweight=iptw_q4]

	/**************************************************/
	/*itpw weights - stabilized*/
	/**************************************************/

		/*this calculates the baseline prevalence of the treatment variable*/

			/*use mlogit - note, predict commands use the same baseoutcome model (no need to run differently for each outcome), only need to change the predict syntax*/
			tab jobcontrol_q4
			mlogit jobcontrol_q4, baseoutcome(1)
			capture drop pscore_1_base
			predict pscore_1_base, pr equation(1)
			sum pscore_1_base, d
			capture drop pscore_2_base
			predict pscore_2_base, pr equation(2)
			sum pscore_2_base, d
			capture drop pscore_3_base
			predict pscore_3_base, pr equation(3)
			sum pscore_3_base, d
			capture drop pscore_4_base
			predict pscore_4_base, pr equation(4)
			sum pscore_4_base, d

				/*scores for each individual add up to 1*/
				generate temp=pscore_1_base+pscore_2_base+pscore_3_base+pscore_4_base
				tab temp
				drop temp

		/*multiply the weights by the baseline prevalence of treatment and control in the overall sample (marginal probability of treatment)*/
		/*use 1/1-pr for control group, which would be same as using the reverse scored outcome to generate pr followed by 1/pr*/
		capture drop iptw_stab_q4
		generate iptw_stab_q4=pscore_1_base/pscore_1 if jobcontrol_q4==1
		replace iptw_stab_q4=pscore_2_base/pscore_2 if jobcontrol_q4==2
		replace iptw_stab_q4=pscore_3_base/pscore_3 if jobcontrol_q4==3
		replace iptw_stab_q4=pscore_4_base/pscore_4 if jobcontrol_q4==4
		sum iptw_stab_q4, d
		/*check total weighted population - this version sums to study population*/
		dis `r(sum)'

			scatter iptw_q4 iptw_stab_q4

		/*apply weights*/
		*svyset [pweight=iptw_stab_q4]

	/**************************************************/
	/*can trim extreme weights*/
	/**************************************************/

		sum iptw_q4, d
		capture drop iptw_q4_trimmed
		generate iptw_q4_trimmed=iptw_q4
		replace iptw_q4_trimmed=`r(p1)' if iptw_q4_trimmed<`r(p1)'
		replace iptw_q4_trimmed=`r(p99)' if iptw_q4_trimmed>`r(p99)' & iptw_q4_trimmed!=.
		sum iptw_q4_trimmed, d
		scatter iptw_q4 iptw_q4_trimmed

		/*apply weights*/
		*svyset [pweight=iptw_q4_trimmed]

/**************************************************/
/*assess balance of covariates across treatment groups - balance is achieved*/
/**************************************************/

	/*unweighted*/
	tabstat wstdpsy wstdsoc wstdphy wstdjin, statistics(mean) by(jobcontrol_q4)
		graph box wstdpsy, over(jobcontrol_q4)
		graph box wstdsoc, over(jobcontrol_q4)
		graph box wstdphy, over(jobcontrol_q4)
		graph box wstdjin, over(jobcontrol_q4)
	tab geo_prv jobcontrol_q4, col nofreq chi2
	tab dhh_sex jobcontrol_q4, col nofreq chi2
	tab dhhgms jobcontrol_q4, col nofreq chi2
	tab dhhghsz jobcontrol_q4, col nofreq chi2
	tab dhhgdwe jobcontrol_q4, col nofreq chi2

	/*itpw weighted*/
	tabstat wstdpsy wstdsoc wstdphy wstdjin [aweight=iptw_q4], statistics(mean) by(jobcontrol_q4)
		graph box wstdpsy [aweight=iptw_q4], over(jobcontrol_q4)
		graph box wstdsoc [aweight=iptw_q4], over(jobcontrol_q4)
		graph box wstdphy [aweight=iptw_q4], over(jobcontrol_q4)
		graph box wstdjin [aweight=iptw_q4], over(jobcontrol_q4)
	svy: tab geo_prv jobcontrol_q4, col
	svy: tab dhh_sex jobcontrol_q4, col
	svy: tab dhhgms jobcontrol_q4, col
	svy: tab dhhghsz jobcontrol_q4, col
	svy: tab dhhgdwe jobcontrol_q4, col

/**************************************************/
/*models*/
/**************************************************/

	/*unadjusted*/
	logistic dep_yn i.jobcontrol_q4

	/*regression adjusted with no pscore*/
	logistic dep_yn i.jobcontrol_q4 wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe

	/*direct pscore adjustment*/
	logistic dep_yn i.jobcontrol_q4 pscore_2 pscore_3 pscore_4, robust

	/*double robust pscore with covariates used for pscore model*/
	logistic dep_yn i.jobcontrol_q4 pscore_2 pscore_3 pscore_4 wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe, robust

	/*iptw adjustment*/
	/*use robust estimator to account for the fact that the weights are estimated using estimated pscore; could also use bootstrap methods*/
	logistic dep_yn i.jobcontrol_q4 [pweight=iptw_q4], robust
	logistic dep_yn i.jobcontrol_q4 [pweight=iptw_stab_q4], robust
	logistic dep_yn i.jobcontrol_q4 [pweight=iptw_q4_trimmed], robust

		/*treatment effects on the probability scale (linear probability model)*/
		/*compare with linear probability model from teffects*/
		svy: logistic dep_yn i.jobcontrol_q4, nolog
		margins, by(jobcontrol_q4)
		margins, over(jobcontrol_q4)
		margins, dydx(jobcontrol_q4)
		dis .0656436-.0438357

		/*iptw using teffects - note, same as margins based on logit model, or linear probability model*/
		/*NEED TO UPDATE WITH MULTINOMIAL TREATMENT*/
		regress dep_yn i.jobcontrol_q4 [pweight=iptw_q4], robust
		*teffects ipw (dep_yn) (jobcontrol_q4 wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe, logit), aequations

	/*same as above, using means, demonstrating model independence*/
	regress dep_yn i.jobcontrol_q4 [pweight=iptw_q4], robust
	svy: mean dep_yn, over(jobcontrol_q4)
	lincom _b[4]-_b[1]
	tabstat dep_yn [aweight=iptw_q4], statistics(mean sd) by(jobcontrol_q4)
	dis .0656436-.0438357

	/*using mmws command - see help files for examples*/

		mmws jobcontrol_q4, pscore(pscore_1 pscore_2 pscore_3 pscore_4) nominal iptw figure replace /*common*/
			dis `r(suppmin1)'
			dis `r(suppmax1)'

		/*in addition to using the survey weights in the propensity score model, multiply the IPTW weights by the survey weights*/
		/*https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5802372*/
		/*https://academic.oup.com/biostatistics/article/20/1/147/4780267*/
		/*https://journals.sagepub.com/doi/full/10.1177/0193841X20938497*/
		replace _mmws=_mmws*wts_m
		replace _iptw=_iptw*wts_m
		scatter _mmws _iptw

		brow iptw_q4 _iptw
		corr iptw_q4 _iptw
		scatter iptw_q4 _iptw
		/*weights will not be equal if "common" option used, as weights will only be created for observations with scores that are in region of common support*/
		capture noisily assert iptw_q4==_iptw

		/*weights are the same as iptw manual*/
		logistic dep_yn i.jobcontrol_q4 [pweight=_mmws], robust
		logistic dep_yn i.jobcontrol_q4 [pweight=_iptw], robust
		margins jobcontrol_q4
		dis .0656436-.0438357
		marginsplot, plotopts(connect(i))
		margins jobcontrol_q4, pwcompare(effects) mcompare(bonferroni)

	/*iptw adjustment - double robust with covariates used for pscore model*/
	svy: logistic dep_yn i.jobcontrol_q4 wstdpsy wstdsoc wstdphy wstdjin i.geo_prv i.dhh_sex i.dhhgms i.dhhghsz i.dhhgdwe, nolog
