
*************************************************************************************
/* 
Purpose: RUN PROPENSITY SCORES AND ESTIMATE EFFECTS USING AIPW 
Created: 2024/03/16 (YYYY/MM/DD)
By: KF
*/


// path globals for test program
if c(username) == "mjone91" {
	global pathdat "/Users/mjone91/Dropbox/Research/Aboriginal/ASETS/Analysis/program_prep/testdata"
	global pathout  "/Users/mjone91/Dropbox/Research/Aboriginal/ASETS/Analysis/program_prep/mjtestdata/Revision"
	global pathlog  "/Users/mjone91/Dropbox/Research/Aboriginal/ASETS/Analysis/program_prep/mjtestdata/Revision"
	global pathprog  "/Users/mjone91/Dropbox/Research/Aboriginal/ASETS/Analysis/program_prep/mjtestdata"	
}

if c(username) == "kef289" {
	global pathdat "C:\Users\kef289\Dropbox\ASETS Eval\Analysis\program_prep\testdata"
	global pathout  "C:\Users\kef289\Dropbox\ASETS Eval\Analysis\program_prep\testdata\out"
	global pathlog  "C:\Users\kef289\Dropbox\ASETS Eval\Analysis\program_prep\testdata\out"
	global pathprog  "C:\Users\kef289\Dropbox\ASETS Eval\Analysis\program_prep"	
}

else {
adopath + "E:\data\SRDC WORK\ado"
	global path100 "//mnapesd4299/data/LMPDP_V2.0/STATA FILES/100_Percent_Data"
	global path10 "//mnapesd4299/data/LMPDP_V2.0/STATA FILES/10_Percent_Data"
	global pathdat "E:/data/SRDC WORK/Kelly/dat"
	global pathout "//mnapesd4299/data/SRDC WORK/Kelly/dat/Revision"
	global pathlog  "//mnapesd4299/data/SRDC WORK/Kelly/out/Revision"
	global pathprog  "//mnapesd4299/data/SRDC WORK/Kelly/prog/Revision"
}

cd "$pathout"

capture log close
log using "${pathlog}/MAIN_LOG.txt", text replace

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//
//
//	 OPEN DATA AND SELECT YEARS
//  
//
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

use "${pathdat}\ANALYSIS_DATA.dta", clear


// There are a few cases of ASETS before 2010, but that is the
// official date of program start so we drop < 2010
// The T1 and T4 data only extends to 2016 so I'll drop 
// the 2016 and 2017 cohorts. If we use two years out for outcomes
// then the 2015 cohort will also need to be dropped
// 2010 is a small cohort, so for robustness we may also want to drop
// 2010 to make sure it doesn't affect the results 


if c(username) != "mjone91" & c(username) != "kef289"   {
keep if   inrange(yrstart,2010,2014)

// handful of observations with missing gender
drop if female == .

// handful of observations with missing province

drop if prov == .


// drop student work experience 

drop if swespell == 1
 
  }
 
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//
//
//	 DEFINE OUTCOMES AND TREATMENT
//  
//
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


global treatvar training

if c(username) != "mjone91" & c(username) != "kef289"   {
#delim ;

global covars 
"c.age##c.age i.kids  i.disability  ib(4).marstat i.rural i.yrstart
c.earnpre1##c.earnpre1 c.earnpre2##c.earnpre2 c.earnpre3##c.earnpre3 c.earnpre4##c.earnpre4 c.earnpre5##c.earnpre5
c.cumearn##c.cumearn
c.totincpre1##c.totincpre1 c.cuminc    
c.saincpre1 i.sapre1 c.cumsainc i.csadum 
c.cumaearn##c.cumaearn 
i.fnworkev  
i.emppre1 i.emppre2 i.emppre3 i.emppre4 i.emppre5
i.nopiort4  i.lastt4un i.t4ls5yr   
c.nunion5yr##c.nunion5yr c.nt4in5yr##c.nt4in5yr 
ib(10).einoc1dum i.roesep
i.lsintercomp i.pstrain c.lastintm##c.lastintm c.pstrm##c.pstrm
i.fsamiss c.pcun c.pcearn   
i.eielig c.wkeiearn c.eihrs c.eigap 
c.ampr5yto##c.ampr5yto c.wkpr5yto##c.wkpr5yto
" ;

global meansvars 
"age    married   kids  disability  rural
yeardum1 yeardum2 yeardum3 yeardum4 yeardum5
emppre1 emppre2 emppre3 emppre4 emppre5 
sapre1 csadum saincpre1  cumsainc
fnworkev cumaearn  
earnpre1   earnpre2  earnpre3 earnpre4  earnpre5 cumearn
lastintm  pstrm lsintercomp pstrain 
totincpre1   cuminc   
nopiort4 lastt4un t4ls5yr nunion5yr  nt4in5yr  
nolastroe baselinework  
eielig1 eielig2 eielig3 eielig4   
wkeiearn eihrs eigap    ampr5yto wkpr5yto
pcun pcearn fsamiss
 " ;
 
#delim cr

capture drop dearnpst2
capture drop dearnpst1
capture drop demppst2
capture drop demppst1

gen dearnpst2 = earnpst2 - earnpre1
gen dearnpst1 = earnpst1 - earnpre1
gen demppst2 = emppst2 - emppre1
gen demppst1 = emppst2 - emppre1

global margvars "age i.kids  i.disability  ib(4).marstat i.rural i.yrstart earnpre1 fnworkev emppre1 nopiort4 totincpre1 i.lsintercomp i.pstrain"
	
global outvars = "earnpst1 earnpst2 emppst1 emppst2 wkfnpst1 wkfnpst2 tearnpst1 tearnpst2 takehmpst1 takehmpst2 dearnpst2 dearnpst1 demppst2 demppst1"



global senseout = "earnpst1 earnpst2 emppst1 emppst2"
global benchvar = "earnpre1 fnworkev emppre1 i.lsintercomp i.pstrain i.rural i.nopiort4"



}



if c(username) == "mjone91" | c(username) == "kef289"   {


capture drop dearnpst2
capture drop dearnpst1
capture drop demppst2
capture drop demppst1

gen dearnpst2 = earnpst2 - earnpre1
gen dearnpst1 = earnpst1 - earnpre1
gen demppst2 = emppst2 - emppre1
gen demppst1 = emppst2 - emppre1
 
global treatvar training
global covars "i.age_12 i.marstat  c.tenure"

global margvars "tenure"

//global outvars = "earnpst1 earnpst2 emppst1 emppst2 earnpst1 earnpst2 emppst1 emppst2 emppst1 emppst2"
global outvars = "earnpst1 earnpst2"


global meansvars "tenure mardum1 mardum2 mardum3 mardum4 mardum5"
global senseout = "earnpst1 earnpst2"
global benchvar = "i.marstat  tenure"



}

global ycov $covars

global trimup .9
global trimdown .1


capture erase "${pathlog}\MAIN_AIPW_RESULTS.xlsx"

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//
//
//	RUN PROPENSITY SCORES AND TRIM VARS OR LOAD THEM IN 
//  
//
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logit $treatvar  ${covars}  i.female i.Client_Ab

capture drop pscoreout
capture drop trimout
logit $treatvar  ${covars}  i.female i.Client_Ab
parmest ,  saving("$pathdat/temp", replace) idn(1) ids(Full) escal(N)
predict pscoreout if e(sample), p

capture drop pq
xtile pq = pscoreout, nq(1000)
gen trimout = pscoreout > $trimup | pscoreout  < $trimdown | pq == 1 | pq == 1000 if e(sample)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("Pscore_Full_Coef", replace) keepcellfmt firstrow(var)
capture erase  "$pathdat/temp"
restore

margins , dydx(i.female i.Client_Ab $margvars) post
parmest ,  saving("$pathdat/temp", replace) idn(1) ids(Full) escal(N)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("Pscore_Full_Marg", replace) keepcellfmt firstrow(var)
capture erase  "$pathdat/temp"
restore

logit $treatvar pscoreout  ${covars}  i.female i.Client_Ab if trimout == 0
parmest ,  saving("$pathdat/temp", replace) idn(1) ids(Full) escal(N)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("Pscore_Full_Test", replace) keepcellfmt firstrow(var)
capture erase  "$pathdat/temp"
restore


testparm ${covars}  i.female i.Client_Ab
local cntpvalue =r(p)
local cntchi  =r(chi2)
putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("Pscore_Full_Test")  modify
putexcel L1 = "ChiSquare"
putexcel M1 = "pvalue" 
putexcel L2 = `cntchi'
putexcel M2 = `cntpvalue'



//START RUNNING HISTORGRAM
if c(username) == "kef289" | c(username) == "mjone91"  {

qui sum pq

replace pq = 1 if pq == r(min)
replace pq = 1000 if pq == r(max)
}


sum pscoreout if pq == 1
local qtrimlow = r(min)
sum pscoreout if pq == 1000
local qtrimhigh = r(max)
 local bwidth .025


 
 #delim ;
 capture noisily tw histogram pscoreout if $treatvar == 1   , 
 lcolor(black) fc(none) name("traintemp", replace) title("High-Intensity Group", size(small))
 graphr(color(white)) xtitle("Propensity score", size(vsmall)) width(`bwidth') start(0) ytitle(" ") xscale(range(0 1))
 xline($trimup, lcolor(red)) xline($trimdown, lcolor(red)) xline(`qtrimlow', lcolor(blue) lpattern(dash)) xline(`qtrimhigh', lcolor(blue)  lpattern(dash)) ;
 capture noisily tw histogram pscoreout if $treatvar == 0  , lcolor(black) 
 fc(none) name("conttemp", replace) title("Low-Intensity Group", size(small))  
 graphr(color(white)) xtitle("Propensity score", size(vsmall)) width(`bwidth')  xscale(range(0 1))
 start(0) ytitle(" ")  xline($trimup, lcolor(red)) xline($trimdown, lcolor(red))  xline(`qtrimlow', lcolor(blue) lpattern(dash)) xline(`qtrimhigh', lcolor(blue)  lpattern(dash)) ;


capture noisily graph combine traintemp  conttemp, ycommon xcommon 
title("Overlapping Support: Full Sample", size(small)) 
  graphr(color(white)) col(1)
saving("${pathlog}\Mainolap", replace) ;
#delim cr


//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//	INTERACTED GROUPS
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

local idgroup 2 

local ee = uchar(233)
local tg0 "Men"
local tg1 "Women"
local tn1 "Status First Nations"
local tn2 "M`ee'tis"
local tn3 "Inuit"
local tn4 "Non-Status First Nations"
local tn5 "Uncategorised"

foreach j in  1 2 4  {
forval gg = 0(1)1 {



logit $treatvar  ${covars}  if female == `gg'  & Client_Ab == `j'

capture drop N`j'G`gg'pscoreout
capture drop N`j'G`gg'trimout
logit $treatvar  ${covars}   if female == `gg'  & Client_Ab == `j'
parmest ,  saving("$pathdat/temp", replace) idn(`idgroup') ids(N`j'G`gg') escal(N)
predict N`j'G`gg'pscoreout if e(sample), p

capture drop pq
xtile pq = N`j'G`gg'pscoreout, nq(1000)
gen N`j'G`gg'trimout = N`j'G`gg'pscoreout > $trimup | N`j'G`gg'pscoreout  < $trimdown | pq == 1 | pq == 1000 if e(sample)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("Pscore_N`j'G`gg'_Coef", replace) keepcellfmt firstrow(var)
capture erase  "$pathdat/temp"
restore

margins , dydx($margvars) post
parmest ,  saving("$pathdat/temp", replace) idn(`idgroup') ids(N`j'G`gg') escal(N)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("Pscore_N`j'G`gg'_Marg", replace) keepcellfmt firstrow(var)
capture erase  "$pathdat/temp"
restore

logit $treatvar N`j'G`gg'pscoreout  ${covars} if N`j'G`gg'trimout == 0  & female == `gg'  & Client_Ab == `j'
parmest ,  saving("$pathdat/temp", replace) idn(`idgroup') ids(N`j'G`gg') escal(N)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("Pscore_N`j'G`gg'_Test", replace) keepcellfmt firstrow(var)
capture  erase  "$pathdat/temp"
restore


testparm ${covars} 
local cntpvalue =r(p)
local cntchi  =r(chi2)
putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("Pscore_N`j'G`gg'_Test")  modify
putexcel L1 = "ChiSquare"
putexcel M1 = "pvalue" 
putexcel L2 = `cntchi'
putexcel M2 = `cntpvalue'







local bwidth .025
 
sum  N`j'G`gg'pscoreout if pq == 1
local qtrimlow = r(max)
sum  N`j'G`gg'pscoreout if pq == 1000
local qtrimhigh = r(min)


 

//check the cellsizes, the arguments are propensity score and cell width
 
 #delim ;
 capture noisily tw histogram N`j'G`gg'pscoreout if $treatvar == 1  & Client_Ab == `j' & female == `gg' `NGsel' , 
 lcolor(black) fc(none) name("traintemp", replace) title("High-Intensity Group", size(small))  xscale(range(0 1))
 graphr(color(white)) xtitle("Propensity score", size(vsmall)) width(`bwidth') start(0) ytitle(" ") 
 xline($trimup, lcolor(red)) xline($trimdown, lcolor(red)) xline(`qtrimlow', lcolor(blue) lpattern(dash)) xline(`qtrimhigh', lcolor(blue)  lpattern(dash));
 capture noisily tw histogram N`j'G`gg'pscoreout if $treatvar == 0 & Client_Ab == `j' & female == `gg' `NsGel' , lcolor(black) 
 fc(none) name("conttemp", replace) title("Low-Intensity Group", size(small))    xscale(range(0 1))
 graphr(color(white)) xtitle("Propensity score", size(vsmall)) width(`bwidth') 
 start(0) ytitle(" ")  xline($trimup, lcolor(red)) xline($trimdown, lcolor(red)) xline(`qtrimlow', lcolor(blue) lpattern(dash)) xline(`qtrimhigh', lcolor(blue)  lpattern(dash));

 
capture noisily graph combine traintemp  conttemp , ycommon xcommon 
title("Overlapping Support: `tn`j'' `tg`gg'' " , size(small)) 
  graphr(color(white)) col(1)
saving("${pathlog}\N`j'G`gg'Mainolap", replace) ;

#delim cr


local idgroup = `idgroup' +1
}
}


preserve 

keep Seqno *trim* *pscore*

save "${pathout}/main_sample_pscores.dta", replace

restore



//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//
//
//	CHECK FOR BALANCE WITHIN STRATA
//  
//
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

scalar drop _all
capture erase "${pathlog}\FULL_BALANCE_STRATA_ReRun.xlsx"


foreach vv in  female indentcat1 indentcat4 indentcat2 indentcat3 indentcat5 {
putexcel set "${pathlog}\FULL_BALANCE_STRATA_ReRun.xlsx", sheet("`vv'")  modify
forval i = .1(.1).8 {
local k = `i' + .1
local rr = `i'*10+1

di "selected >= `i' & < `k' row is `rr' "

putexcel A1 = "Strata"
putexcel B1 = "Mean0"
putexcel C1 = "Mean1"
putexcel D1 = "diff"
putexcel E1 = "Se diff"
putexcel F1 = "p-value"
putexcel A`rr' = "`i'"

capture noisily ttest `vv' if pscoreout >= `i' & pscoreout < `k' & trimout ==0, by(training) unequal

if _rc == 0 {
scalar mean0 = r(mu_1)  // first group is training == 0
scalar mean1 =r(mu_2) // second group is training == 1
scalar diff = mean1-mean0
scalar sediff = r(se)
scalar pdiff = r(p)
putexcel B`rr' =  mean0
putexcel C`rr' = mean1
putexcel D`rr' = diff
putexcel E`rr' =  sediff
putexcel F`rr' =   pdiff

}
}

}

// now all the other variables
foreach vv of global meansvars {



putexcel set "${pathlog}\FULL_BALANCE_STRATA_ReRun.xlsx", sheet("`vv'")  modify


forval i = .1(.1).8 {
local k = `i' + .1
local rr = `i'*10+1

di "selected >= `i' & < `k' row is `rr' "

putexcel A1 = "Strata"
putexcel B1 = "Mean0"
putexcel C1 = "Mean1"
putexcel D1 = "diff"
putexcel E1 = "Se diff"
putexcel F1 = "p-value"
putexcel A`rr' = "`i'"

capture noisily ttest `vv' if pscoreout >= `i' & pscoreout < `k' & trimout ==0, by(training) unequal

if _rc == 0 {
scalar mean0 = r(mu_1)  // first group is training == 0
scalar mean1 =r(mu_2) // second group is training == 1
scalar diff = mean1-mean0
scalar sediff = r(se)
scalar pdiff = r(p)
putexcel B`rr' =  mean0
putexcel C`rr' = mean1
putexcel D`rr' = diff
putexcel E`rr' =  sediff
putexcel F`rr' =   pdiff

}
}

}



*************************************************************************************
*************************************************************************************

//		BY GROUP

*************************************************************************************
*************************************************************************************

foreach j in 1 2 4  {

forval gg = 0(1)1 {



// clear the files to start fresh 
scalar drop _all
capture erase ${pathlog}\N`j'G`gg'_BALANCE_STRATA_ReRun.xlsx"



foreach vv of global meansvars {



putexcel set "${pathlog}\N`j'G`gg'_BALANCE_STRATA_ReRun.xlsx", sheet("`vv'")  modify


forval i = .1(.1).8 {
local k = `i' + .1
local rr = `i'*10+1

di "selected >= `i' & < `k' row is `rr' "

putexcel A1 = "Strata"
putexcel B1 = "Mean0"
putexcel C1 = "Mean1"
putexcel D1 = "diff"
putexcel E1 = "Se diff"
putexcel F1 = "p-value"
putexcel A`rr' = "`i'"

capture noisily ttest `vv' if female == `gg' & Client_Ab == `j' & N`j'G`gg'pscoreout >= `i' & N`j'G`gg'pscoreout < `k' & N`j'G`gg'trimout == 0, by(training) unequal

if _rc == 0 {
scalar mean0 = r(mu_1)  // first group is training == 0
scalar mean1 =r(mu_2) // second group is training == 1
scalar diff = mean1-mean0
scalar sediff = r(se)
scalar pdiff = r(p)
putexcel B`rr' =  mean0
putexcel C`rr' = mean1
putexcel D`rr' = diff
putexcel E`rr' =  sediff
putexcel F`rr' =   pdiff
}

}
}
}
}



//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//
//
//	TREATMENT EFFECTS
//  
//
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// FULL SAMPLE 

capture drop temprob 
capture drop temptrim

// the triming has been done with the full set of controls, and here there are sometimes cases where there are high proabilities in the lagged regressions
quietly logit $treatvar ${covars} i.female i.Client_Ab  if   trimout == 0 
predict temprob, p
gen temptrim = temprob > .9999 | temprob < .0001
ta temptrim
capture drop temprob 

gen fullsamp = trimout == 0  & temptrim == 0

global outname Main

local i = 1

foreach vv of  global outvars {

di "Loop  OUTCOME: `vv' OUTCOME NUM: `i' "
capture noisily teffects aipw (`vv'  $ycov  i.female i.Client_Ab ) ($treatvar ${covars}  i.female i.Client_Ab  ) if  trimout == 0  & temptrim == 0,  ate vce(cluster pcprev) iterate(100)

if _rc == 0 & e(converged) == 1 {

parmest ,  saving("$pathdat/temp", replace) idn(1) ids(Full) escal(N n1 n0)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("ATE_Full_`vv'", replace) keepcellfmt firstrow(var)
capture erase  "$pathdat/temp"
restore

if `i' == 1 {
tebalance summarize
mat outbalance = r(table)
putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("BALANCE_Full") modify
putexcel B1= "Standardized Raw Difference"
putexcel C1= "Standardized Weighted Difference"
putexcel D1= "Raw Variance ratio"
putexcel E1= "Weighted Variance ratio"
putexcel A2=matrix(outbalance), rownames
} // only save the balance once
} // converged


 local i = `i'+1
 } // close vv
 
 
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//	INTERACTED GROUPS
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

local idgroup 2
foreach j in  1 2 4  {
forval gg = 0(1)1 {
di "Loop j = `j' g =`gg' "
capture drop temprob 
capture drop temptrim

// the triming has been done with the full set of controls, and here there are sometimes cases where there are high probilities in the lagged regressions
quietly logit $treatvar ${covars}  if Client_Ab == `j' & female == `gg' & N`j'G`gg'trimout == 0 
predict temprob, p
gen temptrim = temprob > .9999 | temprob < .0001
ta temptrim
capture drop temprob 

gen N`j'G`gg'samp =  N`j'G`gg'trimout == 0  & temptrim == 0

local i = 1

foreach vv of  global outvars {

di "Loop j = `j' g =`gg'  OUTCOME: `vv' OUTCOME NUM: `i' "

capture noisily teffects aipw (`vv'  "$ycov" ) ($treatvar ${covars}  ) if female == `gg' & Client_Ab == `j'   &  N`j'G`gg'trimout == 0  & temptrim == 0,  ate vce(cluster pcprev) iterate(100)
if _rc == 0 & e(converged) == 1 {



parmest ,  saving("$pathdat/temp", replace) idn(`idgroup') ids( N`j'G`gg') escal(N n1 n0)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("ATE_ N`j'G`gg'_`vv'", replace) keepcellfmt firstrow(var)
capture  erase  "$pathdat/temp"
restore
if  `i' == 1 {
tebalance summarize
mat outbalance = r(table)
putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("BALANCE_ N`j'G`gg'") modify
putexcel B1= "Standardized Raw Difference"
putexcel C1= "Standardized Weighted Difference"
putexcel D1= "Raw Variance ratio"
putexcel E1= "Weighted Variance ratio"
putexcel A2=matrix(outbalance), rownames
} // close tebalance
}


local i = `i' +1
} // close the vv loop


local idgroup =`idgroup'+1
} // close the g loop
} //close the j loop


//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


//  MAKE PARTIAL R-SQUARED MATRICES


quietly pcorr earnpst2 $covars if fullsamp == 1
mat p1 =  r(p_corr) 
mat sp1 = r(sp_corr) 

local pp 2
foreach j in  1 2 4  {
forval gg = 0(1)1 {

di "N = `j' and G = `gg'"

quietly pcorr earnpst2 $covars if N`j'G`gg'samp == 1  & female == `gg' & Client_Ab == `j'
mat p`pp'=  r(p_corr) 
mat sp`pp' = r(sp_corr) 

local pp = `pp'+1 
}
}


putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("PARTIAL_CORRELATIONS") modify
putexcel A2=matrix(p1), rownames
putexcel B1= "Full sample"
putexcel C2=matrix(p1), rownames
putexcel D1= "Status First Nations Men"
putexcel E2=matrix(p1), rownames
putexcel F1= "Status First Nations Women"
putexcel G2=matrix(p1), rownames
putexcel H1= "Metis Men"
putexcel I2=matrix(p1), rownames
putexcel J1= "Metis Women"
putexcel K2=matrix(p1), rownames
putexcel L1= "non-Status Men"
putexcel M2=matrix(p1), rownames
putexcel N1= "non-Status Women"


putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("SEMI_PARTIAL_CORRELATIONS") modify
putexcel A2=matrix(sp1), rownames
putexcel B1= "Full sample"
putexcel C2=matrix(sp1), rownames
putexcel D1= "Status First Nations Men"
putexcel E2=matrix(sp1), rownames
putexcel F1= "Status First Nations Women"
putexcel G2=matrix(sp1), rownames
putexcel H1= "Metis Men"
putexcel I2=matrix(sp1), rownames
putexcel J1= "Metis Women"
putexcel K2=matrix(sp1), rownames
putexcel L1= "non-Status Men"
putexcel M2=matrix(sp1), rownames
putexcel N1= "non-Status Women"



//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


// PICK UP CONTROL GROUP MEANS

putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("CONTROL GROUP MEANS") modify

putexcel B1= "Full"
putexcel C1= "Status First Nations Men"
putexcel D1= "Status First Nations Women"
putexcel E1= "Metis Men"
putexcel F1= "Metis Women"
putexcel G1= "non-Status First Nations Men"
putexcel H1= "non-Status First Nations Women"




mean $outvars if fullsamp == 1 & $treatvar == 0
mat b = e(b)'

putexcel A2=matrix(b), rownames

local cc 3


foreach j in  1 2 4  {
forval gg = 0(1)1 {

di "N = `j' and G = `gg'"

local alpha `=char(`cc'+64)'
di "`alpha'"


 mean $outvars if N`j'G`gg'samp == 1  & female == `gg' & Client_Ab == `j' & $treatvar == 0
 mat b = e(b)'
 
putexcel `alpha'2=matrix(b)

local cc = `cc' +1
}
}



//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

// sample size and mean durations


 
putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("DURATIONS AND SAMPLE SIZES") modify

putexcel B1= "Full"
putexcel C1= "Status First Nations Men"
putexcel D1= "Status First Nations Women"
putexcel E1= "Metis Men"
putexcel F1= "Metis Women"
putexcel G1= "non-Status First Nations Men"
putexcel H1= "non-Status First Nations Women" 
 
putexcel A2= "Total High Intensity N"
putexcel A3= "Total Low Intensity N"
putexcel A4= "Sample High Intensity N"
putexcel A5= "Sample Low Intensity N"
putexcel A6= "Mean Duration High Intensity "
putexcel A7= "Median Duration High Intensity "
putexcel A8= "Mean Duration Low Intensity "
putexcel A9= "Median Duration Low Intensity "


inspect $treatvar
local control =r(N_0) 
local treated =r(N_pos) 
putexcel B2=  `treated'
putexcel B3=  `control' 

inspect $treatvar if fullsamp == 1
local control =r(N_0) 
local treated =r(N_pos) 
putexcel B4=  `treated'
putexcel B5=  `control' 

sum interdur if fullsamp == 1 & $treatvar == 1, detail
local intmean =r(mean) 
local intmed =r(p50) 
putexcel B6=  `intmean'
putexcel B7=  `intmed' 


sum interdur if fullsamp == 1 & $treatvar == 0, detail
local intmean =r(mean) 
local intmed =r(p50) 
putexcel B8=  `intmean'
putexcel B9=  `intmed' 


local cc 3

foreach j in  1 2 4  {
forval gg = 0(1)1 {

di "N = `j' and G = `gg'"

local alpha `=char(`cc'+64)'
di "`alpha'"


inspect $treatvar if female == `gg' & Client_Ab == `j'
local control =r(N_0) 
local treated =r(N_pos) 
putexcel `alpha'2=  `treated'
putexcel `alpha'3=  `control' 

inspect $treatvar if N`j'G`gg'samp == 1 & female == `gg' & Client_Ab == `j'
local control =r(N_0) 
local treated =r(N_pos) 
putexcel `alpha'4=  `treated'
putexcel `alpha'5=  `control' 

sum interdur if N`j'G`gg'samp == 1 & female == `gg' & Client_Ab == `j' & $treatvar == 1, detail
local intmean =r(mean) 
local intmed =r(p50) 
putexcel `alpha'6=  `intmean'
putexcel `alpha'7=  `intmed' 


sum interdur if N`j'G`gg'samp == 1 & female == `gg' & Client_Ab == `j' & $treatvar == 0, detail
local intmean =r(mean) 
local intmed =r(p50) 
putexcel `alpha'8=  `intmean'
putexcel `alpha'9=  `intmed' 
local cc = `cc' +1
}
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


//  OLS SENSE MAKER RESULTS 

foreach ov in $senseout {
local loopcount 1

foreach bm in $benchvar {

local saverow =`loopcount'*3 - 1
di "outvar `ov' and benchmark `bm' "


capture mat olsres drop
sensemakr earnpst2 training  $covars i.female i.Client_Ab if fullsamp == 1, treat(training) 
mat olsres =r(table)'

putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("SEN_COEF_`ov'") modify

local rnms: rownames  olsres
local nrms 2
foreach lbc of local rnms {
putexcel A`nrms'= "`lbc'"
local nrms = `nrms' +1
}
putexcel B1=matrix(olsres), colnames

putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("SEN_BOUND_`ov'") modify
if `loopcount' == 1 {
 putexcel B1 = "kd"     
 putexcel C1 = "ky"      
 putexcel D1 = "r2d_x"
 putexcel E1 = "r2yz_dx"   
 putexcel F1 = "adjusted_e"
 putexcel G1 = "adjusted_se"   
 putexcel H1 = "adjusted_t"     
 putexcel I1 = "lower_CI"    
 putexcel J1 = "upper_CI"
 putexcel K1 = "treat_coef"
 putexcel L1 = "treat_se"
 putexcel M1 = "r2yd_x"
 putexcel N1 = "rv_q"
 putexcel O1 = "rv_qa"
 }
 
capture noisily sensemakr `ov' training  $covars i.female i.Client_Ab if fullsamp == 1 , treat(training) benchmark(`bm')
if _rc == 0 {
 scalar treat_coef = e(treat_coef)
 scalar treat_se = e(treat_se)
 scalar r2yd_x= e(r2yd_x)
 scalar rv_q= e(rv_q)
 scalar rv_qa= e(rv_qa)
 mat bounds =   e(bounds)
putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("SEN_BOUND_`ov'") modify
if `loopcount' == 1 {
  putexcel K2 = treat_coef
 putexcel L2 = treat_se
 putexcel M2 = r2yd_x
 putexcel N2 = rv_q
 putexcel O2 = rv_qa
 }
 
 putexcel A`saverow' = matrix(bounds), rownames
 
 } // end rc exception

 local loopcount = `loopcount' +1
}  // benchmark loop
} // outcome loop

//******************************************************************************
//******************************************************************************
//		INTERACTED GROUPS
//******************************************************************************
//******************************************************************************

foreach j in  1 2 4  {
forval gg = 0(1)1 {

di "N = `j' and G = `gg'"

foreach ov in $senseout {
local loopcount 1

foreach bm in $benchvar {

local saverow =`loopcount'*3 - 1
di "outvar `ov' and benchmark `bm' "


capture mat olsres drop
sensemakr earnpst2 training  $covars if N`j'G`gg'samp == 1 & female == `gg' & Client_Ab == `j', treat(training) 
mat olsres =r(table)'

putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("N`j'G`gg'_SEN_COEF_`ov'") modify

local rnms: rownames  olsres
local nrms 2
foreach lbc of local rnms {
putexcel A`nrms'= "`lbc'"
local nrms = `nrms' +1
}
putexcel B1=matrix(olsres), colnames

putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("N`j'G`gg'_SEN_BOUND_`ov'") modify
if `loopcount' == 1 {
 putexcel B1 = "kd"     
 putexcel C1 = "ky"      
 putexcel D1 = "r2d_x"
 putexcel E1 = "r2yz_dx"   
 putexcel F1 = "adjusted_e"
 putexcel G1 = "adjusted_se"   
 putexcel H1 = "adjusted_t"     
 putexcel I1 = "lower_CI"    
 putexcel J1 = "upper_CI"
 putexcel K1 = "treat_coef"
 putexcel L1 = "treat_se"
 putexcel M1 = "r2yd_x"
 putexcel N1 = "rv_q"
 putexcel O1 = "rv_qa"
 }
 
capture noisily sensemakr `ov' training  $covars  if N`j'G`gg'samp == 1  & female == `gg' & Client_Ab == `j' , treat(training) benchmark(`bm')
if _rc == 0 {
 scalar treat_coef = e(treat_coef)
 scalar treat_se = e(treat_se)
 scalar r2yd_x= e(r2yd_x)
 scalar rv_q= e(rv_q)
 scalar rv_qa= e(rv_qa)
 mat bounds =   e(bounds)
putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("N`j'G`gg'_SEN_BOUND_`ov'") modify
if `loopcount' == 1 {
  putexcel K2 = treat_coef
 putexcel L2 = treat_se
 putexcel M2 = r2yd_x
 putexcel N2 = rv_q
 putexcel O2 = rv_qa
 }
 
 putexcel A`saverow' = matrix(bounds), rownames
 
 } // end rc exception

 local loopcount = `loopcount' +1
}  // benchmark loop
} // outcome loop

} // gender loop
} // pop group loop


 
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


//  LONGER RUN FOLLOW UP

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//
//
//	 OPEN DATA AND SELECT YEARS
//  
//
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

use "${pathdat}\ANALYSIS_DATA.dta", clear


if c(username) != "mjone91" & c(username) != "kef289"   {
keep if   inrange(yrstart,2010,2012)

// handful of observations with missing gender
drop if female == .

// handful of observations with missing province

drop if prov == .


// drop student work experience 

drop if swespell == 1
 
  }
 
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//
//
//	 DEFINE OUTCOMES AND TREATMENT
//  
//
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


global treatvar training

if c(username) != "mjone91" & c(username) != "kef289"   {
#delim ;

global covars 
"c.age##c.age i.kids  i.disability  ib(4).marstat i.rural i.yrstart
c.earnpre1##c.earnpre1 c.earnpre2##c.earnpre2 c.earnpre3##c.earnpre3 c.earnpre4##c.earnpre4 c.earnpre5##c.earnpre5
c.cumearn##c.cumearn
c.totincpre1##c.totincpre1 cuminc    
saincpre1 i.sapre1 cumsainc i.csadum 
c.cumaearn##c.cumaearn 
i.fnworkev  
i.emppre1 i.emppre2 i.emppre3 i.emppre4 i.emppre5
i.nopiort4  i.lastt4un i.t4ls5yr   
c.nunion5yr##c.nunion5yr c.nt4in5yr##c.nt4in5yr 
ib(10).einoc1dum i.roesep
i.lsintercomp i.pstrain c.lastintm##c.lastintm c.pstrm##c.pstrm
i.fsamiss pcun pcearn   
i.eielig wkeiearn eihrs eigap 
c.ampr5yto##c.ampr5yto c.wkpr5yto##c.wkpr5yto
" ;

#delim cr

global margvars "age i.kids  i.disability  ib(4).marstat i.rural i.yrstart earnpre1 fnworkev emppre1 nopiort4 totincpre1"
	
global outvars = "earnpst1 earnpst2 earnpst3 earnpst4"
}



if c(username) == "mjone91" | c(username) == "kef289"   {

global treatvar training
global covars "i.age_12 i.marstat  c.tenure"

global margvars "tenure"

global outvars = "earnpst1 earnpst2 earnpst3 earnpst4"

}

global ycov $covars

global trimup .9
global trimdown .1


//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//
//
//	RUN PROPENSITY SCORES AND TRIM VARS OR LOAD THEM IN 
//  
//
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logit $treatvar  ${covars}  i.female i.Client_Ab

capture drop pscoreout
capture drop trimout
logit $treatvar  ${covars}  i.female i.Client_Ab
parmest ,  saving("$pathdat/temp", replace) idn(1) ids(Full) escal(N)
predict pscoreout if e(sample), p

capture drop pq
xtile pq = pscoreout, nq(1000)
gen trimout = pscoreout > $trimup | pscoreout  < $trimdown | pq == 1 | pq == 1000 if e(sample)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("Pscore_Full_Coef_LF", replace) keepcellfmt firstrow(var)
capture erase  "$pathdat/temp"
restore

margins , dydx(i.female i.Client_Ab $margvars) post
parmest ,  saving("$pathdat/temp", replace) idn(1) ids(Full) escal(N)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("Pscore_Full_Marg_LF", replace) keepcellfmt firstrow(var)
capture erase  "$pathdat/temp"
restore

logit $treatvar pscoreout  ${covars}  i.female i.Client_Ab if trimout == 0
parmest ,  saving("$pathdat/temp", replace) idn(1) ids(Full) escal(N)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("Pscore_Full_Test_LF", replace) keepcellfmt firstrow(var)
capture erase  "$pathdat/temp"
restore


testparm ${covars}  i.female i.Client_Ab
local cntpvalue =r(p)
local cntchi  =r(chi2)
putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("Pscore_Full_Test_LF")  modify
putexcel L1 = "ChiSquare"
putexcel M1 = "pvalue" 
putexcel L2 = `cntchi'
putexcel M2 = `cntpvalue'


//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//	INTERACTED GROUPS
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

local idgroup 2 

foreach j in  1 2 4  {
forval gg = 0(1)1 {



logit $treatvar  ${covars}  if female == `gg'  & Client_Ab == `j'

capture drop N`j'G`gg'pscoreout
capture drop N`j'G`gg'trimout
logit $treatvar  ${covars}   if female == `gg'  & Client_Ab == `j'
parmest ,  saving("$pathdat/temp", replace) idn(`idgroup') ids(N`j'G`gg') escal(N)
predict N`j'G`gg'pscoreout if e(sample), p

capture drop pq
xtile pq = N`j'G`gg'pscoreout, nq(1000)
gen N`j'G`gg'trimout = N`j'G`gg'pscoreout > $trimup | N`j'G`gg'pscoreout  < $trimdown | pq == 1 | pq == 1000 if e(sample)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("Pscore_N`j'G`gg'_Coef_LF", replace) keepcellfmt firstrow(var)
capture erase  "$pathdat/temp"
restore

margins , dydx($margvars) post
parmest ,  saving("$pathdat/temp", replace) idn(`idgroup') ids(N`j'G`gg') escal(N)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("Pscore_N`j'G`gg'_Marg_LF", replace) keepcellfmt firstrow(var)
capture erase  "$pathdat/temp"
restore

logit $treatvar N`j'G`gg'pscoreout  ${covars} if N`j'G`gg'trimout == 0  & female == `gg'  & Client_Ab == `j'
parmest ,  saving("$pathdat/temp", replace) idn(`idgroup') ids(N`j'G`gg') escal(N)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("Pscore_N`j'G`gg'_Test_LF", replace) keepcellfmt firstrow(var)
capture  erase  "$pathdat/temp"
restore


testparm ${covars} 
local cntpvalue =r(p)
local cntchi  =r(chi2)
putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("Pscore_N`j'G`gg'_Test_LF")  modify
putexcel L1 = "ChiSquare"
putexcel M1 = "pvalue" 
putexcel L2 = `cntchi'
putexcel M2 = `cntpvalue'

local idgroup = `idgroup' +1
}
}


preserve 

keep Seqno *trim* *pscore*

save "${pathout}/LF_sample_pscores.dta", replace

restore

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//
//
//	TREATMENT EFFECTS
//  
//
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ra


// FULL SAMPLE 

capture drop temprob 
capture drop temptrim

// the triming has been done with the full set of controls, and here there are sometimes cases where there are high proabilities in the lagged regressions
quietly logit $treatvar ${covars} i.female i.Client_Ab  if   trimout == 0 
predict temprob, p
gen temptrim = temprob > .9999 | temprob < .0001
ta temptrim
capture drop temprob 


global outname Main

local i = 1

foreach vv of  global outvars {

di "Loop  OUTCOME: `vv' OUTCOME NUM: `i' "
capture noisily teffects aipw (`vv'  $ycov  i.female i.Client_Ab ) ($treatvar ${covars}  i.female i.Client_Ab  ) if  trimout == 0  & temptrim == 0,  ate vce(cluster pcprev) iterate(100)

if _rc == 0 & e(converged) == 1 {

parmest ,  saving("$pathdat/temp", replace) idn(1) ids(Full) escal(N n1 n0)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("ATE_Full_`vv'_LF", replace) keepcellfmt firstrow(var)
capture erase  "$pathdat/temp"
restore

if `i' == 1 {
tebalance summarize
mat outbalance = r(table)
putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("BALANCE_Full_LF") modify
putexcel B1= "Standardized Raw Difference"
putexcel C1= "Standardized Weighted Difference"
putexcel D1= "Raw Variance ratio"
putexcel E1= "Weighted Variance ratio"
putexcel A2=matrix(outbalance), rownames
} // only save the balance once
} // converged


 local i = `i'+1
 } // close vv
 
 
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
//	INTERACTED GROUPS
//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

local idgroup 2
foreach j in  1 2 4  {
forval gg = 0(1)1 {
di "Loop j = `j' g =`gg' "
capture drop temprob 
capture drop temptrim

// the triming has been done with the full set of controls, and here there are sometimes cases where there are high probilities in the lagged regressions
quietly logit $treatvar ${covars}  if Client_Ab == `j' & female == `gg' & N`j'G`gg'trimout == 0 
predict temprob, p
gen temptrim = temprob > .9999 | temprob < .0001
ta temptrim
capture drop temprob 

local i = 1

foreach vv of  global outvars {

di "Loop j = `j' g =`gg'  OUTCOME: `vv' OUTCOME NUM: `i' "

capture noisily teffects aipw (`vv'  "$ycov" ) ($treatvar ${covars}  ) if female == `gg' & Client_Ab == `j'   &  N`j'G`gg'trimout == 0  & temptrim == 0,  ate vce(cluster pcprev) iterate(100)
if _rc == 0 & e(converged) == 1 {



parmest ,  saving("$pathdat/temp", replace) idn(`idgroup') ids( N`j'G`gg') escal(N n1 n0)

preserve 
use "$pathdat/temp", clear
export excel * using "${pathlog}\MAIN_AIPW_RESULTS.xlsx",  sheet("ATE_ N`j'G`gg'_`vv'_LF", replace) keepcellfmt firstrow(var)
capture  erase  "$pathdat/temp"
restore
if  `i' == 1 {
tebalance summarize
mat outbalance = r(table)
putexcel set "${pathlog}\MAIN_AIPW_RESULTS.xlsx", sheet("BALANCE_ N`j'G`gg'_LF") modify
putexcel B1= "Standardized Raw Difference"
putexcel C1= "Standardized Weighted Difference"
putexcel D1= "Raw Variance ratio"
putexcel E1= "Weighted Variance ratio"
putexcel A2=matrix(outbalance), rownames
} // close tebalance
}


local i = `i' +1
} // close the vv loop


local idgroup =`idgroup'+1
} // close the g loop
} //close the j loop


log close
