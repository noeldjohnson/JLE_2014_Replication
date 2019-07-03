/* Run the Logit and Count Regressions Using Original Data */use "/Users/Noel Johnson Notebook/Desktop/Dropbox/Research/Witch Paper Current/Witch Paper Work 6-14-12/Witches Empirical/Witchcraft_Master_Panel_6-14-2012.dta"

log using "/Users/Noel Johnson Notebook/Desktop/Dropbox/Research/Witch Paper Current/JLE r&r/JLE r&r Empirical/Regs 6-24-13.log", replace


/*********************************************************************/
/* Run Regs Using Count and OLS pc on Baseline Data and Malet Periods *//*********************************************************************/
use "/Users/Noel Johnson Notebook/Desktop/Dropbox/Research/Witch Paper Current/JLE r&r/JLE r&r Empirical/Witchcraft_Master_Panel_6-14-2012.dta"

drop if year<1550

sort id period
by id period: egen sumtrials=sum(num_accused)
collapse sumtrials num_accused wcyp silvy appxarea appxpop1700 dept_direct dept_charges creationyear paysdetat distparis citypoplarge citypopsmall avg_elevat avg_rugg10 avg_bishop avg_archbi avg_soilqu avg_commun avg_parl_a avgnrnearcities avgnrnearcities_1, by(id period)count
gen taxpc= dept_direct/ appxpop1700gen rwtaxpc= taxpc/ wcyp gen rstaxpc= taxpc* silvy gen lrwtaxpc=log(1+ rwtaxpc)gen lrstaxpc=log(1+ rstaxpc)gen laccused=log(1+sumtrials)
sort id period

xtset id period


replace lrstaxpc = .01 in 58
replace lrstaxpc = .01 in 59
replace lrstaxpc = .01 in 61
replace lrstaxpc = .01 in 62


/*
drop if id==26
drop if id==29
*/


gen trials=0replace trials=1 if num_accused>0

gen laccusedpc=(laccused/appxpop1700)*10000

xtsum sumtrials laccusedpc lrstaxpc period citypoplarge citypopsmall avg_elevat avg_rugg10 avg_bishop avg_archbi avg_soilqu avg_commun avg_parl_a


/*  Negative Binomial Regressions */

xi: nbreg sumtrials lrstaxpc citypopsmall,
xi: nbreg sumtrials lrstaxpc citypopsmall, irr

xi: nbreg sumtrials lrstaxpc i.period citypopsmall,
xi: nbreg sumtrials lrstaxpc i.period citypopsmall, irr

xi: xtnbreg sumtrials lrstaxpc citypopsmall, fe
xi: xtnbreg sumtrials lrstaxpc citypopsmall, fe irr

xi: xtnbreg sumtrials lrstaxpc i.period citypopsmall, fe
xi: xtnbreg sumtrials lrstaxpc i.period citypopsmall, fe irr

/* OLS Regressions using logged pc trials */

xi: reg laccusedpc lrstaxpc citypopsmall

xi: reg laccusedpc lrstaxpc i.period citypopsmall

xi: reg laccusedpc lrstaxpc i.id citypopsmall

xi: reg laccusedpc lrstaxpc i.period i.id citypopsmall

/* Drop Paris */

xi: xtnbreg sumtrials lrstaxpc citypopsmall if id~=11, fe
xi: xtnbreg sumtrials lrstaxpc citypopsmall if id~=11, fe irr

xi: xtnbreg sumtrials lrstaxpc i.period citypopsmall if id~=11, fe
xi: xtnbreg sumtrials lrstaxpc i.period citypopsmall if id~=11, fe irr

/* Drop Metz-Alsace */

xi: xtnbreg sumtrials lrstaxpc citypopsmall if id~=29, fe
xi: xtnbreg sumtrials lrstaxpc citypopsmall if id~=29, fe irr

xi: xtnbreg sumtrials lrstaxpc i.period citypopsmall if id~=29, fe
xi: xtnbreg sumtrials lrstaxpc i.period citypopsmall if id~=29, fe irr

/* Control for Nearby Cities */

xi: xtnbreg sumtrials lrstaxpc citypopsmall avgnrnearcities, fe
xi: xtnbreg sumtrials lrstaxpc citypopsmall avgnrnearcities, fe irr

xi: xtnbreg sumtrials lrstaxpc i.period citypopsmall avgnrnearcities, fe
xi: xtnbreg sumtrials lrstaxpc i.period citypopsmall avgnrnearcities, fe irr

/* IV Regs using controls */

sort id
by id: egen fullsumtrials = sum(sumtrials)
by id: egen meanlrstaxpc = mean(lrstaxpc)
by id: egen meancitypopsmall = mean(citypopsmall)
by id: egen meanavgnrnearcities = mean(avgnrnearcities)

sum fullsumtrials meanlrstaxpc meancitypopsmall meanavgnrnearcities

sort paysdetat
by paysdetat: sum meanlrstaxpc meancitypopsmall

sort paysdetat
by paysdetat: sum meanlrstaxpc meancitypopsmall if id~=11

gen temp=distparis/100
drop distparis
rename temp distparis

replace paysdetat=.5 if id==2

ivreg2 fullsumtrials (meanlrstaxpc=paysdetat) meancitypopsmall meanavgnrnearcities  avg_elevat avg_rugg10 avg_bishop avg_archbi avg_soilqu avg_commun avg_parl_a if period==0, first

clear
/*********************************************************************/
/* Run Regs Using Count and OLS pc on Maximal Data and Malet Periods *//*********************************************************************/
use "/Users/Noel Johnson Notebook/Desktop/Dropbox/Research/Witch Paper Current/JLE r&r/JLE r&r Empirical/Witchcraft_Master_Panel_6-14-2012.dta"

drop if year<1550

sort id period
by id period: egen sumtrials=sum(num_accused)
collapse sumtrials num_accused wcyp silvy appxarea appxpop1700 dept_direct dept_charges creationyear paysdetat distparis citypoplarge citypopsmall avg_elevat avg_rugg10 avg_bishop avg_archbi avg_soilqu avg_commun avg_parl_a avgnrnearcities avgnrnearcities_1, by(id period)count
gen taxpc= dept_direct/ appxpop1700gen rwtaxpc= taxpc/ wcyp gen rstaxpc= taxpc* silvy gen lrwtaxpc=log(1+ rwtaxpc)gen lrstaxpc=log(1+ rstaxpc)gen laccused=log(1+sumtrials)
sort id period

xtset id period


replace lrstaxpc = .01 in 58
replace lrstaxpc = .01 in 59
replace lrstaxpc = .01 in 61
replace lrstaxpc = .01 in 62


/*
drop if id==26
drop if id==29
*/

replace sumtrials=112 if id==5 & period==0
replace sumtrials=55 if id==5 & period==1
replace sumtrials=0 if id==5 & period==2

replace sumtrials=50 if id==26 & period==0
replace sumtrials=98 if id==26 & period==1
replace sumtrials=55 if id==26 & period==2

replace sumtrials=124 if id==4 & period==0
replace sumtrials=69 if id==4 & period==1
replace sumtrials=9 if id==4 & period==2



gen trials=0replace trials=1 if num_accused>0

gen laccusedpc=(laccused/appxpop1700)*10000

xtsum sumtrials laccusedpc lrstaxpc citypoplarge citypopsmall avg_elevat avg_rugg10 avg_bishop avg_archbi avg_soilqu avg_commun avg_parl_a


/*  Negative Binomial Regressions */

xi: nbreg sumtrials lrstaxpc citypopsmall,
xi: nbreg sumtrials lrstaxpc citypopsmall, irr

xi: nbreg sumtrials lrstaxpc i.period citypopsmall,
xi: nbreg sumtrials lrstaxpc i.period citypopsmall, irr

xi: xtnbreg sumtrials lrstaxpc citypopsmall, fe
xi: xtnbreg sumtrials lrstaxpc citypopsmall, fe irr

xi: xtnbreg sumtrials lrstaxpc i.period citypopsmall, fe
xi: xtnbreg sumtrials lrstaxpc i.period citypopsmall, fe irr

/* OLS Regressions using logged pc trials */

xi: reg laccusedpc lrstaxpc citypopsmall

xi: reg laccusedpc lrstaxpc i.period citypopsmall

xi: reg laccusedpc lrstaxpc i.id citypopsmall

xi: reg laccusedpc lrstaxpc i.period i.id citypopsmall





/* IV Regs using controls */

sort id
by id: egen fullsumtrials = sum(sumtrials)
by id: egen meanlrstaxpc = mean(lrstaxpc)

sum fullsumtrials meanlrstaxpc

gen temp=distparis/100
drop distparis
rename temp distparis

replace paysdetat=.5 if id==2

ivreg2 fullsumtrials (meanlrstaxpc=paysdetat) citypopsmall avg_elevat avg_rugg10 avg_bishop avg_archbi avg_soilqu avg_commun avg_parl_a if period==0, first


clear


/*********************************************************************/
/* Run Regs Using Count and OLS pc on Baseline Data and Forbonnais Periods *//*********************************************************************/

/* Run the Count Regressions Using Forbonnais Data */
use "/Users/Noel Johnson Notebook/Desktop/Dropbox/Research/Witch Paper Current/JLE r&r/JLE r&r Empirical/Witchcraft_Master_Panel_6-14-2012.dta"drop if year<1550

sort id newperiod
by id newperiod: egen sumtrials=sum(num_accused)
collapse sumtrials num_accused wcyp silvy appxarea appxpop1700 guerytax creationyear paysdetat distparis citypoplarge citypopsmall avg_elevat avg_rugg10 avg_bishop avg_archbi avg_soilqu avg_commun avg_parl_a avgnrnearcities avgnrnearcities_1, by(id newperiod)count


gen gtaxpc= guerytax / appxpop1700gen rwgtaxpc= gtaxpc/ wcyp gen rsgtaxpc= gtaxpc* silvy gen lrwgtaxpc=log(1+ rwgtaxpc)gen lrsgtaxpc=log(1+ rsgtaxpc)sort id newperiod


xtset id newperiod


replace lrsgtaxpc = .01 in 58
replace lrsgtaxpc = .01 in 59
replace lrsgtaxpc = .01 in 61
replace lrsgtaxpc = .01 in 62


/*
drop if id==26
drop if id==29
*/


gen laccused=log(1+sumtrials)
gen laccusedpc=(laccused/appxpop1700)*10000


xtsum sumtrials laccusedpc lrsgtaxpc newperiod citypoplarge citypopsmall avg_elevat avg_rugg10 avg_bishop avg_archbi avg_soilqu avg_commun avg_parl_a


/*  Negative Binomial Regressions */

xi: nbreg sumtrials lrsgtaxpc citypopsmall,
xi: nbreg sumtrials lrsgtaxpc citypopsmall, irr

xi: nbreg sumtrials lrsgtaxpc i.newperiod citypopsmall,
xi: nbreg sumtrials lrsgtaxpc i.newperiod citypopsmall, irr

xi: xtnbreg sumtrials lrsgtaxpc citypopsmall, fe
xi: xtnbreg sumtrials lrsgtaxpc citypopsmall, fe irr

xi: xtnbreg sumtrials lrsgtaxpc i.newperiod citypopsmall, fe
xi: xtnbreg sumtrials lrsgtaxpc i.newperiod citypopsmall, fe irr

/* OLS Regressions using logged pc trials */

xi: reg laccusedpc lrsgtaxpc citypopsmall

xi: reg laccusedpc lrsgtaxpc i.newperiod citypopsmall

xi: reg laccusedpc lrsgtaxpc i.id citypopsmall

xi: reg laccusedpc lrsgtaxpc i.newperiod i.id citypopsmall


/* Robustness */

xi: xtnbreg sumtrials lrsgtaxpc i.newperiod citypopsmall if id~=29, fe

/* IV Regs using controls */

sort id
by id: egen fullsumtrials = sum(sumtrials)
by id: egen meanlrstaxpc = mean(lrsgtaxpc)
by id: egen meancitypopsmall = mean(citypopsmall)
by id: egen meanavgnrnearcities = mean(avgnrnearcities)

sum fullsumtrials meanlrstaxpc 

gen temp=distparis/100
drop distparis
rename temp distparis

replace paysdetat=.5 if id==2

ivreg2 fullsumtrials (meanlrstaxpc=paysdetat) meancitypopsmall meanavgnrnearcities avg_elevat avg_rugg10 avg_bishop avg_archbi avg_soilqu avg_commun avg_parl_a if newperiod==1, first



clear

/*********************************************************************/
/* Run Regs Using Count and OLS pc on Maximal Data and Forbonnais Periods *//*********************************************************************/

/* Run the Count Regressions Using Forbonnais Data */
use "/Users/Noel Johnson Notebook/Desktop/Dropbox/Research/Witch Paper Current/JLE r&r/JLE r&r Empirical/Witchcraft_Master_Panel_6-14-2012.dta"drop if year<1550

sort id newperiod
by id newperiod: egen sumtrials=sum(num_accused)
collapse sumtrials num_accused wcyp silvy appxarea appxpop1700 guerytax creationyear paysdetat distparis citypoplarge citypopsmall avg_elevat avg_rugg10 avg_bishop avg_archbi avg_soilqu avg_commun avg_parl_a avgnrnearcities avgnrnearcities_1 count avgcitypo avgcity1 avgsea avgriver avghub_3r avgrom_ro avgcarava avgcara_1 avgelevat avgrugg10 avgbishop avgarchbi avgcapita avguniver avgplunde avgsoilqu avgcommun avgfree_p avgtotal_ avgdmedin avgdmecca avgdrome avgdjerus avgdbyzan avgfup avgmusfup avgchrfup avgmusf_1 avgchrf_1 avgdistan avgdist_1 avgparl_a avgd_larg avggranad avgottoma, by(id newperiod)count


gen gtaxpc= guerytax / appxpop1700gen rwgtaxpc= gtaxpc/ wcyp gen rsgtaxpc= gtaxpc* silvy gen lrwgtaxpc=log(1+ rwgtaxpc)gen lrsgtaxpc=log(1+ rsgtaxpc)sort id newperiod


xtset id newperiod


replace lrsgtaxpc = .01 in 58
replace lrsgtaxpc = .01 in 59
replace lrsgtaxpc = .01 in 61
replace lrsgtaxpc = .01 in 62


/*
drop if id==26
drop if id==29
*/


replace sumtrials=112 if id==5 & newperiod==0
replace sumtrials=55 if id==5 & newperiod==1
replace sumtrials=0 if id==5 & newperiod==2

replace sumtrials=50 if id==26 & newperiod==0
replace sumtrials=98 if id==26 & newperiod==1
replace sumtrials=55 if id==26 & newperiod==2

replace sumtrials=124 if id==4 & newperiod==0
replace sumtrials=69 if id==4 & newperiod==1
replace sumtrials=9 if id==4 & newperiod==2

gen laccused=log(1+sumtrials)
gen laccusedpc=(laccused/appxpop1700)*10000


xtsum sumtrials laccusedpc lrsgtaxpc citypoplarge citypopsmall avg_elevat avg_rugg10 avg_bishop avg_archbi avg_soilqu avg_commun avg_parl_a


/*  Negative Binomial Regressions */

xi: nbreg sumtrials lrsgtaxpc citypopsmall,
xi: nbreg sumtrials lrsgtaxpc citypopsmall, irr

xi: nbreg sumtrials lrsgtaxpc i.newperiod citypopsmall,
xi: nbreg sumtrials lrsgtaxpc i.newperiod citypopsmall, irr

xi: xtnbreg sumtrials lrsgtaxpc citypopsmall, fe
xi: xtnbreg sumtrials lrsgtaxpc citypopsmall, fe irr

xi: xtnbreg sumtrials lrsgtaxpc i.newperiod citypopsmall, fe
xi: xtnbreg sumtrials lrsgtaxpc i.newperiod citypopsmall, fe irr

/* OLS Regressions using logged pc trials */

xi: reg laccusedpc lrsgtaxpc citypopsmall

xi: reg laccusedpc lrsgtaxpc i.newperiod citypopsmall

xi: reg laccusedpc lrsgtaxpc i.id citypopsmall

xi: reg laccusedpc lrsgtaxpc i.newperiod i.id citypopsmall



/* IV Regs using controls */

sort id
by id: egen fullsumtrials = sum(sumtrials)
by id: egen meanlrstaxpc = mean(lrsgtaxpc)

sum fullsumtrials meanlrstaxpc

gen temp=distparis/100
drop distparis
rename temp distparis

replace paysdetat=.5 if id==2

ivreg2 fullsumtrials (meanlrstaxpc=paysdetat) citypopsmall avg_elevat avg_rugg10 avg_bishop avg_archbi avg_soilqu avg_commun avg_parl_a if newperiod==1, first





log close
