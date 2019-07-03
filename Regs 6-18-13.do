/* Run the Logit and Count Regressions Using Original Data */

log using "/Users/Noel Johnson Notebook/Desktop/Dropbox/Research/Witch Paper Current/JLE r&r/JLE r&r Empirical/Regs 6-24-13.log", replace


/*********************************************************************/
/* Run Regs Using Count and OLS pc on Baseline Data and Malet Periods */


drop if year<1550

sort id period
by id period: egen sumtrials=sum(num_accused)


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


gen trials=0

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


/*********************************************************************/
/* Run Regs Using Count and OLS pc on Maximal Data and Malet Periods */


drop if year<1550

sort id period
by id period: egen sumtrials=sum(num_accused)


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



gen trials=0

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
/* Run Regs Using Count and OLS pc on Baseline Data and Forbonnais Periods */

/* Run the Count Regressions Using Forbonnais Data */


sort id newperiod
by id newperiod: egen sumtrials=sum(num_accused)



gen gtaxpc= guerytax / appxpop1700


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
/* Run Regs Using Count and OLS pc on Maximal Data and Forbonnais Periods */

/* Run the Count Regressions Using Forbonnais Data */


sort id newperiod
by id newperiod: egen sumtrials=sum(num_accused)



gen gtaxpc= guerytax / appxpop1700


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