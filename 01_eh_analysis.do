********************************************************************************
* Tunisia Revolution Event History Analysis
********************************************************************************

use "data/analysis/tunisia_29.dta", clear

encode delegation, gen(del_id)
xtset del_id date
gen day=time-18612
gen lnpop= ln(pop_10plus_2014)

gen lsplag=l.spprot

by del_id (date), sort: gen byte cumfirstsum= sum(cumfirst)
drop if cumfirstsum>1 //drop for event history analysis, keep for survival
*replace cumfirstsum=1 if cumfirstsum>1 

gen sqrt_deaths=sqrt(nat_deaths)
gen lsqrt_deaths=l.sqrt_deaths

save data/analysis/tunehdata.dta, replace

*****Main reported models*****

logit del_event c.lsplag c.idr c.pct_2029_2014 lnpop c.lsqrt_deaths, or vce(cluster del_id)
qui fitstat
local AIC=r(aic)
local BIC=r(bic)
local MCF=r(r2_mf)
outreg2 using data/output/tables/eh1.doc, replace ctitle(Model 1: Without time interaction) drop(del_event) addstat(AIC, `AIC', BIC, `BIC', McFadden's R2, `MCF' ) alpha(.001, .01, .05) dec(3) tex
// Model with time interaction of IDR
logit del_event c.lsplag c.day c.idr c.idr#c.day c.pct_2029_2014 lnpop c.lsqrt_deaths,  vce(cluster del_id)
qui fitstat
local AIC=r(aic)
local BIC=r(bic)
local MCF=r(r2_mf)
outreg2 using data/output/tables/eh1.doc, append ctitle(Model 2: With time interaction) drop(del_event) addstat(AIC, `AIC', BIC, `BIC', McFadden's R2, `MCF' ) alpha(.001, .01, .05) dec(3) tex

// Adjusted predictions by quartile band
egen idrcat = cut(idr), group(4) label

logit del_event c.lsplag i.idrcat##c.day c.pct_2029_2014 lnpop lsqrt_deaths, or vce(cluster del_id)
margins idrcat if idrcat==0|idrcat==3, at (day=(0(1)29))
/* Specify black and grey symbols for each outcome for publication purposes */
local symbol1 mcolor(black) msymbol(circle)
local symbol2 mcolor(gs8) msymbol(square)

marginsplot, noci xlab(, nogrid) ylab(, nogrid) scheme(gg_tableau) ///
 title("IDR", size(small)) xtitle("Day" , size(small)) ytitle("Hazard of protest", size(small)) ///
 graphregion(margin(zero)) plotregion(fcolor(white)) aspectratio(1) ///
 legend(order(1 "1st quartile" 2 "4th quartile") pos(6) cols(4) size(small)) ///
 plot1opts(lcolor("black") `symbol1') plot2opts(lcolor(gs3) `symbol2')
 
graph export data/plots/rpqm1.png, replace
 
// Looking at marginal effects by stages
gen stage=1
replace stage =2 if day>16&day<=24
replace stage =3 if day>=25

logit del_event c.lsplag c.idr##i.stage c.pct_2029_2014 lnpop lsqrt_deaths, or vce(cluster del_id)
margins, dydx(stage) at(idr =(0(.05)1))

local symbol1 mcolor(black) msymbol(circle)
local symbol2 mcolor(gs8) msymbol(square)

marginsplot, xlab(, nogrid) ylab(, nogrid) recastci(rarea) ///
 xlab(0(.3).9, nogrid) ylab(, nogrid) title("") xtitle("IDR", size(small)) ytitle("Effects on hazard of protest", size(small)) ///
 scheme(gg_tableau) yline(0) aspectratio(1) graphregion(margin(zero)) plotregion(fcolor(white)) ///
 legend(order(1 "Stage 2 vs. stage 1" 2 "Stage 3 vs. stage 1") pos(6) cols(4) size(small)) ///
 plot1opts(`symbol1' lcolor("black")) ci1opt(color("black")) ///
 plot2opts(`symbol2' lcolor(gs3)) ci2opt(color(gs3))
 
gr_edit .plotregion1.plot2.style.editstyle area(shadestyle(intensity(inten20))) editcopy
gr_edit .plotregion1.plot1.style.editstyle area(shadestyle(intensity(inten20))) editcopy

graph export data/plots/rpsm1.png, replace
 
 
//Supplementary appendix models looking at other other ecological covariates of interest

*drop ecint
clonevar ecint=pct_illit_2014

logit del_event c.lsplag c.ecint c.pct_2029_2014 lnpop c.lsqrt_deaths, or vce(cluster del_id)
qui fitstat
local AIC=r(aic)
local BIC=r(bic)
local MCF=r(r2_mf)
outreg2 using data/output/tables/eh2.doc, replace ctitle(Model 1: Illit. rate without time interaction) drop(del_event) addstat(AIC, `AIC', BIC, `BIC', McFadden's R2, `MCF' ) alpha(.001, .01, .05) dec(3) tex

logit del_event c.lsplag c.day c.ecint c.ecint#c.day c.pct_2029_2014 lnpop c.lsqrt_deaths, or vce(cluster del_id)
qui fitstat
local AIC=r(aic)
local BIC=r(bic)
local MCF=r(r2_mf)
outreg2 using data/output/tables/eh2.doc, append ctitle(Model 2: Illit. rate with time interaction) drop(del_event) addstat(AIC, `AIC', BIC, `BIC', McFadden's R2, `MCF' ) alpha(.001, .01, .05) dec(3) tex

drop ecint
clonevar ecint=dip_unemp_rate_2014

logit del_event c.lsplag c.ecint c.pct_2029_2014 lnpop c.lsqrt_deaths, or vce(cluster del_id)
qui fitstat
local AIC=r(aic)
local BIC=r(bic)
local MCF=r(r2_mf)
outreg2 using data/output/tables/eh2.doc, append ctitle(Model 3: Grad. unemp. rate without time interaction) drop(del_event) addstat(AIC, `AIC', BIC, `BIC', McFadden's R2, `MCF' ) alpha(.001, .01, .05) dec(3) tex

logit del_event c.lsplag c.day c.ecint c.ecint#c.day c.pct_2029_2014 lnpop lsqrt_deaths, or vce(cluster del_id)
qui fitstat
local AIC=r(aic)
local BIC=r(bic)
local MCF=r(r2_mf)
outreg2 using data/output/tables/eh2.doc, append ctitle(Model 4: Grad. unemp. rate with time interaction) drop(del_event) addstat(AIC, `AIC', BIC, `BIC', McFadden's R2, `MCF' ) alpha(.001, .01, .05) dec(3) tex

drop ecint
clonevar ecint=internet_use_2014

logit del_event c.lsplag c.ecint c.pct_2029_2014 lnpop c.lsqrt_deaths, or vce(cluster del_id)
qui fitstat
local AIC=r(aic)
local BIC=r(bic)
local MCF=r(r2_mf)
outreg2 using data/output/tables/eh2.doc, append ctitle(Model 5: Internet use without time interaction) drop(del_event) addstat(AIC, `AIC', BIC, `BIC', McFadden's R2, `MCF' ) alpha(.001, .01, .05) dec(3) tex

logit del_event c.lsplag c.day c.ecint c.ecint#c.day c.pct_2029_2014 lnpop lsqrt_deaths, or vce(cluster del_id)
qui fitstat
local AIC=r(aic)
local BIC=r(bic)
local MCF=r(r2_mf)
outreg2 using data/output/tables/eh2.doc, append ctitle(Model 6: Internet use with time interaction) drop(del_event) addstat(AIC, `AIC', BIC, `BIC', McFadden's R2, `MCF' ) alpha(.001, .01, .05) dec(3) tex 

//Correlation matrix of other ecological variables
corrtex idr pct_illit_2014 dip_unemp_rate_2014 internet_use_2014, file(data/output/tables/corrtex) replace

//Robustness with random intercepts
xtlogit del_event c.lsplag c.idr##c.day c.pct_2029_2014 lnpop lsqrt_deaths, or 

// Robustness excluding Tunis
encode(governorate), gen(gov_id)
logit del_event c.lsplag c.idr##c.day c.pct_2029_2014 lnpop lsqrt_deaths if gov_id!=23, or vce(cluster del_id)

// Robustness with time interactions of other vars.
logit del_event c.lsplag c.lsplag#c.day c.day c.idr c.idr#c.day c.pct_2029_2014 c.lnpop c.lnpop#c.day c.lsqrt_deaths c.lsqrt_deaths#c.day if gov_id!=23, or vce(cluster del_id)

// Adjusted predictions by quartile band for Appendix analyses
egen illitcat = cut(pct_illit_2014), group(4) label
egen dunempcat = cut(dip_unemp_rate_2014), group(4) label
egen intcat = cut(internet_use_2014), group(4) label

logit del_event c.lsplag i.illitcat##c.day c.pct_2029_2014 lnpop lsqrt_deaths, or vce(cluster del_id)
margins illitcat if illitcat==0|illitcat==3, at (day=(0(1)29))
marginsplot, noci xlab(, nogrid) ylab(, nogrid) scheme(gg_tableau) ///
 title("Illit. rate", size(small)) xtitle("Day" , size(small)) ytitle("Hazard of protest", size(small)) ///
 graphregion(margin(zero)) plotregion(fcolor(white)) aspectratio(1) ///
 legend(order(1 "1st quartile" 2 "4th quartile") pos(6) cols(4) size(small)) ///
 plot1opts(mcolor("189 30 36") lcolor("189 30 36")) plot2opts(mcolor(gs3) lcolor(gs3))
 
graph export data/plots/rpqm2.png, replace
 
logit del_event c.lsplag i.dunempcat##c.day c.pct_2029_2014 lnpop lsqrt_deaths, or vce(cluster del_id)
margins dunempcat if dunempcat==0|dunempcat==3, at (day=(0(1)29))
marginsplot, noci xlab(, nogrid) ylab(, nogrid) scheme(gg_tableau) ///
 title("Grad. unemp. rate", size(small)) xtitle("Day" , size(small)) ytitle("Hazard of protest", size(small)) ///
 graphregion(margin(zero)) plotregion(fcolor(white)) aspectratio(1) ///
 legend(order(1 "1st quartile" 2 "4th quartile") pos(6) cols(4) size(small)) ///
 plot1opts(mcolor("189 30 36") lcolor("189 30 36")) plot2opts(mcolor(gs3) lcolor(gs3))
 
graph export data/plots/rpqm3.png, replace

logit del_event c.lsplag i.intcat##c.day c.pct_2029_2014 lnpop lsqrt_deaths, or vce(cluster del_id)
margins intcat if intcat==0|intcat==3, at (day=(0(1)29))
marginsplot, noci xlab(, nogrid) ylab(, nogrid) scheme(gg_tableau) ///
 title("Internet use", size(small)) xtitle("Day" , size(small)) ytitle("Hazard of protest", size(small)) ///
 graphregion(margin(zero)) plotregion(fcolor(white)) aspectratio(1) ///
 legend(order(1 "1st quartile" 2 "4th quartile") pos(6) cols(4) size(small)) ///
 plot1opts(mcolor("189 30 36") lcolor("189 30 36")) plot2opts(mcolor(gs3) lcolor(gs3))
 
graph export data/plots/rpqm4.png, replace
  
// Looking at marginal effects by stages (after Karaca-Mandic et al.)

logit del_event c.lsplag c.pct_illit_2014##i.stage c.pct_2029_2014 lnpop lsqrt_deaths, or vce(cluster del_id)
margins, dydx(stage) at(pct_illit_2014 =(5(5)55))
marginsplot,  xlab(5(10)55, nogrid) ylab(, nogrid) recastci(rarea) ciopts(color(%20)) ///
 xlab(, nogrid) ylab(, nogrid) title("") xtitle("Illit. rate" , size(small)) ytitle("Effects on hazard of protest", size(small)) ///
 scheme(gg_tableau) yline(0) aspectratio(1) graphregion(margin(zero)) plotregion(fcolor(white)) ///
 legend(order(1 "Stage 2 vs. stage 1" 2 "Stage 3 vs. stage 1") pos(6) cols(4) size(small)) ///
 plot1opts(mcolor("189 30 36") lcolor("189 30 36")) ci1opt(color("189 30 36")) ///
 plot2opts(mcolor(gs3) lcolor(gs3)) ci2opt(color(gs3))
 
gr_edit .plotregion1.plot2.style.editstyle area(shadestyle(intensity(inten20))) editcopy
gr_edit .plotregion1.plot1.style.editstyle area(shadestyle(intensity(inten20))) editcopy
 
graph export data/plots/rpsm2.png, replace
 
logit del_event c.lsplag c.dip_unemp_rate_2014##i.stage c.pct_2029_2014 lnpop lsqrt_deaths, or vce(cluster del_id)
margins, dydx(stage) at(dip_unemp_rate_2014 =(5(5)55))
marginsplot,  xlab(5(10)55, nogrid) ylab(, nogrid) recastci(rarea) ciopts(color(%20)) ///
 xlab(, nogrid) ylab(, nogrid) title("") xtitle("Grad. unemp. rate" , size(small)) ytitle("Effects on hazard of protest", size(small)) ///
 scheme(gg_tableau) yline(0) aspectratio(1) graphregion(margin(zero)) plotregion(fcolor(white)) ///
 legend(order(1 "Stage 2 vs. stage 1" 2 "Stage 3 vs. stage 1") pos(6) cols(4) size(small)) ///
 plot1opts(mcolor("189 30 36") lcolor("189 30 36")) ci1opt(color("189 30 36")) ///
 plot2opts(mcolor(gs3) lcolor(gs3)) ci2opt(color(gs3))
 
gr_edit .plotregion1.plot2.style.editstyle area(shadestyle(intensity(inten20))) editcopy
gr_edit .plotregion1.plot1.style.editstyle area(shadestyle(intensity(inten20))) editcopy
 
graph export data/plots/rpsm3.png, replace
 
logit del_event c.lsplag c.internet_use_2014##i.stage c.pct_2029_2014 lnpop lsqrt_deaths, or vce(cluster del_id)
margins, dydx(stage) at(internet_use_2014 =(10(5)75))
marginsplot,  xlab(5(15)75, nogrid) ylab(, nogrid) recastci(rarea) ciopts(color(%20)) ///
 xlab(, nogrid) ylab(, nogrid) title("") xtitle("Grad. unemp. rate" , size(small)) ytitle("Effects on hazard of protest", size(small)) ///
 scheme(gg_tableau) yline(0) aspectratio(1) graphregion(margin(zero)) plotregion(fcolor(white)) ///
 legend(order(1 "Stage 2 vs. stage 1" 2 "Stage 3 vs. stage 1") pos(6) cols(4) size(small)) ///
 plot1opts(mcolor("189 30 36") lcolor("189 30 36")) ci1opt(color("189 30 36")) ///
 plot2opts(mcolor(gs3) lcolor(gs3)) ci2opt(color(gs3))

gr_edit .plotregion1.plot2.style.editstyle area(shadestyle(intensity(inten20))) editcopy
gr_edit .plotregion1.plot1.style.editstyle area(shadestyle(intensity(inten20))) editcopy
 
graph export data/plots/rpsm4.png, replace

// Look at nightlights

*Merge shapefile delegation names
merge m:1 delegation using "data/output/tunisia_shp_delegations1.dta"
// Note no El Krib in nightlights data
drop _merge

preserve
import excel "data/output/tunisia_delegations_nightlights_VIIRS.xlsx", sheet("tunisia_delegations_nightlights") firstrow clear
duplicates drop gov_name_f deleg_na_1 , force
save data/output/nightlights.dta, replace
restore

preserve
import excel "data/output/tunisia_delegations_nightlights_GRCN.xlsx", sheet("delegations_nightlights_GRCN") firstrow clear
duplicates drop gov_name_f deleg_na_1 , force
save data/output/nightlights_grcn.dta, replace
restore

//rename delegation_shp1 deleg_na_1
merge m:1 deleg_na_1 using nightlights
// No El Korba in master EH data; other 6 are water bodies and can be dropped
keep if _merge==3

rename _mean_1 nlights
gen ln_nlights = ln(nlights)
corr idr ln_nlights

twoway scatter ln_nlights idr || lfit ln_nlights idr, ///
scheme(gg_tableau) aspectratio(1) legend(off) ///
ytitle(Mean nightlights (logged)) xtitle(IDR) ylab(, nogrid) 

graph export data/plots/viirs.png, replace


//rename delegation_shp1 deleg_na_1
drop _merge
merge m:1 deleg_na_1 using nightlights_grcn
// No El Korba in master EH data; other 6 are water bodies and can be dropped
keep if _merge==3

rename _mean nlights_gcrn
gen ln_nlights_gcrn = ln(nlights_gcrn)
corr idr ln_nlights_gcrn

twoway scatter ln_nlights_gcrn idr || lfit ln_nlights_gcrn idr, ///
scheme(gg_tableau) aspectratio(1) legend(off) ///
ytitle(Mean nightlights (logged)) xtitle(IDR) ylab(, nogrid) 

graph export data/plots/gcrn.png, replace

logit del_event c.lsplag c.ln_nlights##c.day c.pct_2029_2014 lnpop c.lsqrt_deaths, or vce(cluster del_id)

drop ecint
clonevar ecint=ln_nlights

logit del_event c.lsplag c.ecint c.pct_2029_2014 lnpop c.lsqrt_deaths, or vce(cluster del_id)
qui fitstat
local AIC=r(aic)
local BIC=r(bic)
local MCF=r(r2_mf)
outreg2 using data/output/tables/ehnl.doc, replace ctitle(Model 1: Nightlights without time interaction) drop(del_event) addstat(AIC, `AIC', BIC, `BIC', McFadden's R2, `MCF' ) alpha(.001, .01, .05) dec(3) tex

logit del_event c.lsplag c.day c.ecint c.ecint#c.day c.pct_2029_2014 lnpop c.lsqrt_deaths, or vce(cluster del_id)
qui fitstat
local AIC=r(aic)
local BIC=r(bic)
local MCF=r(r2_mf)
outreg2 using data/output/tables/ehnl.doc, append ctitle(Model 2: Nightlights with time interaction) drop(del_event) addstat(AIC, `AIC', BIC, `BIC', McFadden's R2, `MCF' ) alpha(.001, .01, .05) dec(3) tex


*drop nlightscat
egen nlightscat = cut(ln_nlights), group(4) label

logit del_event c.lsplag i.nlightscat##c.day c.pct_2029_2014 lnpop lsqrt_deaths, or vce(cluster del_id)
margins nlightscat if nlightscat==0|nlightscat==3, at (day=(0(1)29))
marginsplot, noci xlab(, nogrid) ylab(, nogrid) scheme(gg_tableau) ///
 title("Nightlights (logged)", size(small)) xtitle("Day" , size(small)) ytitle("Hazard of protest", size(small)) ///
 graphregion(margin(zero)) plotregion(fcolor(white)) aspectratio(1) ///
 legend(order(1 "1st quartile" 2 "4th quartile") pos(6) cols(4) size(small)) ///
 plot1opts(mcolor("189 30 36") lcolor("189 30 36")) plot2opts(mcolor(gs3) lcolor(gs3))
 
graph export data/plots/rpqmnl.png, replace
 
logit del_event c.lsplag c.ln_nlights##i.stage c.pct_2029_2014 lnpop lsqrt_deaths, or vce(cluster del_id)
margins, dydx(stage) at(ln_nlights =(-3(.3)4.3))
marginsplot,  xlab(-3(2)3, nogrid) ylab(, nogrid) recastci(rarea) ciopts(color(%20)) ///
 xlab(, nogrid) ylab(, nogrid) title("") xtitle("Nightlights (logged)" , size(small)) ytitle("Effects on hazard of protest", size(small)) ///
 scheme(gg_tableau) yline(0) aspectratio(1) graphregion(margin(zero)) plotregion(fcolor(white)) ///
 legend(order(1 "Stage 2 vs. stage 1" 2 "Stage 3 vs. stage 1") pos(6) cols(4) size(small)) ///
 plot1opts(mcolor("189 30 36") lcolor("189 30 36")) ci1opt(color("189 30 36")) ///
 plot2opts(mcolor(gs3) lcolor(gs3)) ci2opt(color(gs3))
 
gr_edit .plotregion1.plot2.style.editstyle area(shadestyle(intensity(inten20))) editcopy
gr_edit .plotregion1.plot1.style.editstyle area(shadestyle(intensity(inten20))) editcopy
 
graph export data/plots/rpsmnl.png, replace
