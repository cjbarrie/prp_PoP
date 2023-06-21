********************************************************************************
* Tunisia Revolution PRP Survey Analysis
********************************************************************************

***OPEN ARAB BAROMETER DATA (ALSO AVAILABLE AT: http://www.arabbarometer.org/instruments-and-data-files)
** Variable coding script adapted from  https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/IUJPYK
use "data/raw/adbii_merged_data_file_english_final_0.dta", clear
//Keep only Tunisian Respondents
keep if country==21 

//Protest participation measure
recode t902 (1=1) (2=0) (*=.), gen(protest)
label var protest "Participated in Revolutionary Protests (1=yes)"

//Demographics Recoding
gen age=q1001
label var age "Age (in years)"
recode t1003 (1=1) (2=2) (3=3) (4=4) (5=5) (6=6) (*=.), gen(educ)

recode q1002 q1004 q301 (1=1) (2=0) (*=.), gen(male employed voted)
recode q13 (1=0) (2=1) (*=.), gen(rural)
label var rural "Rural (1=yes)"

label var employed "Employed (1=yes)"

label var male "Male (1=yes)"
tab q1005, gen(Dwhynemp)
rename Dwhynemp1 emp_retired
rename Dwhynemp2 emp_housewife
rename Dwhynemp3 emp_student
replace emp_student=emp_student+Dwhynemp5
gen emp_outofforce=emp_retired+emp_housewife
foreach i in outofforce student{
replace emp_`i'=0 if emp_`i'!=1
}
label var emp_outofforce "Retired/Housewife (1=yes)"
label var emp_student "Student (1=yes)"

recode q404 (1=3) (2=2) (3=1) (4 8=0) (*=.), gen(polinterest)
label var polinterest "Interest in Politics (0-3)"
gen VERYpolinterest=polinterest==3
label var VERYpolinterest "Very Interested in Politics (1=yes)"

gen income =q1015
gen income_ln=ln(q1015)
gen income_mis=0
replace income_mis=1 if (q1015>9500000)
label var income_ln "Family Monthly Income (logged; missing=mean)"
label var income_mis "Family Income (missing)"
qui sum income_ln if income_mis==0 & country==21 [aw=wt]
replace income_ln=r(mean) if (q1015>9500000) 

label var educ "Education"
tab educ, gen(Dedu_)
gen EDnone=Dedu_1+Dedu_2
gen EDcoll=Dedu_6+Dedu_5
label var EDnone "Education: Preparatory/basic or less"
label var EDcoll "Education: 4-year College +"

**Relative Deprivation Interactions/Variables
foreach i in income_ln income_mis employed emp_student emp_outofforce{
gen educX`i'=educ*`i'
}
label var educXincome_ln "Income x Education"
label var educXincome_mis "Income (missing) x Education"
label var educXemployed "Employed x Education"
label var educXemp_student "Student x Education"
label var educXemp_outofforce "Retired/Housewife x Education"

****Religiosity
recode q609 (1=3) (2=2) (3=1) (*=.),gen(relig)
recode t6101 t6105 t6106 (1=5) (2=4) (3=3) (4=2) (5=1) (*=.), gen(pray coll_relig quran)
label var coll_relig "Frequency of Mosque Attendance"

alpha quran relig pray, std gen(indiv_relig)
label var indiv_relig "Individual Religiosity (M=0; SD=1)"
label var pray "Frequency of Prayer"
label var quran "Frequency of Reading Quran"
label var relig "Self-described Religiosity"

//GOVERNORATE INDICATORS
tab q1, gen(govT_)
label var govT_19 "Gafsa (1=yes)"
label var govT_12 "Kairouan (1=yes)"
label var govT_13 "Kasserine (1=yes)"
label var govT_18 "Sfax (1=yes)"
label var govT_14 "Sidi Bouzid (1=yes)"
label var govT_15 "Sousse (1=yes)"
//Indicator for governorates surveyed in 2012 Survey
recode q1 (8018 8019 8020 8021 8024 8025=1) (*=0),gen(samp_gov)

**Social forces
recode t905 (1=1) (*=0), gen(friends_part)
label var friends_part "Any friends/acquaintances protest? (1=yes)"

**Civil org?
recode q5012 q5013 q5014 q5016 (1=1) (*=0), gen(civil_char civil_trade civil_youth civil_local)
egen civil_any=rowmax(civil_char civil_trade civil_youth civil_local)
label var civil_any "Member of Civil Society Org.? (1=yes)"

*support for democracy
recode q5162 q5167 (1=0) (2=1) (3=3) (4=4) (*=2), gen(dem_supp2 dem_supp7)
recode q5164 (1=4) (2=3) (3=1) (4=0) (*=2), gen(dem_supp4)
egen hoffman_dem=rmean(dem_supp2 dem_supp4 dem_supp7)
label var hoffman_dem "Commitment to Democracy (mean index, 0-4)"

// (CJB) adding in question 3 to check alpha
recode q5163 (1=0) (2=1) (3=3) (4=4) (*=2), gen(dem_supp3)
alpha dem_supp2 dem_supp4 dem_supp7
alpha dem_supp2 dem_supp7
alpha dem_supp2 dem_supp3 dem_supp4 dem_supp7
alpha dem_supp2 dem_supp3 dem_supp7

egen hoffman_dem1=rmean(dem_supp2 dem_supp3 dem_supp7)
egen hoffman_dem2=rmean(dem_supp2 dem_supp7)
gen hoffman_dem3=dem_supp2 +dem_supp4 +dem_supp7

***SET SAMPLE
gen insample=0
logit protest income_ln income_mis educ employed emp_student age male indiv_relig coll_relig polinterest civil_any rural hoffman_dem govT_* [pw=wt], r
keep if e(sample)

*religiosity index (restandardized within restricted sample)
drop indiv_relig
alpha quran relig pray, std gen(indiv_relig)
qui sum indiv_relig [aw=wt]
replace indiv_relig=(indiv_relig-r(mean))/r(sd)
label var indiv_relig "Religiosity (M=0; SD=1)"

***TABLE 1
tab friends_part [aw=wt]
tab protest friends_part [aw=wt], col
logit protest hoffman_dem income_ln income_mis educ age male employed emp_outofforce emp_student indiv_relig rural polinterest friends_part govT_* [pw=wt], r
logit protest hoffman_dem income_ln income_mis educ age male employed emp_outofforce emp_student indiv_relig coll_relig rural polinterest friends_part civil_any govT_* [pw=wt], r

// Testing varying participation times

//Participated in any protests (Doherty and Schraeder measure)
recode t902 (1=1) (2=0) (*=.), gen(ptest)
label var ptest "Participated in Revolutionary Protests (1=yes)"

//Participation split
recode t9031 (1=1) (2 .=0) (0 8=.), gen(protest1)
label var protest1 "Participated in Revolutionary Protests 1 (1=yes)"

recode t9032 (1=1) (2 .=0) (0 8=.), gen(protest2)
label var protest2 "Participated in Revolutionary Protests 2 (1=yes)"

recode t9033 (1=1) (2 .=0) (0=.), gen(protest3)
label var protest3 "Participated in Revolutionary Protests 3 (1=yes)"

//Asses sequences (no. sequences = 2^n where n is length of possible sequence
egen group = group(protest1 protest2 protest3), label
tab group
latab group

 // Ordinal logistic regression

drop group
egen group = group(protest1 protest2 protest3), label(group, replace)
codebook group
gen optest=0 if group==1
replace optest=1 if  group==5|group==6|group==7|group==8
replace optest=2 if  group==3|group==4
replace optest=3 if group==2

codebook optest

// Makes more sense to have no protest as 4 (as theoretically highest threshold)
// for the ordinal model
recode optest(0=4), into(optest1)

/*Note that weights and 
governorate intercepts have to be removed in order for it to compute Brant 
and others tests--inclusion of governorates mean too many observations dropped
*/
ologit optest1 hoffman_dem income_ln income_mis educ age male employed emp_outofforce ///
 emp_student indiv_relig coll_relig rural polinterest friends_part civil_any, r
 
brant, detail

ologit optest1 hoffman_dem income_ln income_mis educ age male employed emp_outofforce ///
 emp_student indiv_relig coll_relig rural polinterest friends_part civil_any, r
oparallel, ic

//Regression output comparing aggregate to ordinal
//Note here using alternative governorate indicator as there are mistakes in the other govT_* indicator used in Doherty and Schraeder
rename q1 governorate

logit ptest hoffman_dem income_ln income_mis educ age male employed emp_outofforce ///
 emp_student indiv_relig coll_relig rural polinterest friends_part civil_any i.governorate [pw=wt], r
 outreg2 using data/output/tables/ab1.doc, replace ctitle(Model 1: Logistic) drop(ptest) alpha(.001, .01, .05) dec(3) tex
 
ologit optest1 hoffman_dem income_ln income_mis educ age male employed emp_outofforce ///
 emp_student indiv_relig coll_relig rural polinterest friends_part civil_any i.governorate [pw=wt], r
 outreg2 using data/output/tables/ab1.doc, append ctitle(Model 2: Ordinal logistic) drop(ptest) alpha(.001, .01, .05) dec(3) tex

mlogit optest hoffman_dem income_ln income_mis educ age male employed emp_outofforce ///
 emp_student indiv_relig coll_relig rural polinterest friends_part civil_any i.governorate [pw=wt], r 
outreg2 using data/output/tables/ab2.doc, replace ctitle(Model 1: Multinomial regression) alpha(.001, .01, .05) dec(3) tex


// Generate link plot with mlogitplot command with government intercepts removed
mlogit optest hoffman_dem income_ln income_mis educ age male employed emp_outofforce ///
 emp_student indiv_relig coll_relig rural polinterest friends_part civil_any i.governorate [pw=wt], r 
 
lab var hoffman_dem "Dem. commit."
lab var educ "Education"
lab var age "Age"
lab var emp_student "Student"
lab var income_ln "Income"

set scheme gg_tableau

mlogitplot hoffman_dem educ age emp_student, ///
symbols(S0 S1 S2 S3) linepvalue(.05) ///
offsetlist(2 -2 0 2 -2 2 -2 0 2 -2 2 -2 0 2 -2 2) ///
graphregion(margin(zero)) aspectratio(1) varlabels amount(rng)  ///
note("Note: S0 = base outcome; S1 = Stage 1; S2 = Stage 2; S3 = Stage 3;", size(vsmall)) ///
caption("Dem. commit. = Commitment to democracy", size(vsmall))

gr_edit .xaxis1.title.style.editstyle size(small) editcopy
gr_edit .xaxis2.title.style.editstyle size(small) editcopy
// title size
gr_edit .plotregion1.textbox15.style.editstyle color(red) editcopy
// text() 15 color
gr_edit .plotregion1.textbox10.style.editstyle color(red) editcopy
// text() 10 color
gr_edit .plotregion1.textbox12.style.editstyle color(red) editcopy
// text() 12 color
gr_edit .plotregion1.textbox4.style.editstyle color(red) editcopy
// text() 4 color
gr_edit .plotregion1.textbox7.style.editstyle color(red) editcopy
gr_edit .plotregion1.textbox4.style.editstyle size(large) editcopy
// text() 4 edits
gr_edit .plotregion1.textbox7.style.editstyle size(large) editcopy
// text() 7 size
gr_edit .plotregion1.textbox10.style.editstyle size(large) editcopy
// text() 10 size
gr_edit .plotregion1.textbox12.style.editstyle size(large) editcopy
// text() 12 size
gr_edit .plotregion1.textbox15.style.editstyle size(large) editcopy
// text() 15 size
graph export data/plots/mlp.png, replace


//Including income 
mlogitplot hoffman_dem educ age emp_student income_ln, ///
symbols(S0 S1 S2 S3) linepvalue(.05) ///
offsetlist(2 -2 0 2 -2 2 -2 0 2 -2 2 -2 0 2 -2 2 2 -2 0 2 ) ///
graphregion(margin(zero)) aspectratio(1) varlabels amount(rng) ///
caption("Note: S0 = base outcome; S1 = Stage 1; S2 = Stage 2; S3 = Stage 3", size(vsmall))

// Generate predictive margins
mlogit optest hoffman_dem income_ln income_mis educ age male employed emp_outofforce ///
 emp_student indiv_relig coll_relig rural polinterest friends_part civil_any i.governorate [pw=wt], r
 
margins , at(hoffman_dem=(0(.1)4)) predict(outcome(1)) saving(data/output/stage1, replace)
margins , at(hoffman_dem=(0(.1)4)) predict(outcome(2)) saving(data/output/stage2, replace)
margins , at(hoffman_dem=(0(.1)4)) predict(outcome(3)) saving(data/output/stage3, replace)

combomarginsplot data/output/stage1 data/output/stage2 data/output/stage3, aspectratio(1) scheme(gg_tableau) ///
xlab(, nogrid) ylab(, nogrid) /// 
labels("Stage 1" "Stage 2" "Stage 3") noci ///
plot1opts(mcolor("0 0 0") lcolor("0 0 0") msymbol(circle)) ///
plot2opts(mcolor("128 128 128") lcolor("128 128 128") msymbol(square)) ///
plot3opts(mcolor("192 192 192") lcolor("192 192 192") msymbol(triangle)) ///
legend(order(1 "Stage 1" 2 "Stage 2" 3 "Stage 3") pos(6) cols(3) size(small)) ///
title("") ytitle("Pr(Protest participation)") plotregion(fcolor(white))


graph export data/plots/dempred.png, replace

// Robustness with alternatively coded democracy var.
mlogit optest hoffman_dem1 income_ln income_mis educ age male employed emp_outofforce ///
 emp_student indiv_relig coll_relig rural polinterest friends_part civil_any i.governorate [pw=wt], r
 
mlogit optest hoffman_dem2 income_ln income_mis educ age male employed emp_outofforce ///
 emp_student indiv_relig coll_relig rural polinterest friends_part civil_any i.governorate [pw=wt], r
 
mlogit optest hoffman_dem3 income_ln income_mis educ age male employed emp_outofforce ///
 emp_student indiv_relig coll_relig rural polinterest friends_part civil_any i.governorate [pw=wt], r
