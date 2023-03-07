//Import the dataset
//Inspect the dataset
browse
describe

//Descriptive Analysis
//Two-way table for all levels of seniority levels with other variables

tabstat annualsalary totalexperience currentexperience bonus, by(seniority_level) statistics(count min max median sd) longstub
ssc install asdoc //To export the tables
asdoc tabstat annualsalary totalexperience currentexperience bonus, by(seniority_level) statistics(count min max median sd) longstub

//Correlation Matrix

pwcorr year annualsalary totalexperience currentexperience bonus, star(0.01)
asdoc pwcorr year annualsalary totalexperience currentexperience bonus, star(0.01)
graph matrix pwcorr year annualsalary totalexperience currentexperience bonus, star(0.01)

//Encode the variables
encode seniority_level, generate(senior_dum)
encode gender, generate(gender_dum)

//Log conversion
generate log_salary = log(annualsalary)
generate log_bonus = log(bonus)

//Significance Tests
//One-way Anova
oneway log_bonus year , tabulate
graph box log_bonus, over(year)

//t-test
ttest log_salary ,by(gender) 
hist log_salary, by(gender)
graph bar (mean) log_salary, over(gender)

//One-way Anova
oneway log_salary seniority_level, tabulate
graph box log_salary, over(seniority_level)


//Exploratory Analysis
//Annual Salary and bonus over Seniority Level
graph hbar (mean) annualsalary (sd)  annualsalary , over(seniority_level) name(g1)
graph hbar (mean) bonus (sd)  bonus , over(seniority_level) name(g2)
graph combine g1 g2

//Annual Salary and bonus over Year
graph hbar (mean) annualsalary (sd)  annualsalary , over(year) name(g3)
graph hbar (mean) bonus (sd)  bonus , over(year) name(g4)
graph combine g3 g4

//Total and current experience over seniority level
graph hbar (mean) totalexperience (sd)  totalexperience (median) totalexperience , over(seniority_level) name(g5)
graph hbar (mean) currentexperience (sd)  currentexperience (median) totalexperience , over(seniority_level) name(g6)
graph combine g5 g6

//Skewness and normal distribution of indepedent variables
histogram annualsalary , normal name(g7)
histogram totalexperience, normal name(g8)
histogram currentexperience , normal name(g9)
histogram bonus , normal name(g10)

//Outlier Detection 
graph hbox annualsalary totalexperience currentexperience bonus year id

//Outlier subcategory Discovery
graph box bonus , over (seniority_level) name(b1)
graph box bonus , over (year) name(b2)
graph box annualsalary, over (seniority_level) name(b3)
graph box annualsalary, over (year) name(b4)
graph combine b1 b2 b3 b4

//Fitted line plots
twoway (scatter annualsalary currentexperience , mcolor(blue) msymbol(triangle))  (lfit annualsalary currentexperience), name(b5)
twoway (scatter annualsalary totalexperience , mcolor(blue) msymbol(triangle))  (lfit annualsalary totalexperience), name(b6)
twoway (scatter annualsalary bonus , mcolor(blue) msymbol(triangle))  (lfit annualsalary bonus), name(b7)
graph combine b5 b6 b7

// Case study for status variable by creation
bysort year id: egen status= sum(bonus)

// Creating status_id & setting values
gen status_id = 1 if status>2000
replace status_id = 0 if status<=2000
graph hbar (mean) annualsalary , over(status_id)

//Regression Analysis
//Baseline model
ssc install outreg2
reg log_salary log_bonus currentexperience totalexperience i.gender_dum i.senior_dum
outreg2 using reg1, excel
margins senior_dum //Effect of seniority level on log_salary
marginsplot
marginsplot, recastci(rarea)
marginsplot, recast(line) noci

reg log_salary i.gender_dum##c.totalexperience currentexperience log_bonus i.senior_dum
margins // Effect of Gender over total experience
margins gender_dum
marginsplot

//Modified Baseline Model
reg log_salary log_bonus i.gender_dum##c.totalexperience c.currentexperience##i.senior_dum
outreg2 using reg2, excel

//Diagnostics and Robustness

//Multicollinearity
pwcorr year annualsalary totalexperience currentexperience bonus, star(0.01)
reg log_salary log_bonus currentexperience totalexperience i.gender_dum i.senior_dum
vif
asdoc vif

//Heteroskedasticity
reg log_salary log_bonus currentexperience totalexperience i.gender_dum i.senior_dum
predict fitted
predict resid, residual
twoway (scatter resid  fitted), yline(0)
drop fitted resid

//detection
hettest
imtest, white
//Remedy
reg log_salary log_bonus totalexperience currentexperience i.gender_dum i.senior_dum, vce(robust)

//Outliers
reg log_salary log_bonus currentexperience totalexperience i.gender_dum i.senior_dum
predict r, rstandard
browse log_salary r if abs(r)>3 & !missing(r)
sort r

//Residual vs fitted plots
rvfplot, mlabel(log_salary)

//Partial Regressors
avplots, mlabel(log_salary)
drop r

//Leverage
predict lev, leverage
gen lev_cutoff=(2*8+2)/1746
browse log_salary lev lev_cutoff if lev>lev_cutoff & !missing(lev)
gsort -lev

// leverage vs squared residual
lvr2plot, mlabel(log_salary)

//Influential obseervations
predict cook, cooksd
gen cook_cutoff= 4/1746
browse id cook cook_cutoff if cook>cook_cutoff & !missing(cook)
gsort -cook


//Normality of error term
reg log_salary log_bonus currentexperience totalexperience i.gender_dum i.senior_dum
predict resid, residual
hist resid

//mid part//
pnorm resid
//tails//
qnorm resid




