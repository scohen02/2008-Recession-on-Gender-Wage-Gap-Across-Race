/* Matt Querdasi, Sophie Cohen, Jiyu Shin
Longitudinal Final Project Analysis
Prof. Kaparakis 
QAC 311 */ 


clear 

cd "P:\QAC\qac311\TEAMS\Team1\Final Project" 

// testing import delimited "P:\QAC\qac311\TEAMS\Team1\Final Project\census_with_id.csv"

use "P:\QAC\qac311\TEAMS\Team1\Final Project\censusnew.dta" // import data

tostring cbserial, gen(serial_s)
tostring pernum, gen(pernum_s)
gen unique_id = serial_s +"_" + pernum_s
egen numeric_unique_id = group(unique_id)

xtset numeric_unique_id year

ssc install outreg2


// control for inflation
gen lwage = log(incwage) // logged 

gen adj_wage = incwage/cpi99
gen ladj = log(adj_wage)

codebook empstat
drop if empstat != 1 // only looking at people who were employed during the census year


// generating dummy variables 
codebook sex 
recode sex (1=0) (2=1), gen(gender) // gender, dummy variable


codebook race
recode race (1=0) (2/9=1), gen(racedummy) // race, dummy variable for white/non-white



// create race wage and graph
gen malewage = adj_wage if gender == 0 
gen femalewage = adj_wage if gender == 1

gen whitewage = adj_wage if racedummy == 0 
gen nonwhitewage = adj_wage if racedummy == 1

graph bar whitewage nonwhitewage, over(year)
graph bar (median) whitewage nonwhitewage, over(year)

egen medmale = median(malewage), by(year)
egen medfemale = median(femalewage), by(year)

gen lmalewage = log(malewage)
gen lfemalewage = log(femalewage)

bysort year: gen gap_year = medfemale/medmale

// create gap_year_race 
egen medwhite = median(whitewage), by(year)
egen mednonwhite = median(nonwhitewage), by(year)
bysort year: gen gap_year_race = mednonwhite/medwhite

// graph race
twoway connected gap_year_race year, sort(year)

// create combined gender x race variables and graph
gen whitemalewage = adj_wage if racedummy == 0 & gender == 0
gen whitefemalewage = adj_wage if racedummy == 0 & gender == 1
gen nonwhitemalewage = adj_wage if racedummy == 1 & gender == 0
gen nonwhitefemalewage = adj_wage if racedummy == 1 & gender == 1

graph bar whitemalewage whitefemalewage nonwhitemalewage nonwhitefemalewage, over(year)
graph bar (median) whitemalewage whitefemalewage nonwhitemalewage nonwhitefemalewage, over(year)


tab year sex, sum(adj_wage)

graph bar malewage femalewage, over(year)
graph bar (median) malewage femalewage, over(year)

twoway bar gap_year year

twoway connected gap_year year, sort(year)



// regression: 

xtreg ladj i.gender, re
est store reff
outreg2 using reg1.doc, ctitle(Random Effects)

xtreg ladj i.gender, fe
est store feff
outreg2 using reg1.doc, append(Fixed Effects)

hausman reff feff 
/* As the chi-squared value is large and significant at p = 0.05, we will choose to use the fixed effects model since we reject the null hypothesis and find that the random effects model is not efficient.  */ 


xtreg ladj i.gender i.year, fe
margins, at(year =(2006(1)2014) gender = (0 1))
marginsplot



// wage gap alone across race: 
xtreg ladj i.racedummy, fe 
outreg2 using race.doc, ctitle(Fixed Effects)

xtreg ladj i.racedummy i.year, fe
margins, at(year =(2006(1)2014) race = (0 1))
marginsplot


// gender wage gap across race: 
xtreg ladj i.gender##i.racedummy, fe
outreg2 using reg2.doc, ctitle(Fixed  Effects)
margins gender#racedummy
marginsplot

margins racedummy, dydx(gender)
marginsplot


xtreg ladj i.gender##i.racedummy i.year, fe
margins gender#racedummy, at(year =(2006(1)2014))
marginsplot


// pre, during, and post recession: 
xtreg ladj i.gender##i.racedummy if year < 2008, fe
outreg2 using reg3.doc, ctitle(Pre-recession)

xtreg ladj i.gender##i.racedummy if year == 2008 | year == 2009, fe
outreg2 using reg3.doc, append ctitle(Recession period)

xtreg ladj i.gender##i.racedummy if year > 2009, fe
outreg2 using reg3.doc, append ctitle(Post-recession)



// FIXED OR RANDOM ON GENDER and RACE
// fixed effects 
xtreg ladj gender##racedummy, fe
est store fixed

// random effects
xtreg ladj gender##racedummy, re
est store random

hausman fixed random // As with the previous Hausman test, we find that the p-value is < 0.05 so the chi-squared value is significant, so we will use the fixed effects test. 