use "W:\WU\Projekte\mincome\Mincome\Data\final datasets\basepay.dta"
destring FSI, replace 

logit birth treated i.FSI i.incbracket, asis 
margins, dydx(treated)
estimates store m1, title(Model 1)

logit birth plan_1 plan_2 plan_3 plan_4 plan_5 plan_7 plan_8 i.FSI i.incbracket, asis 
margins, dydx(plan_1 plan_2 plan_3 plan_4 plan_5 plan_8) post
estimates store m2, title(Model 2)
logit birth plan_1 plan_2 plan_3 plan_4 plan_5 plan_7 plan_8 i.FSI i.incbracket, asis 
eststo margin27: margins, dydx(plan_7) post
estimates store m27, title(Model 27)

logit birth treated DH i.age i.FSI i.NumChild i.incbracket yrschf costch i. chout i.if_birth9 i.changeDHSH i.changeSHDH i.femhome, asis 
eststo margin3: margins, dydx(treated) post
estimates store m3, title(Model 3)

logit birth plan_1 plan_2 plan_3 plan_4 plan_5 plan_7 plan_8 DH i.age i.FSI i.NumChild i.incbracket yrschf costch i. chout i.if_birth9 i.changeDHSH i.changeSHDH i.femhome, asis 
eststo margin4: margins, dydx(plan_1 plan_2 plan_3 plan_4 plan_5 plan_7  plan_8) post
estimates store m4, title(Model 4)

logit birth treated age1519 age2024 age2429 age3034 age3539 age4044 age4550 i.FSI i.NumChild i.incbracket yrschf costch i. chout i.if_birth9 i.femhome yrschm MAGE, asis 
eststo margin5: margins, dydx(treated) post
estimates store m5, title(Model 5)
logit birth plan_1 plan_2 plan_3 plan_4 plan_5 plan_7 plan_8 age1519 age2024 age2429 age3034 age3539 age4044 age4550 i.FSI i.NumChild i.incbracket yrschf costch i. chout i.if_birth9 yrschm MAGE, asis 
eststo margin6: margins, dydx(plan_1 plan_2 plan_3 plan_4 plan_5 plan_7  plan_8) post
estimates store m6, title(Model 6)

esttab margin1 margin2 margin27 margin3 margin4 margin5 margin6 using reg.tex, label nostar ///
title(Regression table\label{tab1})

keep if plan_9 == 1 | plan_3 == 1
save basepay3.dta
clear 
use "W:\WU\Projekte\mincome\Mincome\Data\final datasets\basepay.dta"

keep if plan_9 == 1 | plan_1 == 1
save basepay1.dta
clear
use "W:\WU\Projekte\mincome\Mincome\Data\final datasets\basepay.dta"

keep if plan_9 == 1 | plan_2 == 1
save basepay2.dta
clear 
use "W:\WU\Projekte\mincome\Mincome\Data\final datasets\basepay.dta"

keep if plan_9 == 1 | plan_4 == 1
save basepay4.dta
clear 
use "W:\WU\Projekte\mincome\Mincome\Data\final datasets\basepay.dta"

keep if plan_9 == 1 | plan_5 == 1
save basepay5.dta
clear 
use "W:\WU\Projekte\mincome\Mincome\Data\final datasets\basepay.dta"

keep if plan_9 == 1 | plan_7 == 1
save basepay7.dta
clear 
use "W:\WU\Projekte\mincome\Mincome\Data\final datasets\basepay.dta"

keep if plan_9 == 1 | plan_8 == 1
save basepay8.dta
clear 

use basepay1.dta
destring plan_1, replace
destring FSI, replace
destring NumChild, replace
logit birth plan_9 i.FSI i.incbracket age1519 age2024 age2429 age3034 age3539 age4044 age4550 i.FSI i.NumChild i.incbracket yrschf costch i. chout i.if_birth9, asis  
margins, dydx(treated)


*event history 
use "W:\WU\Projekte\mincome\Mincome\Data\final datasets\datapp.dta"
xtmelogit treatexp i.age i.j i.year || OID: , intpoints(10)
gen treatexp = treated * experiment

