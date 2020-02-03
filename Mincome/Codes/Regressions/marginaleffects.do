use "W:\WU\Projekte\mincome\Mincome\Data\final datasets\basepay.dta"
destring FSI, replace 
logit birth treated i.FSI i.incbracket
margins, dydx(treated)

