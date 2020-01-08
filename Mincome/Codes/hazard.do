stset age, failure(event)
sts graph, na
gen treatexp = experiment*treated
sts test treatexp, logrank
sts graph, by(treatexp) 

destring OID, replace
set matsize 11000
stcox treatexp treated experiment i.OID i.year, nohr
