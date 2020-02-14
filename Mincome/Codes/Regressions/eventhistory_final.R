setwd("W:/WU/Projekte/mincome/Mincome/Data")
library("lme4")
library("stargazer")
library(sjPlot)
library(margins)
install.packages("devtools")


#all people we had in the baseline, so 440 women, 35 years for each 

datapp$treated_exp = datapp$treated*datapp$experiment
datapp$p7exp = datapp$plan_7*datapp$experiment
datapp$p1exp = datapp$plan_1*datapp$experiment
datapp$p2exp = datapp$plan_2*datapp$experiment
datapp$p3exp = datapp$plan_3*datapp$experiment
datapp$p4exp = datapp$plan_4*datapp$experiment
datapp$p5exp = datapp$plan_5*datapp$experiment
datapp$p8exp = datapp$plan_8*datapp$experiment
datapp$p9exp = datapp$plan_9*datapp$experiment


datapp2$treated_exp = datapp2$treated*datapp2$experiment
datapp2$p7exp = datapp2$plan_7*datapp2$experiment
datapp2$p1exp = datapp2$plan_1*datapp2$experiment
datapp2$p2exp = datapp2$plan_2*datapp2$experiment
datapp2$p3exp = datapp2$plan_3*datapp2$experiment
datapp2$p4exp = datapp2$plan_4*datapp2$experiment
datapp2$p5exp = datapp2$plan_5*datapp2$experiment
datapp2$p8exp = datapp2$plan_8*datapp2$experiment
datapp2$p9exp = datapp2$plan_9*datapp2$experiment


m1 <- lmer(formula = birth ~ treated + experiment + treated_exp + factor(age) + (1|OID) + factor(j) + factor(year), 
             data = datapp)

dydx(m1, data= datapp, variable = "treated")


stargazer(m1)
#not significant 

m2 <- lmer(formula = birth ~ plan_1 + plan_2 
            + plan_3 + plan_4 + plan_5 
            + plan_7 + plan_8 + experiment + p1exp + p2exp + p3exp + p4exp + p5exp + p7exp + p8exp + 
            +  factor(age) + (1|OID) 
             + factor(j),  
            data = datapp)
stargazer(m2)
margins(m2, data = datapp, allow.new.levels=TRUE)

#plans 5 and 7 significant 

#adding marital status, so we have to erase those that have been divorced, this is datapp3 

m3 <- glmer(formula = birth ~ treated*experiment + factor(age) + (1|OID) + factor(j) + factor(year)
            + married, 
            data = datapp3)
stargazer(m3)

m4 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
             + plan_3*experiment + plan_4*experiment + plan_5*experiment 
             + plan_7*experiment + plan_8*experiment + factor(age) + (1|OID) + factor(j) + factor(year)
             + married, 
             data = datapp3)
stargazer(m3)

#plan 7 significant 



##now only with women born after 1934, as before, the childbirth history might suffer from 
#left trunctuation

m5 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
             + plan_3*experiment + plan_4*experiment + plan_5*experiment 
             + plan_7*experiment + plan_8*experiment + 
              factor(age) + (1|OID) + factor(year) + factor(j), 
             data = datapp1)
stargazer(m5)


#Plans 3 and 7 positive and significant (without the parity dummy, plan 1 also significant, but not 3 and 7)


#with married dummy, removing divorced women 

m6 <- lmer(formula = birth ~ plan_1 + plan_2 
           + plan_3 + plan_4 + plan_5 
           + plan_7 + plan_8 + experiment + p1exp + p2exp + p3exp + p4exp + p5exp + p7exp + p8exp + factor(age) 
            + (1|OID) + factor(year) + factor(j) + married, 
            data = datapp2)
stargazer(m6)

margins(m6, data= datapp, allow.new.levels=TRUE)

#3 and 7 are significant 

sjp.glmer(m2, type = "eff")

stargazer(m2, m6)

stargazer(m2, m6, apply.coef=exp, t.auto=F, p.auto=F, report = "vct*")

library(foreign)
write.dta(datapp, "datapp.dta") 

m <- margins(m1, data = datapp)
m



