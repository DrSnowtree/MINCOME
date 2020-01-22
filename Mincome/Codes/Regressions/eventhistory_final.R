setwd("W:/WU/Projekte/mincome/Mincome/Data")
library("lme4")

#all people we had in the baseline, so 440 women, 35 years for each 

m1 <- glmer(formula = birth ~ treated*experiment + factor(age) + (1|OID) + factor(j) + factor(year), 
             data = datapp)
stargazer(m1)
#not significant 
m2 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
            + plan_3*experiment + plan_4*experiment + plan_5*experiment 
            + plan_7*experiment + plan_8*experiment + factor(age) + (1|OID) + factor(j) + factor(year), 
            data = datapp)
stargazer(m2)

#plans 5 and 7 significant 

#adding marital status, so we have to erase those that have been divorced, this is datapp3 

m3 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
             + plan_3*experiment + plan_4*experiment + plan_5*experiment 
             + plan_7*experiment + plan_8*experiment + factor(age) + (1|OID) + factor(j) + factor(year)
             + married, 
             data = datapp3)
stargazer(m3)

#plan 7 significant 



##now only with women born after 1934, as before, the childbirth history might suffer from 
#left trunctuation

m4 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
             + plan_3*experiment + plan_4*experiment + plan_5*experiment 
             + plan_7*experiment + plan_8*experiment + factor(age) + (1|OID) + factor(year) + factor(j), 
             data = datapp1)
stargazer(m4)

m5 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
            + plan_3*experiment + plan_4*experiment + plan_5*experiment 
            + plan_7*experiment + plan_8*experiment + age1519 + age2024 + age2429 
            + age3034  + (1|OID) + factor(year), 
            data = datapp1)
stargazer(m5)

#Plans 3 and 7 positive and significant (without the parity dummy, plan 1 also significant, but not 3 and 7)


#with married dummy, removing divorced women 

m6 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
            + plan_3*experiment + plan_4*experiment + plan_5*experiment 
            + plan_7*experiment + plan_8*experiment + age1519 + age2024 + age2429 
            + age3034  + (1|OID) + factor(year) + factor(j) + married, 
            data = datapp2)
stargazer(m6)

m6 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
            + plan_3*experiment + plan_4*experiment + plan_5*experiment 
            + plan_7*experiment + plan_8*experiment + age1519 + age2024 + age2429 
            + age3034  + (1|OID) + factor(year) + factor(j) + married, 
            data = datapp2)
stargazer(m6)
#3 and 7 are significant 



stargazer(m1, m2, m3, m4, m5, m6)

