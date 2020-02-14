
setwd("W:/WU/Projekte/mincome/Mincome/Data")
library("lme4")
library("stargazer")
library("jtools")
#all people we had in the baseline, so 440 women, 35 years for each 

datapp$plan_7 <- as.numeric(datapp$plan_7)
datapp$plan_1 <- as.numeric(datapp$plan_1)
datapp$plan_2 <- as.numeric(datapp$plan_2)
datapp$plan_3 <- as.numeric(datapp$plan_3)
datapp$plan_4 <- as.numeric(datapp$plan_4)
datapp$plan_5 <- as.numeric(datapp$plan_5)
datapp$plan_8 <- as.numeric(datapp$plan_8)
datapp$plan_9 <- as.numeric(datapp$plan_9)
datapp$treated <- as.numeric(datapp$treated)
datapp$experiment <- as.numeric(datapp$experiment)

datapp$p7exp <- datapp$plan_7*datapp$experiment
datapp$p1exp <- datapp$plan_1*datapp$experiment
datapp$p2exp <- datapp$plan_2*datapp$experiment
datapp$p3exp <- datapp$plan_3*datapp$experiment
datapp$p4exp <- datapp$plan_4*datapp$experiment
datapp$p5exp <- datapp$plan_5*datapp$experiment
datapp$p8exp <- datapp$plan_8*datapp$experiment
datapp$p9exp <- datapp$plan_9*datapp$experiment

datapp$p7exp <- as.factor(datapp$p7exp)
datapp$p1exp <- as.factor(datapp$p1exp)
datapp$p2exp <- as.factor(datapp$p2exp)
datapp$p3exp <- as.factor(datapp$p3exp)
datapp$p4exp <- as.factor(datapp$p4exp)
datapp$p5exp <- as.factor(datapp$p5exp)
datapp$p8exp <- as.factor(datapp$p8exp)
datapp$p9exp <- as.factor(datapp$p9exp)

datapp2$p7exp <- datapp2$plan_7*datapp2$experiment
datapp2$p1exp <- datapp2$plan_1*datapp2$experiment
datapp2$p2exp <- datapp2$plan_2*datapp2$experiment
datapp2$p3exp <- datapp2$plan_3*datapp2$experiment
datapp2$p4exp <- datapp2$plan_4*datapp2$experiment
datapp2$p5exp <- datapp2$plan_5*datapp2$experiment
datapp2$p8exp <- datapp2$plan_8*datapp2$experiment
datapp2$p9exp <- datapp2$plan_9*datapp2$experiment

datapp2$p7exp <- as.factor(datapp2$p7exp)
datapp2$p1exp <- as.factor(datapp2$p1exp)
datapp2$p2exp <- as.factor(datapp2$p2exp)
datapp2$p3exp <- as.factor(datapp2$p3exp)
datapp2$p4exp <- as.factor(datapp2$p4exp)
datapp2$p5exp <- as.factor(datapp2$p5exp)
datapp2$p8exp <- as.factor(datapp2$p8exp)
datapp2$p9exp <- as.factor(datapp2$p9exp)


m1 <- glmer(formula = birth ~ treated*experiment + factor(age) + (1|OID) + factor(j) + factor(year), 
            data = datapp)
stargazer(m1)

#not significant 
m2 <- glmer(formula = birth ~ plan_1+ plan_2 
            + plan_3 + plan_4 + plan_5
            + plan_7 + plan_8 + experiment + p1exp
            + p2exp + p3exp + p4exp + p5exp + p7exp + p8exp + 
            factor(age) + (1|OID) + factor(j) + factor(year), 
            data = datapp)
stargazer(m2)


#plans 5 and 7 significant 

##now only with women born after 1934 and never been divorced before 


#Plans 3 and 7 positive and significant (without the parity dummy, plan 1 also significant, but not 3 and 7)

m3 <- glmer(formula = birth ~ treated*experiment + factor(age) 
            + (1|OID) + factor(year) + factor(j) + married, 
            data = datapp2)
stargazer(m3)

           
m4 <- glmer(formula = birth ~ plan_1 + plan_2 
            + plan_3 + plan_4 + plan_5
            + plan_7 + plan_8 + experiment + p1exp
            + p2exp + p3exp + p4exp + p5exp + p7exp + p8exp + factor(age) 
            + (1|OID) + factor(year) + factor(j) + married, 
            data = datapp2)
stargazer(m4)

#3 and 7 are significant 
m4, m5, m6,


a <- plot_coefs(m2, m4, scale = TRUE, plot.distributions = FALSE, 
                model.names = c("All", "Restricted sample"), 
                coefs = c("Plan 1" = "p1exp","Plan 2" = "p2exp", "Plan 3" =
                            "p3exp",  
                          "Plan 4" ="p4exp", 
                          "Plan 5" ="p5exp", 
                          "Plan 7" ="p7exp", 
                          "Plan 8" ="p8exp"),  
                inner_ci_level = .9, colors = "Qual3")

a

library("ggstance")

stargazer(m2, m6)

stargazer(m2, m6, apply.coef=exp, t.auto=F, p.auto=F, report = "vct*")

library(foreign)
write.dta(datapp, "datapp.dta") 



