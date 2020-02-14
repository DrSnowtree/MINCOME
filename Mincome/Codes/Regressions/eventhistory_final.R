
setwd("W:/WU/Projekte/mincome/Mincome/Data")
library("lme4")
library("stargazer")
library("ggstance")
library("jtools")
library("coefplot")

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


#all people we had in the baseline, so 440 women, 35 years for each p)


m1 <- glmer(formula = birth ~ treated*experiment + factor(age) + (1|OID) + factor(j) + factor(year), 
            data = datapp)

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

##now only with women born after 1934 and never been divorced before 


m3 <- glmer(formula = birth ~ treated*experiment + factor(age) 
            + (1|OID) + factor(year) + factor(j) + married, 
            data = datapp2)
stargazer(m3)

          
m4 <- lmer(formula = birth ~ plan_1 + plan_2 
            + plan_3 + plan_4 + plan_5
            + plan_7 + plan_8 + experiment + p1exp
            + p2exp + p3exp + p4exp + p5exp + p7exp + p8exp + factor(age) 
            + (1|OID) + factor(year) + factor(j) + married, 
            data = datapp2)
stargazer(m4)

a <- plot_coefs (m2, m4, model.names = c("All", "Restricted sample"), 
                coefs = c("Plan 1" = "p1exp","Plan 2" = "p2exp", "Plan 3" =
                            "p3exp",  
                          "Plan 4" ="p4exp", 
                          "Plan 5" ="p5exp", 
                          "Plan 7" ="p7exp", 
                          "Plan 8" ="p8exp"), 
                level = 0.05, colors = "Qual3")

a

stargazer(m2, m4)





