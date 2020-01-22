setwd("W:/WU/Projekte/mincome/Mincome/Data")
library("compareGroups")
library("data.table")
library("gtools")
library("haven")
library("dplyr")
library("tidyr") 
library("tidyverse")
library("lubridate") 
library("data.table")
library("foreign")
library("haven")
library("quantmod") 
library("zoo")
library("plm")
library("gplots")
library("stargazer")
library("lfe")
library("Hmisc")
library("readxl")
library("naniar")
library("strex")
library(devtools)
library(survival)
library(pltesim)
library(informR)
library(frailtypack)
library("logistf")
library("brglm")
library("lme4")
library("optimx")
library("numDeriv")
library("RCurl")
library("dfoptim")
library("nloptr")
library("fastDummies")

#datapp has all people we have in basepay with each year from age 15 to age 50 


#glmer is generalized linear model with Firth method so that the coefficients do not explode
#and allows for random effects 

#let´s see what happens if we model fertility as a function of age only, with individual random effects 
#and year fixed effects 

m1 <- glmer(formula = birth ~ factor(age) + (1|OID) + factor(year), 
            family = binomial(link ="logit"), data = datapp)

stargazer(m1)
#failure to converge, unable to evaluate scaled gradients
#degenerate Hessian with 13 negative eigenvalues, also nothing significant 

#try with age dummies 

m2 <- glmer(formula = birth ~ age1519 + age2024 + age2429 
            + age3034 + age3539 + age4044 + (1|OID) + factor(year), 
            family = binomial(link ="logit"), data = datapp)
stargazer(m2)
# same problems with convergence, but the age dummies are significant 

#try it with the parity dummy 
m3 <- glmer(formula = birth ~ age1519 + age2024 + age2429 
            + age3034 + age3539 + age4044 + (1|OID) + factor(year)
            + factor(j), 
            family = binomial(link ="logit"), data = datapp)
stargazer(m3)

# same problems with convergence, but the age dummies are significant 
#fixed effect years seem a bit off before 1946, we can remove the 
#units who turn 15 before 1950 

datapp1 <- datapp[which(datapp$birthyear > 1934), ]

#now people will be 40 max, so age4044 no longer relevant, we leave age3539 out as reference dummy 

m4 <- glmer(formula = birth ~ age1519 + age2024 + age2429 
            + age3034 + (1|OID) + factor(year)
            + factor(j), 
            family = binomial(link ="logit"), data = datapp1)
stargazer(m4)
#singular fit, most likely due to the random effect
#no convergence issues when the fit is singular
#singularity becomes a problem when we look at the younger women in the sample 

#try without the year fixed effect

m5 <- glmer(formula = birth ~ age1519 + age2024 + age2429 
            + age3034 + (1|OID) + factor(j), 
            family = binomial(link ="logit"), data = datapp1)
stargazer(m5)

#again singular fit 

#make j 4 after the fourth child, there are very few observations for above 4  
#a new variable m 
datapp1$m <- datapp1$j
datapp1$m[datapp1$m > 4] <- 4


m6 <- glmer(formula = birth ~ age1519 + age2024 + age2429 
            + age3034 + (1|OID) + factor(m), 
            family = binomial(link ="logit"), data = datapp1)
stargazer(m6)

#again the singularity problem, but now everything is significant 
# try with j above 3 3, n instead of m 
datapp1$n <- datapp1$j
datapp1$n[datapp1$n > 3] <- 3
m7 <- glmer(formula = birth ~ age1519 + age2024 + age2429 
            + age3034 + (1|OID) + factor(n), 
            family = binomial(link ="logit"), data = datapp1)
stargazer(m7)
#similar results, model still singular

#try it without random effects 

m8 <- brglm(formula = birth ~ age1519 + age2024 + age2429 
            + age3034 + factor(n), 
                data = datapp1)
stargazer(m8)

#in magnitude different, otherwise significant and with the same signs 

#try with random effects. age and parity interaction 
m9 <- glmer(formula = birth ~ age1519 + age2024 + age2429 
            + age3034 + (1|OID) + factor(n) + factor(n)*age1519 +
            age2024*factor(n) + factor(n)*age3539 + factor(n)*age3034, 
            family = binomial(link ="logit"), data = datapp1)
stargazer(m9)

#nothing significant, probably misspecified 

#add treatment and experiment variables 
m10 <- glmer(formula = birth ~ treated*experiment + age1519 + age2024 + age2429 
            + age3034 + factor(n) + (1|OID), 
            data = datapp1)
stargazer(m10)


#let´s add plan dummies everywhere 

class(datapp1$OID)

datapp1$psum = 0
for (i in levels(datapp1$OID)){
  m <- mean(datapp1[datapp1$OID == i & !is.na(datapp1$plan), "plan"])
  datapp1$psum[datapp1$OID == i] <- m
}

datapp1$plan <- datapp1$psum

datapp1 <- fastDummies::dummy_columns(datapp1, select_columns = "plan")

m11 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
             + plan_3*experiment + plan_4*experiment + plan_5*experiment 
             + plan_7*experiment + plan_8*experiment + age1519 + age2024 + age2429 
             + age3034 + factor(n) + (1|OID), 
             data = datapp1)
stargazer(m11)

#Plan 7 is again positive and significant at 90% 

#with year fixed effects 

m12 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
             + plan_3*experiment + plan_4*experiment + plan_5*experiment 
             + plan_7*experiment + plan_8*experiment + age1519 + age2024 + age2429 
             + age3034 + factor(n) + (1|OID) + factor(year), 
             data = datapp1)
stargazer(m12)

#same 

#without parity dummy
m13 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
             + plan_3*experiment + plan_4*experiment + plan_5*experiment 
             + plan_7*experiment + plan_8*experiment + age1519 + age2024 + age2429 
             + age3034  + (1|OID) + factor(year), 
             data = datapp1)
stargazer(m13)


#plan 1 is significant at 95% and positive 

#with treatment dummy, without the parity dummy 
m14 <- glmer(formula = birth ~ treated*experiment + age1519 + age2024 + age2429 
             + age3034 + (1|OID), 
             data = datapp1)
stargazer(m14)

#results don´t change 

#with marital status dummy, but now we have to remove those that 
#are divorced, because we are missing the complete marital history  

datapp2 <- datapp1[-which(!is.na(datapp1$datesepcl)|!is.na(datapp1$datesepmrg)), ]

m15 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
             + plan_3*experiment + plan_4*experiment + plan_5*experiment 
             + plan_7*experiment + plan_8*experiment + age1519 + age2024 + age2429 
             + age3034  + (1|OID) + factor(year) + factor(n), 
             data = datapp2)
stargazer(m15)

#plan 1 positive and significant at 95%, and plan 7 is significant and positive at 90% 
#without the parity dummy, and plan 7 95% an plan 3 90% with the parity dummy 


m16 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
             + plan_3*experiment + plan_4*experiment + plan_5*experiment 
             + plan_7*experiment + plan_8*experiment + age1519 + age2024 + age2429 
             + age3034  + (1|OID) + married + factor(n), 
             data = datapp2)
stargazer(m16)

#how we define the parities change the results slightly, 
#if we treat all parities separately, plans 3 and 7 
# are positive and significant, if we treat all above 3
#as same, then also sígnificant, but at 90%, treating all above 
#4 produces the same results as treating them separately, 
#so why not just take j, more accurate  


#let´s try again with all units we have 
datapp$psum = 0
for (i in levels(datapp$OID)){
  m <- mean(datapp[datapp$OID == i & !is.na(datapp$plan), "plan"])
  datapp$psum[datapp$OID == i] <- m
}

datapp$plan <- datapp$psum

datapp <- fastDummies::dummy_columns(datapp, select_columns = "plan")

m17 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
             + plan_3*experiment + plan_4*experiment + plan_5*experiment 
             + plan_7*experiment + plan_8*experiment + age1519 + age2024 + age2429 
             + age3034 + age3539 + age4044 + (1|OID) + factor(j) + factor(year), 
             data = datapp)
stargazer(m17)


saveRDS(datapp1, "datapp1.rds")
saveRDS(datapp2, "datapp2.rds")
