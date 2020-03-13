library("glm2")
library("oddsratio")
library("margins")
library("mfx")
library(jtools)
library(dplyr)
library(stargazer)
library(foreign)


#adding controls 
formula1 = birth ~ treated + DH + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
  chout + if_birth9 + femhome + changeDHSH + changeSHDH + NumAdults + yrschf + fill + hmown

formula2 = birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
  plan_5 + plan_7 + plan_8 + DH + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
  chout + if_birth9 + femhome + changeDHSH + changeSHDH + NumAdults + yrschf + fill + hmown

formula3 = birth ~ treated  + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
  chout + if_birth9 + femhome + NumAdults + yrschf + fill + hmown + MAGE + mill + minsch + yrschm 

formula4 = birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
  plan_5 + plan_7 + plan_8  + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
  chout + if_birth9 + femhome + NumAdults + yrschf + fill + hmown + MAGE + mill + minsch + yrschm

formula5 =  birth ~ treated  + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
  chout + if_birth9 + femhome + NumAdults + yrschf + fill + hmown + MAGE + mill + minsch + yrschm + edlevelmoth

formula6 =   birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
  plan_5 + plan_7 + plan_8  + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
  chout + if_birth9 + femhome + NumAdults + yrschf + fill + hmown + MAGE + mill + minsch + yrschm + fmotheduc


reg1 <- glm(formula = formula1, 
              family = binomial(link = "logit"), data = basepay)


reg2 <- glm(formula = formula2, 
              family = binomial(link = "logit"), data = basepay)
stargazer(reg2)



#adding controls for the male householder 

reg3 <- glm(formula = formula3, 
              family = binomial(link = "logit"), data = basepay)
stargazer(reg3)


reg4 <- glm(formula = formula4, family = binomial(link = "logit"), data = basepay)
stargazer(reg4)
levels(basepay$femhome)

#adding control for mother´s education

reg5 <- glm(formula = formula5, 
            family = binomial(link = "logit"), data = basepay)

summary(reg5, apply.coef = exp)


reg6 <- glm(formula = formula6, 
            family = binomial(link = "logit"), data = basepay)

stargazer(reg5, apply.coef = exp)


stargazer(reg1, reg2, reg3, reg4, reg5, reg6)
stargazer(reg3, reg5)

jtools::plot_summs(reg1, reg3, scale = TRUE, plot.distributions = FALSE,
                   coefs = c("Treated" = "treated"),
                   model.names = c("Without controls for male householder", "With controls for male householder"),
                   inner_ci_level = .9, colors = "Qual3", exp = TRUE)

jtools::plot_summs(reg2, reg4, scale = TRUE, plot.distributions = FALSE,
           coefs = c("Plan 1" = "plan_1","Plan 2" = "plan_2", "Plan 3" =
                       "plan_3",  
                     "Plan 4" ="plan_4", 
                     "Plan 5" ="plan_5", 
                     "Plan 7" ="plan_7", 
                     "Plan 8" ="plan_8"), 
           inner_ci_level = .9, colors = "Qual3")



install.packages("logitmfx")
install.packages("texreg")
library(texreg)
stargazer(reg1, reg2, reg3, reg4, reg5, reg6)

mef1 <- logitmfx(reg1, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef2 <- logitmfx(reg2, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef3 <- logitmfx(formula = formula3, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef4 <- logitmfx(formula = formula4, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef5 <- logitmfx(formula = formula5, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef6 <- logitmfx(formula = formula6, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())


texreg(list(mef1, mef2))
stargazer(reg3)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, apply.coef=exp, t.auto=F, p.auto=F, report = "vc*s")


write.dta(basepay, "basepay.dta")
