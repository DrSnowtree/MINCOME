basepay$if_birth[basepay$FAMNUM == 14324] <- 0 
basepay$costch <- as.numeric(basepay$costch)
basepay$numvehic <- as.numeric(as.character(basepay$numvehic))
saveRDS(basepay, "basepay.rds")
library("glm2")
library("oddsratio")
library("margins")
library("mfx")
library(jtools)
library(dplyr)
library(stargazer)



basepay$age1519 <- as.factor(basepay$age1519)
basepay$age2024 <- as.factor(basepay$age2024)
basepay$age2429 <- as.factor(basepay$age2429)
basepay$age3034 <- as.factor(basepay$age3034)
basepay$age3539 <- as.factor(basepay$age3539)
basepay$age4044 <- as.factor(basepay$age4044)
basepay$treated <- as.factor(basepay$treated)
basepay$highschf <- as.factor(basepay$highschf)
basepay$femhome <- as.factor(basepay$femhome)

basepay$yrschf <- 0
basepay$yrschf[basepay$highschf == 1 & basepay$yrschf < 13] <- 1
basepay$yrschf[basepay$highschf == 1 & basepay$yrschf > 12] <- 2


basepay$fmotheduc <- 0
basepay$fmotheduc[basepay$highschm == 1 & basepay$yrschm < 13] <- 1
basepay$fmotheduc[basepay$highschm == 1 & basepay$yrschm > 12] <- 2

basepay$fmotheducoth <- 0
basepay$fmotheducoth[basepay$fmotheduc < 13 & basepay$fmotheduc > 8] <- 1
basepay$fmotheducoth[basepay$fmotheduc > 12] <- 2
basepay$fmotheducoth[is.na(basepay$fmotheduc)] <- NA

basepay$yrschf <- as.factor(basepay$yrschf)
basepay$fmotheduc <- as.factor(basepay$fmotheduc)

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
  chout + if_birth9 + femhome + NumAdults + yrschf + fill + hmown + MAGE + mill + minsch + yrschm + fmotheduc

formula6 =   birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
  plan_5 + plan_7 + plan_8  + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
  chout + if_birth9 + femhome + NumAdults + yrschf + fill + hmown + MAGE + mill + minsch + yrschm + fmotheduc


reg1 <- glm(formula = birth ~ treated + DH + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket + numvehic + valvehic + 
              chout + if_birth9 + femhome + changeDHSH + changeSHDH + NumAdults + yrschf + fill + hmown, 
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

stargazer(reg5)


reg6 <- glm(formula = formula6, 
            family = binomial(link = "logit"), data = basepay)

stargazer(reg6)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6, apply.coef=exp,apply.se=exp, t.auto=F, p.auto=F, report = "vc*s")
stargazer(reg1, reg2, reg3, reg4, reg5, reg6)



jtools::plot_summs(reg2, reg6, scale = TRUE, plot.distributions = FALSE,
           coefs = c("Plan 1" = "plan_1","Plan 2" = "plan_2", "Plan 3" =
                       "plan_3",  
                     "Plan 4" ="plan_4", 
                     "Plan 5" ="plan_5", 
                     "Plan 7" ="plan_7", 
                     "Plan 8" ="plan_8"), 
           inner_ci_level = .9, colors = "Qual3")

model.names = c("Without controls for male householder", "With controls for male householder", 
                "With control for mother`s education"),

jtools::plot_summs(reg1, reg3, reg5, scale = TRUE, plot.distributions = FALSE,
           coefs = c("Treated" = "treated"),
           inner_ci_level = .9, colors = "Qual3")


install.packages("logitmfx")
install.packages("texreg")
library(texreg)


mef1 <- logitmfx(formula = formula1, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
         clustervar2 = NULL, start = NULL, control = list())

mef2 <- logitmfx(formula = formula2, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef3 <- logitmfx(formula = formula3, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef4 <- logitmfx(formula = formula4, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef5 <- logitmfx(formula = formula5, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())
mef6 <- logitmfx(formula = formula6, data = basepay, atmean = FALSE, robust = FALSE, clustervar1 = NULL, 
                 clustervar2 = NULL, start = NULL, control = list())

stargazer(reg1, reg2, reg3, reg4, reg5, reg6)
texreg(list(mef1, mef2, mef3, mef4, mef5, mef6), stars = 0.1)
stargazer(reg3)
