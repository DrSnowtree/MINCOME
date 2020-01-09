basepay$incbracket <-  as.factor(basepay$incbracket) 
basepay$guarantee <-  as.factor(basepay$guarantee) 
basepay$rate <-  as.factor(basepay$rate) 
basepay$if_birth[basepay$if_birth9 == 1] <- 0





#baseline without education 
reg1 <- glm(formula = if_birth ~ treated + malepr + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)
summary(reg1)

reg2 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8 + malepr + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550  +
               FSI + NumChild  + incbracket +
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)
summary(reg2)

stargazer(reg1, reg2)

#controlling for the educational level of the female householder 

reg3 <- glm(formula = if_birth ~ treated + malepr + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
               yrschf + 
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)
summary(reg3)

reg4 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8 + malepr + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550  +
               FSI + NumChild  + incbracket +  yrschf + 
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)

stargazer(reg3, reg4)

#controlling for the educational level both householders

reg5 <- glm(formula = if_birth ~ treated + malepr + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
               yrschf + yrschm + 
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)


reg6 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8 + malepr + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550  +
               FSI + NumChild  + incbracket +  yrschf + yrschm + 
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)

stargazer(reg5, reg6)

#robustness 


reg7 <- glm(formula = if_birth ~ treated, family = binomial(link = "logit"), data = basepay)

summary(reg7)
#not significant

reg8 <- glm(formula = if_birth ~ treated + malepr, family = binomial(link = "logit"), data = basepay)

summary(reg8)
#not significant

reg9 <- glm(formula = if_birth ~ treated + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550, family = binomial(link = "logit"), data = basepay)

summary(reg9) 
#not significant

reg10 <- glm(formula = if_birth ~ treated + FSI, family = binomial(link = "logit"), data = basepay)

summary(reg10) 
#significant

reg11 <- glm(formula = if_birth ~ treated + incbracket, family = binomial(link = "logit"), data = basepay)

summary(reg11) 
#significant

reg12 <- glm(formula = if_birth ~ treated + NumChild, family = binomial(link = "logit"), data = basepay)

summary(reg12) 
#significant

reg13 <- glm(formula = if_birth ~ treated + chout, family = binomial(link = "logit"), data = basepay)

summary(reg13) 
#not significant, although this is normal, chout does not mean much by itself

reg14 <- glm(formula = if_birth ~ treated + chout + NumChild, family = binomial(link = "logit"), data = basepay)

summary(reg14)
#significant
reg15 <- glm(formula = if_birth ~ treated + chout + NumChild + if_birth9, family = binomial(link = "logit"), data = basepay)

summary(reg15)
#significant, the effect increases 
reg16 <- glm(formula = if_birth ~ treated + chout + NumChild +
             age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550, 
             family = binomial(link = "logit"), data = basepay)

summary(reg16)
#not significant 
reg17 <- glm(formula = if_birth ~ treated + chout + NumChild +
               age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550
             + FSI + incbracket, 
             family = binomial(link = "logit"), data = basepay)

summary(reg17)
#not significant
reg18 <- glm(formula = if_birth ~ treated + chout + NumChild +
               age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550
             + FSI + incbracket + malepr, 
             family = binomial(link = "logit"), data = basepay)

summary(reg18)
#significant 


reg19 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
              plan_5 + plan_7 + plan_8, family = binomial(link = "logit"), data = basepay)
summary(reg19)
#plan 1 at 90% level. plan 7 at 95% level 

reg20 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + malepr, family = binomial(link = "logit"), data = basepay)
summary(reg20)

#plan 7 significant 
reg21 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550, family = binomial(link = "logit"), data = basepay)
summary(reg21)
#plan 7 significant 

reg22 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + FSI, family = binomial(link = "logit"), data = basepay)
summary(reg22)
#plans 1, 5 and 7 significant at 90%, plan 3 at 95%


reg23 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + incbracket, family = binomial(link = "logit"), data = basepay)
summary(reg23)

#plans 1 at 95%, 7 at 90% 

regstr <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + incbracket + FSI
             , family = binomial(link = "logit"), data = basepay)
summary(regstr)
#only with stratifying variables: plans 1 and 3 significant at 95%, 5 and 7 at 90%

regstrtr <- glm(formula = if_birth ~ treated
              + incbracket + FSI
              , family = binomial(link = "logit"), data = basepay)
summary(regstrtr)
#only with stratifying variables, treatment significant at 90%


reg24 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + NumChild, family = binomial(link = "logit"), data = basepay)
summary(reg24)

#plans 1 and 3 at 90%, 7 at 95%

reg25 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + chout, family = binomial(link = "logit"), data = basepay)
summary(reg25)
#plans 1 at 90%, plan 7 at 95% 

reg26 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + chout + NumChild, family = binomial(link = "logit"), data = basepay)
summary(reg26)
#plan 3 at 90% and 7 significant at 95% 

reg27 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + chout + NumChild + if_birth9, family = binomial(link = "logit"), data = basepay)
summary(reg27)
#plan 3 at 90% and 7 significant at 95% 
reg28 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + DH + NumChild + if_birth9
             + DH, family = binomial(link = "logit"), data = basepay)
summary(reg28)
#plan 7 significant at 95%, plan 1 90%, plan 7 90% if we have malepr instead of DH 

reg29 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + chout + NumChild + if_birth9
             + FSI, family = binomial(link = "logit"), data = basepay)
summary(reg29)
#plan 1 and 3 significant at 90%, 7 at 95%

reg30 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + chout + NumChild + if_birth9
             + FSI + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550
             , family = binomial(link = "logit"), data = basepay)
summary(reg30)
#plan 3 significant at 90%, and plan 7 at 95% 




#then we get the plan 7 significant at 

stargazer(reg1, reg2)
