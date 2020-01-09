basepay2$incbracket <-  as.factor(basepay2$incbracket) 
basepay2$guarantee <-  as.factor(basepay2$guarantee) 
basepay2$rate <-  as.factor(basepay2$rate) 
basepay2$if_birth[basepay2$if_birth9 == 1] <- 0



#baseline without education 
reg1 <- glm(formula = if_birth ~ treated + malepr + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
              chout + if_birth9, family = binomial(link = "logit"), data = basepay2)
summary(reg1)

reg2 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
              plan_5 + plan_7 + plan_8 + malepr + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550  +
              FSI + NumChild  + incbracket +
              chout + if_birth9, family = binomial(link = "logit"), data = basepay2)
summary(reg2)

stargazer(reg1, reg2)

#controlling for the educational level of the female householder 

reg3 <- glm(formula = if_birth ~ treated + DH + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
              yrschf + 
              chout + if_birth9, family = binomial(link = "logit"), data = basepay2)
summary(reg3)

reg4 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
              plan_5 + plan_7 + plan_8 + DH + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550  +
              FSI + NumChild  + incbracket +  yrschf + 
              chout + if_birth9, family = binomial(link = "logit"), data = basepay2)

stargazer(reg3, reg4)

#controlling for the educational level both householders

reg5 <- glm(formula = if_birth ~ treated + malepr + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
              yrschf + yrschm + 
              chout + if_birth9, family = binomial(link = "logit"), data = basepay2)


reg6 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
              plan_5 + plan_7 + plan_8 + malepr + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550  +
              FSI + NumChild  + incbracket +  yrschf + yrschm + 
              chout + if_birth9, family = binomial(link = "logit"), data = basepay2)

stargazer(reg5, reg6)

#robustness 


reg7 <- glm(formula = if_birth ~ treated, family = binomial(link = "logit"), data = basepay2)

summary(reg7)
#not significant

reg8 <- glm(formula = if_birth ~ treated + malepr, family = binomial(link = "logit"), data = basepay2)

summary(reg8)
#not significant

reg9 <- glm(formula = if_birth ~ treated + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550, family = binomial(link = "logit"), data = basepay2)

summary(reg9) 
#not significant

reg10 <- glm(formula = if_birth ~ treated + FSI, family = binomial(link = "logit"), data = basepay2)

summary(reg10) 
#significant

reg11 <- glm(formula = if_birth ~ treated + incbracket, family = binomial(link = "logit"), data = basepay2)

summary(reg11) 
#significant

regstrtr <- glm(formula = if_birth ~ treated
                + incbracket + FSI
                , family = binomial(link = "logit"), data = basepay2)
summary(regstrtr)
#only with stratify

reg12 <- glm(formula = if_birth ~ treated + NumChild, family = binomial(link = "logit"), data = basepay2)

summary(reg12) 
#significant

reg13 <- glm(formula = if_birth ~ treated + chout, family = binomial(link = "logit"), data = basepay2)

summary(reg13) 
#not significant, although this is normal, chout does not mean much by itself

reg14 <- glm(formula = if_birth ~ treated + chout + NumChild, family = binomial(link = "logit"), data = basepay2)

summary(reg14)
#significant
reg15 <- glm(formula = if_birth ~ treated + chout + NumChild + if_birth9, family = binomial(link = "logit"), data = basepay2)

summary(reg15)
#significant, the effect increases 
basepay2$age <- as.factor(basepay2$age)
reg16 <- glm(formula = if_birth ~ treated + chout + NumChild +
               age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550, 
             family = binomial(link = "logit"), data = basepay2)

summary(reg16)
#not significant 
reg17 <- glm(formula = if_birth ~ treated + chout + NumChild +
               age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550
             + FSI + incbracket, 
             family = binomial(link = "logit"), data = basepay2)

summary(reg17)
#not significant
reg18 <- glm(formula = if_birth ~ treated + chout + NumChild +
               age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550
             + FSI + incbracket + malepr, 
             family = binomial(link = "logit"), data = basepay2)

summary(reg18)
#significant 


reg19 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8, family = binomial(link = "logit"), data = basepay2)
summary(reg19)
#plan 1 at 90% level. plan 7 at 95% level 

reg20 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + malepr, family = binomial(link = "logit"), data = basepay2)
summary(reg20)

reg20 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + malepr, family = binomial(link = "logit"), data = basepay2)
summary(reg20)

#plan 7 significant 
reg21 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550, family = binomial(link = "logit"), data = basepay2)
summary(reg21)
#plan 7 significant 

reg22 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + FSI, family = binomial(link = "logit"), data = basepay2)
summary(reg22)
#plans 1, 5 and 7 significant at 90%, plan 3 at 95%


reg23 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + incbracket, family = binomial(link = "logit"), data = basepay2)
summary(reg23)

#plans 1 at 95%, 7 at 90% 

regstr <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
                plan_5 + plan_7 + plan_8
              + incbracket + FSI
              , family = binomial(link = "logit"), data = basepay2)
summary(regstr)
#only with stratifying variables: plans 1 and 3 significant at 95%, 5 and 7 at 90%




reg24 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + NumChild, family = binomial(link = "logit"), data = basepay2)
summary(reg24)

#plans 1 and 3 at 90%, 7 at 95%

reg25 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + chout, family = binomial(link = "logit"), data = basepay2)
summary(reg25)
#plans 1 at 90%, plan 7 at 95% 

reg26 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + chout + NumChild, family = binomial(link = "logit"), data = basepay2)
summary(reg26)
#plan 3 at 90% and 7 significant at 95% 

reg27 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + chout + NumChild + if_birth9, family = binomial(link = "logit"), data = basepay2)
summary(reg27)
#plan 3 at 90% and 7 significant at 95% 
reg28 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + DH + NumChild + if_birth9
             , family = binomial(link = "logit"), data = basepay2)
summary(reg28)
#plan 7 significant at 95%, plan 1 90%, plan 7 90% if we havea malepr instead of DH 

reg29 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + chout + NumChild + if_birth9
             + FSI, family = binomial(link = "logit"), data = basepay2)
summary(reg29)
#plan 1 and 3 significant at 90%, 7 at 95%

reg30 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8
             + chout + NumChild + if_birth9
             + FSI + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550
             , family = binomial(link = "logit"), data = basepay2)
summary(reg30)
#plan 3 significant at 90%, and plan 7 at 95% 


