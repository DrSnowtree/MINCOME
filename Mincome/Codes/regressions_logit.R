basepay$incbracket <-  as.factor(basepay$incbracket) 
basepay$guarantee <-  as.factor(basepay$guarantee) 
basepay$rate <-  as.factor(basepay$rate) 
basepay$if_birth[basepay$if_birth9 == 1] <- 0



basepay <- basepay[-which(is.na(basepay$age)), ]

#baseline without education 

reg1 <- glm(formula = if_birth ~ treated + DH + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)
summary(reg1)

reg2 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8 + DH + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550  +
               FSI + NumChild  + incbracket +
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)



stargazer(reg1, reg2)

#controlling for the educational level of the female householder 

reg3 <- glm(formula = if_birth ~ treated + DH + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
               yrschf + 
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)
summary(reg3)

reg4 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8 + DH + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550  +
               FSI + NumChild  + incbracket +  yrschf + 
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)

stargazer(reg3, reg4)

#controlling for the educational level both householders

reg5 <- glm(formula = if_birth ~ treated + DH + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
               yrschf + yrschm + 
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)


reg6 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8 + DH + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550  +
               FSI + NumChild  + incbracket +  yrschf + yrschm + 
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)

stargazer(reg5, reg6)

#robustness 


reg7 <- glm(formula = if_birth ~ treated, family = binomial(link = "logit"), data = basepay)

summary(reg7)

+ DH + age1519 + age2024 + age2429 + 
  age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
  chout + if_birth9

reg2 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
              plan_5 + plan_7 + plan_8 + DH + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550  +
              FSI + NumChild  + incbracket +
              chout + if_birth9, family = binomial(link = "logit"), data = basepay)


