basepay$incbracket <-  as.factor(basepay$incbracket) 
basepay$guarantee <-  as.factor(basepay$guarantee) 
basepay$rate <-  as.factor(basepay$rate) 


reg1 <- glm(formula = if_birth ~ treated + DH + age1519 + age2024 + age2429 + 
      age3034 + age3539 + age4044 + age4550 + FSI + chnr + incbracket  +
      chout, family = binomial(link = "logit"), data = basepay)

reg2 <- glm(formula = if_birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
      plan_5 + plan_7 + plan_8 + DH + age1519 + age2024 + age2429 + 
      age3034 + age3539 + age4044 + age4550 + FSI + chnr + incbracket +  
      chout, family = binomial(link = "logit"), data = basepay)

reg3 <- glm(formula = if_birth ~ guarantee + rate + DH + age1519 + age2024 + 
      age2429 + age3034 + age3539 + age4044 + age4550 + FSI + chnr + 
      incbracket + chout, family = binomial(link = "logit"), data = basepay)
stargazer(reg1, reg2)
