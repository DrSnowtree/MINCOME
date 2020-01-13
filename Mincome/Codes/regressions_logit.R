

#baseline, treatment variables and stratifying variables only 

reg1 <- glm(formula = birth ~ treated + DH + FSI + incbracket 
            , family = binomial(link = "logit"), data = basepay)
summary(reg1)

reg2 <- glm(formula = birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
              plan_5 + plan_7 + plan_8 + 
              FSI + incbracket, family = binomial(link = "logit"), data = basepay)
summary(reg2)

# with all controls except changes in composition of the household 


reg3 <- glm(formula = birth ~ treated + DH + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550 + FSI   + incbracket  +
               chout + if_birth9, 
             family = binomial(link = "logit"), data = basepay)
summary(reg3)

reg4 <- glm(formula = birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8 + DH + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550  +
               FSI   + incbracket +
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)
summary(reg4)

#also including changes in the composition of the household

reg5 <- glm(formula = birth ~ treated + DH + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
              chout + if_birth9 + changeDHSH + changeSHDH + NumAdults, 
            family = binomial(link = "logit"), data = basepay)
summary(reg5)

reg6 <- glm(formula = birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
              plan_5 + plan_7 + plan_8 + DH + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550  +
              FSI + NumChild  + incbracket +
              chout + if_birth9 + changeDHSH + changeSHDH + NumAdults, family = binomial(link = "logit"), data = basepay)
summary(reg6)


#controlling for the educational level of the female householder 

reg7 <- glm(formula = birth ~ treated + DH + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
               yrschf +
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)
summary(reg7)

reg8 <- glm(formula = birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8 + DH + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550  +
               FSI + NumChild  + incbracket +  yrschf +
               chout + if_birth9, family = binomial(link = "logit"), data = basepay)
summary(reg8)

# add changes in the household composition and number of adults 
reg9 <- glm(formula = birth ~ treated + DH + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
              yrschf +
              chout + if_birth9
            + changeDHSH + changeSHDH + NumAdults, family = binomial(link = "logit"), data = basepay)
summary(reg9)

reg10 <- glm(formula = birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
              plan_5 + plan_7 + plan_8 + DH + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550  +
              FSI + NumChild  + incbracket +  yrschf +
              chout + if_birth9 + changeDHSH + changeSHDH + NumAdults , family = binomial(link = "logit"), data = basepay)
summary(reg10)


reg11 <- glm(formula = birth ~ treated + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
               yrschf + yrschm + changeDHSH + changeSHDH + MAGE+
               chout + if_birth9 + NumAdults, family = binomial(link = "logit"), data = basepay)
summary(reg11)

reg12 <- glm(formula = birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8 + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550  +
               FSI + NumChild  + incbracket +  yrschf + yrschm + MAGE +
               chout + if_birth9 + changeDHSH + changeSHDH + NumAdults, 
             family = binomial(link = "logit"), data = basepay)
summary(reg12)

stargazer(reg1, reg2, reg3, reg4, reg5, reg6)
stargazer(reg7, reg8, reg9, reg10, reg11, reg12)

#see if probability of becoming a DH changes with being treated or not 


cor(basepay$treated, basepay$changeSHDH, method = c("pearson", "kendall", "spearman"))
cor.test(basepay$treated, basepay$changeSHDH, method=c("pearson", "kendall", "spearman"))

cor.test(basepay$plan_1, basepay$changeSHDH, method=c("pearson", "kendall", "spearman"))
cor.test(basepay$plan_2, basepay$changeSHDH, method=c("pearson", "kendall", "spearman"))
cor.test(basepay$plan_3, basepay$changeSHDH, method=c("pearson", "kendall", "spearman"))
cor.test(basepay$plan_4, basepay$changeSHDH, method=c("pearson", "kendall", "spearman"))
cor.test(basepay$plan_5, basepay$changeSHDH, method=c("pearson", "kendall", "spearman"))
cor.test(basepay$plan_7, basepay$changeSHDH, method=c("pearson", "kendall", "spearman"))
cor.test(basepay$plan_8, basepay$changeSHDH, method=c("pearson", "kendall", "spearman"))



#no significant correlation 
