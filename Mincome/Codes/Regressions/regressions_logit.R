
basepay$if_birth[basepay$FAMNUM == 14324] <- 0 
basepay$costch <- as.numeric(basepay$costch)
saveRDS(basepay, "basepay.rds")
library("glm2")
library("oddsratio")
library("margins")
library("mfx")
library(jtools)
library(dplyr)

#function to compute marginal effects 

#function to calculate marginal effects
mfx <- function(x,sims=1000){
  set.seed(1984)
  pdf <- ifelse(as.character(x$call)[3]=="binomial(link = \"logit\")",
                mean(dnorm(predict(x, type = "link"))),
                mean(dlogis(predict(x, type = "link"))))
  pdfsd <- ifelse(as.character(x$call)[3]=="binomial(link = \"logit\")",
                  sd(dnorm(predict(x, type = "link"))),
                  sd(dlogis(predict(x, type = "link"))))
  marginal.effects <- pdf*coef(x)
  sim <- matrix(rep(NA,sims*length(coef(x))), nrow=sims)
  for(i in 1:length(coef(x))){
    sim[,i] <- rnorm(sims,coef(x)[i],diag(vcov(x)^0.5)[i])
  }
  pdfsim <- rnorm(sims,pdf,pdfsd)
  sim.se <- pdfsim*sim
  res <- cbind(marginal.effects,sd(sim.se))
  colnames(res)[2] <- "standard.error"
  ifelse(names(x$coefficients[1])=="(Intercept)",
         return(res[2:nrow(res),]),
         return(res))
}

#baseline, treatment variables and stratifying variables only 

reg1 <- glm(formula = birth ~ treated + FSI + incbracket 
            , family = binomial(link = "logit"), data = basepay)


reg2 <- glm(formula = birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
              plan_5 + plan_7 + plan_8 + 
              FSI + incbracket, family = binomial(link = "logit"), data = basepay)

# with all controls except changes in composition of the household 


reg3 <- glm(formula = birth ~ treated + SH + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550 + FSI  + incbracket  +
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

# add changes in the household composition
basepay$NumAdults[is.na(basepay$NumAdults)] <- 0

reg9 <- glm(formula = birth ~ treated + DH + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550 + FSI + NumChild  + incbracket  +
              yrschf + costch + 
              chout + if_birth9
            + changeDHSH + changeSHDH
            +femhome, family = binomial(link = "logit"), data = basepay)
effects_reg9 = margins(reg9) 

effects_reg9


reg10 <- glm(formula = birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
              plan_5 + plan_7 + plan_8 + DH + age1519 + age2024 + age2429 + 
              age3034 + age3539 + age4044 + age4550  + costch + 
              FSI + NumChild  + incbracket +  yrschf +
              chout + if_birth9 + changeDHSH + changeSHDH
             + femhome, family = binomial(link = "logit"), data = basepay)

effects_reg10 = margins(reg10) 
effects_reg10 <- summary(effects_reg10)

reg11 <- glm(formula = birth ~ treated + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550  + NumChild  +
               yrschf + yrschm +  MAGE+
               chout + if_birth9 + costch + femhome
             , family = binomial(link = "logit"), data = basepay)
effects_reg11 = margins(reg11) 
effects_reg11 <- summary(effects_reg11)

reg12 <- glm(formula = birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8 + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550 + NumChild  +  yrschf + yrschm + MAGE +
               chout + if_birth9 + costch + femhome, 
             family = binomial(link = "logit"), data = basepay)
effects_reg12 = margins(reg12) 
effects_reg12 <- summary(effects_reg12)


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

#visualize coefficients 

install.packages("jtools")
install.packages("ggstance")



plot_summs(reg2, reg10, reg12,  scale = TRUE, plot.distributions = FALSE, 
           model.names = c("Baseline model", "With controls (female householder)", 
                           "With controls (both householders)"),
           coefs = c( "Plan 1" = "plan_1","Plan 2" = "plan_2", "Plan 3" =
                       "plan_3",  
                     "Plan 4" ="plan_4", 
                     "Plan 5" ="plan_5", 
                     "Plan 7" ="plan_7", 
                     "Plan 8" ="plan_8"),  
                     inner_ci_level = .9, colors = "Qual3", 
           exp = TRUE)

plot_summs(reg1, reg9, reg11,  scale = TRUE, plot.distributions = FALSE, 
           model.names = c("Baseline model", "With controls (female householder)", 
                           "With controls (both householders)"),
           coefs = c("Treated" = "treated"),  
           inner_ci_level = .9, colors = "Qual3", exp=TRUE)

stargazer(reg1, reg2, reg9, reg10, reg11, reg12, apply.coef=exp, t.auto=F, p.auto=F, report = "vct*")


plot_summs(reg2, reg4, scale = TRUE, plot.distributions = FALSE, 
           model.names = c("Without controls", "With controls"),
           coefs = c("Plan 1" = "plan_1","Plan 2" = "plan_2", "Plan 3" =
                       "plan_3",  
                     "Plan 4" ="plan_4", 
                     "Plan 5" ="plan_5", 
                     "Plan 7" ="plan_7", 
                     "Plan 8" ="plan_8"),  
           inner_ci_level = .9, colors = "Qual3")

plot_summs(reg1, reg3, scale = TRUE, plot.distributions = TRUE, 
           model.names = c("Without controls", "With controls"),
           coefs = c("Treated" ="treated"),  
           inner_ci_level = .9, colors = "Qual3")

plot_summs(reg1, reg3, scale = TRUE, plot.distributions = FALSE, 
           model.names = c("Without controls", "With controls"),
           coefs = c("Treated" ="treated"),  
           inner_ci_level = .9, colors = "Qual3")

length(which(basepay$treated == 0 & is.na(basepay$MAGE))) 
length(which(basepay$treated == 1 & is.na(basepay$MAGE)))       
       
length(which(basepay$treated == 0)) 
length(which(basepay$treated == 1))

res <- summary(reg1)

pt(coef(res)[, 1], reg1$df, lower = FALSE)


res <- summary(reg1)

pt(coef(res)[, 1], reg1$df, lower = FALSE)
#different control groups  
basepay$age40plus <- 0 
basepay$age40plus[basepay$age > 39] <- 1
base1 <- basepay[which(basepay$plan == "7"), ]
boxplot(base1$age)





