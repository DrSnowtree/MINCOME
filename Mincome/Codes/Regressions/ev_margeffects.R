setwd("W:/WU/Projekte/mincome/Mincome/Data")
library("lme4")
library("stargazer")
library(dplyr)
library(ggeffects)

##function to calculate marginal effects for glmer
glmermfx <- function(x,nsims=1000){
  set.seed(1984)
  pdf <- mean(dlogis(-log((1-fitted(x))/fitted(x))))
  pdfsd <- sd(dlogis(-log((1-fitted(x))/fitted(x))))
  marginal.effects <- pdf*fixef(x)
  sim <- matrix(rep(NA,nsims*length(fixef(x))), nrow=nsims)
  for(i in 1:length(fixef(x))){
    sim[,i] <- rnorm(nsims,fixef(x)[i],diag(vcov(x)^0.5)[i])
  }
  pdfsim <- rnorm(nsims,pdf,pdfsd)
  sim.se <- pdfsim*sim
  res <- cbind(marginal.effects,sd(sim.se))
  colnames(res)[2] <- "standard.error"
  ifelse(names(fixef(x))[1]=="(Intercept)",
         return(res[2:nrow(res),]),return(res))
}


#descriptives --> fertility rates by age 1970-1977

#all people we had in the baseline, so 440 women, 35 years for each 
datapp$trexp <- datapp$treated*datapp$experiment
m1 <- glm(formula = birth ~ trexp + treated + experiment, 
            data = datapp)
stargazer(m1)
mfx(m1)
ggpredict(m1, "trexp")
#not significant 
m2 <- glm(formula = birth ~ plan_1*experiment + plan_2*experiment 
            + plan_3*experiment + plan_4*experiment + plan_5*experiment 
            + plan_7*experiment + plan_8*experiment, 
            data = datapp)
stargazer(m2)
mfx(m2)

#plans 5 and 7 significant 



##now only with women born after 1934, as before, the childbirth history might suffer from 
#left trunctuation

m5 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
            + plan_3*experiment + plan_4*experiment + plan_5*experiment 
            + plan_7*experiment + plan_8*experiment + 
              factor(age) + (1|OID) + factor(year) + factor(j), 
            data = datapp1)
stargazer(m5)


#Plans 3 and 7 positive and significant (without the parity dummy, plan 1 also significant, but not 3 and 7)


#with married dummy, removing divorced women 

m6 <- glmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
            + plan_3*experiment + plan_4*experiment + plan_5*experiment 
            + plan_7*experiment + plan_8*experiment + factor(age) 
            + (1|OID) + factor(year) + factor(j) + married, 
            data = datapp2)
stargazer(m6)

#3 and 7 are significant 



stargazer(m2, m6, apply.coef=exp, t.auto=F, p.auto=F, report = "vct*")

library(foreign)
write.dta(datapp, "datapp.dta") 

