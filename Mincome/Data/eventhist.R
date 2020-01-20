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
install.packages("lme4")
library("lme4")
library("optimx")


#https://bbolker.github.io/mixedmodels-misc/ecostats_chap.html
#https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

data_personperioda <- data_personperiod[which(data_personperiod$birthyear > 1940), ]
data_personperioda$age <- as.factor(data_personperioda$age)

reg121 <- glm(event ~ treated*experiment + age + strata(OID) + strata(year), 
              family = binomial(link = "logit"), data = data_personperioda,  maxit = 500)

summary(reg121)
stargazer(reg121)

reg122 <- logistf(formula = event ~ treated*experiment + age1519 + age2024  + age2429, 
                  data = data_personperioda)

reg123 <- brglm(formula = event ~ treated*experiment + age + strata(OID) + strata(year), 
                data = data_personperiod)

reg124 <- glmer(formula = event ~ treated*experiment + factor(age) + (1|OID) + factor(year), 
                family = binomial(link ="logit"), data = data_personperioda,  REML = FALSE,
                control=glmerControl(optimizer="optimx", optCtrl=list(method='nlminb')))

stargazer::stargazer(reg124)
