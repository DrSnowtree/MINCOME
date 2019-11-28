#Regressions 

library("aod")
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
library("fastDummies")

basepaycross_rem$if_increase <- as.factor(basepaycross_rem$if_increase)
basepaycross_rem <- fastDummies::dummy_cols(basepaycross_rem, select_columns = c("plan", "FAMSIZE"))

probitreg <- glm(if_increase ~ plan_2 + plan_1 + plan_3 +
                   plan_4 + plan_5 + 
                  + plan_7 + plan_8 + 
                   FAGE + MAGE + DH + SH, family = binomial(link = "probit"), 
                data = basepaycross_rem)

probitreg <- glm(if_increase ~ control + 
                   FAMS + 
                   FAGE + MAGE + DH + SH, family = binomial(link = "probit"), 
                 data = basepaycross_rem)


summary(probitreg)
