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
library("lme4")
library("optimx")
library("numDeriv")
library("RCurl")
library("dfoptim")
library("nloptr")

#https://bbolker.github.io/mixedmodels-misc/ecostats_chap.html
#https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html

data_personperioda <- datapp[which(datapp$birthyear > 1940), ]
data_personperioda$age <- as.factor(data_personperioda$age)

reg121 <- glm(event ~ treated*experiment + age + strata(OID) + strata(year), 
              family = binomial(link = "logit"), data = data_personperioda,  maxit = 500)

summary(reg121)
stargazer(reg121)

reg122 <- logistf(formula = event ~ treated*experiment + age1519 + age2024  + age2429, 
                  data = data_personperioda)

reg123 <- brglm(formula = birth ~ treated*experiment + age + factor(OID) + factor(year), 
                data = data_personperioda)
datapp_rem <- datapp[-which(!is.na(datapp$datesepcl) | !is.na(datapp$datesepmrg)), ]

m1 <- glmer(formula = birth ~ treated*experiment + age1519 + age202+ + (1|OID) + factor(year)
            +married, 
                family = binomial(link ="logit"), data = datapp_rem)

stargazer(m1)
#rescale
length(getME(m1,"theta"))
numcols <- grep("^c\\.",names(datapp))
datapps <- datapp
datapps[,numcols] <- scale(datapps[,numcols])
m1_sc <- update(m1,data=datapps)
#check for singularity
tt <- getME(m1_sc,"theta")
ll <- getME(m1_sc,"lower")
min(tt[ll==0])

#no problem of singularity 

#Double-checking gradient calculations, tolerance level typically set is 0.001
derivs1 <- m1_sc@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
# second one is much smaller, but still larger than the tolerance level 

dd <- update(m1_sc,devFunOnly=TRUE)
pars <- unlist(getME(m1_sc,c("theta","fixef")))
grad2 <- grad(dd,pars)
hess2 <- hessian(dd,pars)
sc_grad2 <- solve(hess2,grad2)
max(pmin(abs(sc_grad2),abs(grad2)))
#set the maximum repetiton higher
ss <- getME(m1_sc,c("theta","fixef"))
m2 <- update(m1_sc,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))


stargazer(m2)
#try a different optimizer 
m3 <- update(m1_sc,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
stargazer(m3)




#with plans, let´s say for now that before the experiment, they are all "control"
datapp$plan[is.na(datapp$plan)] <- 9 

m1 <- glmer(formula = birth ~ factor(plan) + factor(age) + (1|OID) + factor(year)
            +married, 
            family = binomial(link ="logit"), data = datapp)

stargazer(m1)
#rescale
length(getME(m1,"theta"))
numcols <- grep("^c\\.",names(datapp))
datapps <- datapp
datapps[,numcols] <- scale(datapps[,numcols])
m1_sc <- update(m1,data=datapps)
#check for singularity
tt <- getME(m1_sc,"theta")
ll <- getME(m1_sc,"lower")
min(tt[ll==0])

#no problem of singularity 

#Double-checking gradient calculations, tolerance level typically set is 0.001
derivs1 <- m1_sc@optinfo$derivs
sc_grad1 <- with(derivs1,solve(Hessian,gradient))
max(abs(sc_grad1))
max(pmin(abs(sc_grad1),abs(derivs1$gradient)))
# second one is much smaller, but still larger than the tolerance level 

dd <- update(m1_sc,devFunOnly=TRUE)
pars <- unlist(getME(m1_sc,c("theta","fixef")))
grad2 <- grad(dd,pars)
hess2 <- hessian(dd,pars)
sc_grad2 <- solve(hess2,grad2)
max(pmin(abs(sc_grad2),abs(grad2)))
#set the maximum repetiton higher
ss <- getME(m1_sc,c("theta","fixef"))
m2 <- update(m1_sc,start=ss,control=glmerControl(optCtrl=list(maxfun=2e4)))


stargazer(m2)
#try a different optimizer 
m3 <- update(m1_sc,start=ss,control=glmerControl(optimizer="bobyqa",
                                                 optCtrl=list(maxfun=2e5)))
stargazer(m3)


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
library("lme4")
library("optimx")
library("numDeriv")
library("RCurl")
library("dfoptim")
library("nloptr")

#+ factor(j)*factor(age)

#without married dummy 
m1 <- glmer(formula = birth ~ treated*experiment + factor(age) + (1|OID) + factor(year)
            + factor(j), 
            family = binomial(link ="logit"), data = datapp)
stargazer(m1)

#remove the households where we have missing information about the length of previous marriage, 
#and try with married dummy 
datapp_rem <- datapp[-which(!is.na(datapp$datesepcl) | !is.na(datapp$datesepmrg)), ]

datapp_rem$j[datapp_rem$j > 4] <- 4
m2 <- glmer(formula = birth ~ treated*experiment + age1519 + age2024 + age2429 
            + age3034 + age3539 + age4044 + (1|OID) + factor(decade)
            + factor(j) + married, 
            family = binomial(link ="logit"), data = datapp_rem)
stargazer(m2)
isSingular(m2)


#first birth, only have people who did not have a child before 1970
#have a dummy that indicates if there has been a childbirth before 1970

datapp$bef70 = 0
datapp$bef70[datapp$birth == 1 & datapp$year < 1970] <- 1
datapp$FAMNUM <- as.factor(datapp$FAMNUM)

for (i in levels(datapp$FAMNUM)){
  s <- sum(datapp[datapp$FAMNUM == i, "bef70"])
  datapp$bef70[datapp$FAMNUM == i] <- s
}

datapp$ifchbef70 = 0   
datapp$ifchbef70[datapp$bef70 != 0] <- 1
length(datapp$ifchbef70 == 1)

data <- datapp[which(datapp$ifchbef70 == 0), ]
data_rem <- data[-which(!is.na(data$datesepcl) | !is.na(data$datesepmrg)), ]
bwplot(data_rem$age)
data_rem$ov40 <- 0
data_rem$ov40[data_rem$age4044 == 1 | data_rem$age4550 == 1] <- 0

m3 <- glmer(formula = birth ~ treated*experiment + age1519 
            + age2024 + age2429 + age3034 + age3539 + (1|OID) + married, 
            family = binomial(link ="logit"), data = data_rem)
stargazer(m3)

length(levels(datapp$FAMNUM))

#Adding the no child dummy for all months until the first child birth  

data_rem$yfirstch <- 0
data_rem$yfirstch[data_rem$event_1 == 1] <- data_rem$year

data_rem$tot = 0 
for (i in levels(data_rem$FAMNUM)){
  s <- sum(data_rem[data_rem$FAMNUM == i, "yfirstch"])
  data_rem$tot[data_rem$FAMNUM == i] <- s
}

data_rem <- data_rem[-which(data_rem$year > data_rem$tot), ]

m4545 <- brglm(formula = birth ~ treated*experiment + factor(age) + factor(OID), 
               data = data_rem)
stargazer(m4545)

