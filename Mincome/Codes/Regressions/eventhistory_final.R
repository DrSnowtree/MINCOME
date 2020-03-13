
setwd("W:/WU/Projekte/mincome/Mincome/Data")
library("lme4")
library("stargazer")
library("ggstance")
library("jtools")
library("coefplot")
library(dplyr)

#remove the 5 women not included in the baseline analysis

bpid <- basepay[, 1]
bpid <- as.data.frame(bpid)

bpid <- bpid %>% rename(FAMNUM=bpid)


# only with women born after 1934 and never been divorced before 

datapp2 <- merge(datapp2, bpid, by = "FAMNUM", all = F)
datapp2$treated <- 1
datapp2$treated[datapp2$plan == 9] <- 0

m1 <- glmer(formula = birth ~ treated*experiment + factor(age) 
            + (1|OID) + factor(year) + factor(j) + married, 
            data = datapp2)
stargazer(m1)

          
m2 <- lmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
           + plan_3*experiment + plan_4*experiment + plan_5*experiment 
           + plan_7*experiment + plan_8*experiment + factor(age) 
            + (1|OID) + factor(year) + factor(j) + married, 
            data = datapp2)
stargazer(m2)

saveRDS(datapp2, "datapp2.rds")


##look only at married couples 

bp2 <- basepay[which(!is.na(basepay$MAGE)), ]
bp2 <- bp2[which(!is.na(bp2$yrschm)), ]
bpid2 <- bp2[, 1]

bpid2 <- as.data.frame(bpid2)

bpid2 <- bpid2 %>% rename(FAMNUM=bpid2)

datapp4 <- merge(datapp2, bpid2, by = "FAMNUM", all = F)

datapp4$treated <- 1
datapp4$treated[datapp4$plan == 9] <- 0

m3 <- glmer(formula = birth ~ treated*experiment + factor(age) 
            + (1|OID) + factor(year) + factor(j) + factor(married), 
            data = datapp4)
stargazer(m3)


m4 <- lmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
           + plan_3*experiment + plan_4*experiment + plan_5*experiment 
           + plan_7*experiment + plan_8*experiment + factor(age) 
           + (1|OID) + factor(year) + factor(j) + factor(married), 
           data = datapp4)
stargazer(m4)


saveRDS(datapp4, "datapp4.rds")

##third set of people 

bp3 <- bp2[which(!is.na(bp2$fmotheduc)), ]
bpid3 <- bp3[, 1]

bpid3 <- as.data.frame(bpid3)

bpid3 <- bpid3 %>% rename(FAMNUM=bpid3)

datapp5 <- merge(datapp2, bpid3, by = "FAMNUM", all = F)


saveRDS(datapp5, "datapp5.rds")  

m5 <- glmer(formula = birth ~ treated*experiment + factor(age) 
            + (1|OID) + factor(year) + factor(j) + factor(married), 
            data = datapp5)
stargazer(m5)


m6 <- lmer(formula = birth ~ plan_1*experiment + plan_2*experiment 
           + plan_3*experiment + plan_4*experiment + plan_5*experiment 
           + plan_7*experiment + plan_8*experiment + factor(age) 
           + (1|OID) + factor(year) + factor(j) + factor(married), 
           data = datapp5)

stargazer(m6)


stargazer(m1, m2, m3, m4, m5, m6)
