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


basepaypanel <- read_dta("W:/WU/Projekte/mincome/Mincome/Data/basepaypanel_revised.dta")
basepaypanel[basepaypanel == -9] <- NA
basepaypanel[basepaypanel == -7] <- NA
basepaypanel[basepaypanel == -1] <- NA
basepaypanel[basepaypanel == "."] <- NA
basepaypanel$individual <- 0 
basepaypanel$individual[basepaypanel$DoubleHead1== 0 & basepaypanel$SingleHead1 == 0] <- 1

#only have Winnipeg 

basepaypanel <- basepaypanel[which(basepaypanel$SiteCode == 1),]

basepaypanel <- basepaypanel %>%
  group_by(FamNum) %>%
  arrange(month)  %>%
  mutate(prevnrch = dplyr::lag(CH, 1))  %>%
  ungroup()%>%
  arrange(FamNum, month)

basepaypanel$increase = 0 
basepaypanel$increase[basepaypanel$prevnrch < basepaypanel$CH] <- 1

#get the information on the treatment cell of the household

basepaypanel$AC <- as.character(basepaypanel$AC)
basepaypanel$plan <- substr(basepaypanel$AC, 1, 1)
basepaypanel$plan <- as.factor(basepaypanel$plan)

#by eliminating the NAs, we are removing the months for each household 
#where they were not enrolled in the experiment 
basepaypanel <- basepaypanel[!is.na(basepaypanel$plan),]

#month1 will then be 1 if that is the month where the family has begun 
#the experiment
basepaypanel <- basepaypanel %>%
  group_by(FamNum) %>%
  arrange(month)  %>%
  mutate(month1 = row_number())  %>%
  ungroup()%>%
  arrange(FamNum, month)

#CR and CS give the male and female householders´ age, also for households that were missing 
#at the beginning of the experiment 

basepaypanel$mage<- basepaypanel$CR
basepaypanel$fage<- basepaypanel$CS

faminfo <- subset(basepaypanel, select=c("FamNum","FSI", "FS", "month1", "mage", "fage", "AC"))
faminfo <- faminfo[which(faminfo$month1 ==1),]
faminfo <- faminfo %>% rename(FAMSI=FSI)
faminfo <- faminfo %>% rename(FAMS=FS)
faminfo <- faminfo %>% rename(FAGE=fage)
faminfo <- faminfo %>% rename(MAGE=mage)
faminfo <- faminfo %>% rename(asscell=AC)
faminfo <- faminfo[,-4]
#have a dummy that indicates if there has been any increases

basepaypanel$sum = 0

basepaypanel$FamNum <- as.factor(basepaypanel$FamNum)
for (i in levels(basepaypanel$FamNum)){
  s <- sum(basepaypanel[basepaypanel$FamNum == i, "increase"])
  basepaypanel$sum[basepaypanel$FamNum == i] <- s
}

basepaypanel$if_increase = 0   
basepaypanel$if_increase[basepaypanel$sum != 0] <- 1

#Plan 6 was merged with plan 7 at some point
basepaypanel$plan[basepaypanel$plan == 6] <- 7

#have a control dummy, 0 for treated, 1 for control group 
basepaypanel$control = 0 
basepaypanel$control[basepaypanel$plan == 9] <- 1

basepaypanel$treated = 0 
basepaypanel$treated[basepaypanel$control == 0] <- 1

#there are families whose treament plan changes 
#and even whether they are in control group or not 

basepaypanel$changetreatment = 0 

basepaypanel <- basepaypanel %>%
  group_by(FamNum) %>%
  arrange(month)  %>%
  mutate(prevtreat = dplyr::lag(plan, 1))  %>%
  ungroup()%>%
  arrange(FamNum, month)

basepaypanel$changetreatment[basepaypanel$prevtreat !=basepaypanel$plan] = 1 

change <- basepaypanel[which(basepaypanel$changetreatment == 1),]

#46 families in Winnipeg, some sometimes in control and treatment  

#let's remove these families for now and then see 

basepaypanel$summ = 0

basepaypanel$FamNum <- as.factor(basepaypanel$FamNum)
for (i in levels(basepaypanel$FamNum)){
  s <- sum(basepaypanel[basepaypanel$FamNum == i, "changetreatment"])
  basepaypanel$summ[basepaypanel$FamNum == i] <- s
}

basepaypanel$if_change = 0   
basepaypanel$if_change[basepaypanel$summ != 0] <- 1

basepaypanel<-merge(basepaypanel, faminfo, by = "FamNum", all=T)

basepaypanel_rem <- basepaypanel[which(basepaypanel$AC != 0),]

basepaypanel_rem <- basepaypanel_rem[which(basepaypanel_rem$if_change == 0),]

#have it cross section 

basepaypanel_rem$plan <- as.numeric(as.character(basepaypanel_rem$plan))
basepaypanel_rem$control <- as.numeric(as.character(basepaypanel_rem$control))
basepaypanel_rem$if_increase <- as.numeric(as.character(basepaypanel_rem$if_increase))
basepaypanel_rem$MAGE <- as.numeric(as.character(basepaypanel_rem$MAGE))
basepaypanel_rem$FAGE <- as.numeric(as.character(basepaypanel_rem$FAGE))
basepaypanel_rem$DoubleHead1 <- as.numeric(as.character(basepaypanel_rem$DoubleHead1))
basepaypanel_rem$individual <- as.numeric(as.character(basepaypanel_rem$individual))
basepaypanel_rem$SingleHead1 <- as.numeric(as.character(basepaypanel_rem$SingleHead1))
basepaypanel_rem$asscell <- as.numeric(as.character(basepaypanel_rem$asscell))


basepay <- basepaypanel_rem %>%
  group_by(FamNum)%>%
  summarise(if_birth=mean(if_increase), control = mean(control), plan =mean(plan), 
            MAGE = mean(MAGE), FAGE = mean(FAGE), DH = mean(DoubleHead1), individual = mean(individual), 
            SH =mean(SingleHead1), AC = mean(asscell)) %>% 
  ungroup()
  

bpinfo <- base_pay_data_revised_Dec_11_2019 %>%
  dplyr::select(`Fam Num`, `Fam Size (x100)`, `Tot Fam Inc 74`, `Num Child`, `Site Code`)

bpinfo <- bpinfo[which(bpinfo$`Site Code` == 1), ]
bpinfo[bpinfo == -9] <- NA

names(bpinfo)[names(bpinfo) == 'Fam Num'] <- "FamNum"

basepay <- merge(basepay, bpinfo, by = "FamNum", all = TRUE)
basepay <- basepay[-which(is.na(basepay$if_birth)), ]
basepay$treated <- 0 
basepay$treated[basepay$control == 0] <- 1
basepay <- basepay[-which(is.na(basepay$FAGE)), ]

names(basepay)[names(basepay) == 'FAGE'] <- "age"
basepay <- fastDummies::dummy_columns(basepay, select_columns = "age")
basepay <- fastDummies::dummy_columns(basepay, select_columns = "plan")


basepay$age1519 <- 0
basepay$age1519[basepay$age == 15 | basepay$age == 16 | basepay$age == 17 |
                  basepay$age == 18 | basepay$age == 19  ] <- 1


basepay$age2024 <- 0 
basepay$age2024[basepay$age == 20 | basepay$age == 21 | basepay$age == 22 |
                  basepay$age == 23 | basepay$age == 24  ] <- 1


basepay$age2429 <- 0 
basepay$age2429[basepay$age == 24 | basepay$age == 25 | basepay$age == 26 |
                             basepay$age == 27 | basepay$age == 28  ] <- 1

basepay$age3034 <- 0 
basepay$age3034[basepay$age == 30 | basepay$age == 31 | basepay$age == 32 |
                             basepay$age == 33 | basepay$age == 34  ] <- 1



basepay$age3539 <- 0
basepay$age3539[basepay$age == 35 | basepay$age == 36 | basepay$age == 37 |
                             basepay$age == 38 | basepay$age == 39  ] <- 1

basepay$age4044 <- 0 
basepay$age4044[basepay$age == 40 | basepay$age == 41 | basepay$age == 42 |
                             basepay$age == 43 | basepay$age == 44  ] <- 1 


basepay$age4550 <- 0 
basepay$age4550[basepay$age == 45 | basepay$age == 46 | basepay$age == 47 |
                             basepay$age == 48 | basepay$age == 49  ] <- 1 

basepay$age50plus <- 0 
basepay$age50plus[basepay$age > 50 ] <- 1 
names(basepay)[names(basepay) == 'Fam Size (x100)'] <- "FSI"
names(basepay)[names(basepay) == 'Num Child'] <- "chnr"
names(basepay)[names(basepay) == 'Tot Fam Inc 74'] <- "TotInc74"
basepay$TotInc74 <- as.numeric(basepay$TotInc74)
basepay$age <- as.factor(basepay$age)
basepay$rate <- 0
basepay$rate[basepay$plan == 1 |basepay$plan == 2 ] <- 35
basepay$rate[basepay$plan == 3 |basepay$plan == 4 | basepay$plan == 5] <- 50
basepay$rate[basepay$plan == 7 |basepay$plan == 28 ] <- 75


basepay$guarantee <- 0
basepay$guarantee[basepay$plan == 1 |basepay$plan == 3 ] <- 3800
basepay$guarantee[basepay$plan == 2 |basepay$plan == 4 | basepay$plan == 7] <- 4800
basepay$guarantee[basepay$plan == 5 |basepay$plan == 8 ] <- 5480

familydata <- read_excel("W:/WU/Projekte/mincome/Mincome/Data/familydata.xlsx")

familydata <- familydata %>%
  dplyr::select("FamNum", "chout")
  
names(familydata)

stata.merge <- function(x,y, by = intersect(names(x), names(y))){
  
  x[is.na(x)] <- Inf
  y[is.na(y)] <- Inf
  
  matched <- merge(x, y, by.x = by, by.y = by, all = TRUE)
  matched <- matched[complete.cases(matched),]
  matched$merge <- "matched"
  master <- merge(x, y, by.x = by, by.y = by, all.x = TRUE)
  master <- master[!complete.cases(master),]
  master$merge <- "master"
  using <- merge(x, y, by.x = by, by.y = by, all.y = TRUE)
  using <- using[!complete.cases(using),]
  using$merge <- "using"
  
  df <- rbind(matched, master,using)
  df[sapply(df, is.infinite)] <- NA
  df
}

basepay <- stata.merge(familydata, basepay, by = "FamNum")
basepay <- basepay[-which(basepay$merge == "master"), ]
basepay[basepay == -9] <- NA
basepay$chout[is.na(basepay$chout)] <- 0

basepay$AC <- as.character(basepay$AC)
basepay$incbracket <- substr(basepay$AC, 2, 3)
basepay$incbracket <- as.factor(basepay$incbracket)

length(which(basepay$plan == 1))
length(which(basepay$plan == 2))
length(which(basepay$plan == 3))
length(which(basepay$plan == 4))
length(which(basepay$plan == 5))
length(which(basepay$plan == 7))
length(which(basepay$plan == 8))
length(which(basepay$plan == 9))

#Regression 

  
  
reg1 <- glm(if_birth ~ treated + age1519 + age2024 + age2429 + age3034 
            + age3539 + age4044 + age4550
            + age1519 + age2024 + age2429 + age3034 
            + age3539 + age4044 + age4550
            + FSI + chnr + TotInc74 + chout
            + DH 
            ,family=binomial(link='logit'),data=basepay)

summary(reg1)
stargazer(reg1)

basepay$chnr <- as.factor(basepay$chnr)
basepay$chout <- as.factor(basepay$chout)
reg2 <- glm(if_birth ~ plan_1 + plan_2 
            + plan_3 + plan_4 + plan_5 
            + plan_7 + plan_8 
            + age1519 + age2024 + age2429 + age3034 
            + age3539 + age4044 + age4550
            + FSI + chnr + TotInc74 + chout
            + DH ,family=binomial(link='logit'),data=basepay)
summary(reg2)
stargazer(reg2)

reg3 <- glm(if_birth ~ treated + age
            + FSI + chnr + TotInc74
            + chout,family=binomial(link='logit'),data=basepay)



reg4 <- glm(if_birth ~ plan_1 + plan_2 
            + plan_3 + plan_4 + plan_5 
            + plan_7 + plan_8 
            + age + FSI + chnr + chout
            + TotInc74,family=binomial(link='logit'),data=basepay)
summary(reg4)



reg5 <-  glm(if_birth ~ plan_1 + plan_2 
             + plan_3 + plan_4 + plan_5 
             + plan_7 + plan_8 + DH
             ,family=binomial(link='logit'),data=basepay)

summary(reg5)

reg6 <-  glm(if_birth ~ plan_1 + plan_2 
             + plan_3 + plan_4 + plan_5 
             + plan_7 + plan_8 + DH + SH
             ,family=binomial(link='logit'),data=basepay)

summary(reg6)

reg7 <-  glm(if_birth ~ plan_1 + plan_2 
             + plan_3 + plan_4 + plan_5 
             + plan_7 + plan_8 + DH + SH 
             + age1519 + age2024
             + age2429 + age3034
             + age3539 + age4044
             + age4550
             ,family=binomial(link='logit'),data=basepay)

summary(reg7)

reg8 <-  glm(if_birth ~ plan_1 + plan_2 
             + plan_3 + plan_4 + plan_5 
             + plan_7 + plan_8 + DH + SH 
             + age1519 + age2024
             + age2429 + age3034
             + age3539 + age4044
             + age4550 + FSI
             ,family=binomial(link='logit'),data=basepay)


reg9 <-  glm(if_birth ~ plan_1 + plan_2 
             + plan_3 + plan_4 + plan_5 
             + plan_7 + plan_8 + DH + SH 
             + age1519 + age2024
             + age2429 + age3034
             + age3539 + age4044
             + age4550 + FSI + TotInc74
             ,family=binomial(link='logit'),data=basepay)

summary(reg9)

reg10 <-  glm(if_birth ~ plan_1 + plan_2 
             + plan_3 + plan_4 + plan_5 
             + plan_7 + plan_8 + DH  
             + age1519 + age2024
             + age2429 + age3034
             + age3539 + age4044
             + age4550 + FSI + TotInc74
             + chnr 
             ,family=binomial(link='logit'),data=basepay)

summary(reg10)
stargazer(reg10)

reg11 <-  glm(if_birth ~ treated + DH  
              + age1519 + age2024
              + age2429 + age3034
              + age3539 + age4044
              + age4550 + FSI + TotInc74
              + chnr 
              ,family=binomial(link='logit'),data=basepay)

summary(reg11)

stargazer(reg11)

reg12 <-  glm(if_birth ~ treated + DH  
              + age1519 + age2024
              + age2429 + age3034
              + age3539 + age4044
              + age4550 + FSI 
              + chnr 
              ,family=binomial(link='logit'),data=basepay)

summary(reg12)

stargazer(reg12)

reg13 <-  glm(if_birth ~ plan_1 + plan_2 
              + plan_3 + plan_4 + plan_5 
              + plan_7 + plan_8  + DH  
             + age1519 + age2024
             + age2429 + age3034
             + age3539 + age4044
             + age4550 + FSI 
             + chnr 
             ,family=binomial(link='logit'),data=basepay)

summary(reg13)

stargazer(reg13)


reg14 <-  glm(if_birth ~ treated + DH  
              + age1519 + age2024
              + age2429 + age3034
              + age3539 + age4044
              + age4550 + FSI 
              + chnr + incbracket
              ,family=binomial(link='logit'),data=basepay)

summary(reg14)

stargazer(reg14)

reg15 <-  glm(if_birth ~ plan_1 + plan_2 
              + plan_3 + plan_4 + plan_5 
              + plan_7 + plan_8  + DH  
              + age1519 + age2024
              + age2429 + age3034
              + age3539 + age4044
              + age4550 + FSI 
              + chnr + incbracket
              ,family=binomial(link='logit'),data=basepay)

summary(reg15)

stargazer(reg15)

reg16 <-  glm(if_birth ~ treated + DH  
              + age1519 + age2024
              + age2429 + age3034
              + age3539 + age4044
              + age4550 + FSI 
              + chnr + incbracket + chout
              ,family=binomial(link='logit'),data=basepay)

summary(reg16)

stargazer(reg14)

reg17 <-  glm(if_birth ~ plan_1 + plan_2 
              + plan_3 + plan_4 + plan_5 
              + plan_7 + plan_8  + DH  
              + age1519 + age2024
              + age2429 + age3034
              + age3539 + age4044
              + age4550 + FSI 
              + chnr + incbracket + chout
              ,family=binomial(link='logit'),data=basepay)

summary(reg17)

stargazer(reg17)

saveRDS(basepay, "basepay.rds")
