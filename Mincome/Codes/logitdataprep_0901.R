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
library(compareGroups)
library("xtable")


basepay2panel <- read_dta("basepaypanel_revised.dta")
basepay2panel[basepay2panel == -9] <- NA
basepay2panel[basepay2panel == -7] <- NA
basepay2panel[basepay2panel == -1] <- NA
basepay2panel[basepay2panel == "."] <- NA

#only have Winnipeg 

basepay2panel <- basepay2panel[which(basepay2panel$SiteCode == 1),]

basepay2panel <- basepay2panel %>%
  group_by(FamNum) %>%
  arrange(month)  %>%
  mutate(prevnrch = dplyr::lag(CH, 1))  %>%
  ungroup()%>%
  arrange(FamNum, month)

basepay2panel$increase = 0 
basepay2panel$increase[basepay2panel$prevnrch < basepay2panel$CH] <- 1

#get the information on the treatment cell of the household

basepay2panel$AC <- as.character(basepay2panel$AC)
basepay2panel$plan <- substr(basepay2panel$AC, 1, 1)
basepay2panel$inc <- substr(basepay2panel$AC, 2, 3)
basepay2panel$inc <- as.factor(basepay2panel$inc)

#by eliminating the NAs, we are removing the months for each household 
#where they were not enrolled in the experiment 
basepay2panel <- basepay2panel[!is.na(basepay2panel$plan),]

#month1 will then be 1 if that is the month where the family has begun 
#the experiment
basepay2panel <- basepay2panel %>%
  group_by(FamNum) %>%
  arrange(month)  %>%
  mutate(month1 = row_number())  %>%
  ungroup()%>%
  arrange(FamNum, month)

#exclude those that experienced birth within the first nine months of the experiement 
basepay2panel$firstnine <- 0
basepay2panel$firstnine[basepay2panel$month1 < 9] <- 1

#CR and CS give the male and female householders´ age, also for households that were missing 
#at the beginning of the experiment 

basepay2panel$mage<- basepay2panel$CR
basepay2panel$fage<- basepay2panel$CS
basepay2panel$DH<- basepay2panel$CP
basepay2panel$SH<- basepay2panel$CQ
basepay2panel$individual <- 0 
basepay2panel$individual[basepay2panel$DoubleHead1== 0 & basepay2panel$SingleHead1 == 0] <- 1


faminfo <- subset(basepay2panel, select=c("FamNum","FSI", "FS", "month1", "mage", "fage", "AC"))
faminfo <- faminfo[which(faminfo$month1 ==1),]
faminfo <- faminfo %>% rename(FAMSI=FSI)
faminfo <- faminfo %>% rename(FAMS=FS)
faminfo <- faminfo %>% rename(FAGE=fage)
faminfo <- faminfo %>% rename(MAGE=mage)
faminfo <- faminfo %>% rename(asscell=AC)
faminfo <- faminfo[,-4]
#have a dummy that indicates if there has been any increases

basepay2panel$sum = 0

basepay2panel$FamNum <- as.factor(basepay2panel$FamNum)
for (i in levels(basepay2panel$FamNum)){
  s <- sum(basepay2panel[basepay2panel$FamNum == i, "increase"])
  basepay2panel$sum[basepay2panel$FamNum == i] <- s
}

basepay2panel$if_increase = 0   
basepay2panel$if_increase[basepay2panel$sum != 0] <- 1

#another dummy if the increase has been during the first nine months
basepay2panel$increase9 = basepay2panel$increase*basepay2panel$firstnine
basepay2panel$top = 0
basepay2panel$FamNum <- as.factor(basepay2panel$FamNum)
for (i in levels(basepay2panel$FamNum)){
  s <- sum(basepay2panel[basepay2panel$FamNum == i, "increase9"])
  basepay2panel$top[basepay2panel$FamNum == i] <- s
}

basepay2panel$if_increase9 = 0   
basepay2panel$if_increase9[basepay2panel$top != 0] <- 1


#Plan 6 was merged with plan 7 at some point
basepay2panel$plan[basepay2panel$plan == 6] <- 7

#have a control dummy, 0 for treated, 1 for control group 
basepay2panel$control = 0 
basepay2panel$control[basepay2panel$plan == 9] <- 1

basepay2panel$treated = 0 
basepay2panel$treated[basepay2panel$control == 0] <- 1

#there are families whose treament plan changes 
#and even whether they are in control group or not 

basepay2panel$changetreatment = 0 

basepay2panel <- basepay2panel %>%
  group_by(FamNum) %>%
  arrange(month)  %>%
  mutate(prevtreat = dplyr::lag(plan, 1))  %>%
  ungroup()%>%
  arrange(FamNum, month)

basepay2panel$changetreatment[basepay2panel$prevtreat !=basepay2panel$plan] = 1 

change <- basepay2panel[which(basepay2panel$changetreatment == 1),]

#46 families in Winnipeg, some sometimes in control and treatment  

#let's remove these families for now and then see 

basepay2panel$summ = 0

basepay2panel$FamNum <- as.factor(basepay2panel$FamNum)
for (i in levels(basepay2panel$FamNum)){
  s <- sum(basepay2panel[basepay2panel$FamNum == i, "changetreatment"])
  basepay2panel$summ[basepay2panel$FamNum == i] <- s
}

basepay2panel$if_change = 0   
basepay2panel$if_change[basepay2panel$summ != 0] <- 1

basepay2panel<-merge(basepay2panel, faminfo, by = "FamNum", all=T)

basepay2panel_rem <- basepay2panel[which(basepay2panel$AC != 0),]

basepay2panel_rem <- basepay2panel_rem[which(basepay2panel_rem$if_change == 0),]

#have it cross section 

basepay2panel_rem$plan <- as.numeric(as.character(basepay2panel_rem$plan))
basepay2panel_rem$control <- as.numeric(as.character(basepay2panel_rem$control))
basepay2panel_rem$if_increase <- as.numeric(as.character(basepay2panel_rem$if_increase))
basepay2panel_rem$if_increase9 <- as.numeric(as.character(basepay2panel_rem$if_increase9))
basepay2panel_rem$MAGE <- as.numeric(as.character(basepay2panel_rem$MAGE))
basepay2panel_rem$FAGE <- as.numeric(as.character(basepay2panel_rem$FAGE))
basepay2panel_rem$DH <- as.numeric(as.character(basepay2panel_rem$DH))
basepay2panel_rem$individual <- as.numeric(as.character(basepay2panel_rem$individual))
basepay2panel_rem$SH <- as.numeric(as.character(basepay2panel_rem$SH))
basepay2panel_rem$asscell <- as.numeric(as.character(basepay2panel_rem$asscell))
basepay2panel_rem$asscell <- as.numeric(as.character(basepay2panel_rem$asscell))


basepay2 <- basepay2panel_rem %>%
  group_by(FamNum)%>%
  summarise(if_birth=mean(if_increase), if_birth9=mean(if_increase9), control = mean(control), plan =mean(plan), 
            MAGE = mean(MAGE), FAGE = mean(FAGE), DH = mean(DH), individual = mean(individual), 
            SH =mean(SH), AC = mean(asscell)) %>% 
  ungroup()


#we want information on number of children, family size index etc. 
#we can get this info from the FSI, NUMCH from the first month households join 
bpinfo <- basepay2panel_rem %>%
  group_by(FamNum) %>%
  filter(month1 == 1) %>%
  ungroup()

bpinfo <- bpinfo %>%
  dplyr::select(FamSize, FamSizex100, FamNum, NumAdults, NumChild)

basepay2 <- merge(basepay2, bpinfo, by = "FamNum", all = TRUE)
basepay2$treated <- 0 
basepay2$treated[basepay2$control == 0] <- 1

#if we want to exclude households with only males 
#basepay2 <- basepay2[-which(is.na(basepay2$FAGE)), ]

names(basepay2)[names(basepay2) == 'FAGE'] <- "age"
basepay2 <- fastDummies::dummy_columns(basepay2, select_columns = "age")
basepay2 <- fastDummies::dummy_columns(basepay2, select_columns = "plan")


basepay2$age1519 <- 0
basepay2$age1519[basepay2$age == 15 | basepay2$age == 16 | basepay2$age == 17 |
                  basepay2$age == 18 | basepay2$age == 19  ] <- 1


basepay2$age2024 <- 0 
basepay2$age2024[basepay2$age == 20 | basepay2$age == 21 | basepay2$age == 22 |
                  basepay2$age == 23 | basepay2$age == 24  ] <- 1


basepay2$age2429 <- 0 
basepay2$age2429[basepay2$age == 24 | basepay2$age == 25 | basepay2$age == 26 |
                  basepay2$age == 27 | basepay2$age == 28  ] <- 1

basepay2$age3034 <- 0 
basepay2$age3034[basepay2$age == 30 | basepay2$age == 31 | basepay2$age == 32 |
                  basepay2$age == 33 | basepay2$age == 34  ] <- 1



basepay2$age3539 <- 0
basepay2$age3539[basepay2$age == 35 | basepay2$age == 36 | basepay2$age == 37 |
                  basepay2$age == 38 | basepay2$age == 39  ] <- 1

basepay2$age4044 <- 0 
basepay2$age4044[basepay2$age == 40 | basepay2$age == 41 | basepay2$age == 42 |
                  basepay2$age == 43 | basepay2$age == 44  ] <- 1 


basepay2$age4550 <- 0 
basepay2$age4550[basepay2$age == 45 | basepay2$age == 46 | basepay2$age == 47 |
                  basepay2$age == 48 | basepay2$age == 49  ] <- 1 

basepay2$age50plus <- 0 
basepay2$age50plus[basepay2$age > 50 ] <- 1 
names(basepay2)[names(basepay2) == 'FamSizex100'] <- "FSI"
names(basepay2)[names(basepay2) == 'TotFamInc74'] <- "TotInc74"

basepay2$TotInc74 <- as.numeric(basepay2$TotInc74)
basepay2$age <- as.factor(basepay2$age)
basepay2$rate <- 0
basepay2$rate[basepay2$plan == 1 |basepay2$plan == 2 ] <- 35
basepay2$rate[basepay2$plan == 3 |basepay2$plan == 4 | basepay2$plan == 5] <- 50
basepay2$rate[basepay2$plan == 7 |basepay2$plan == 28 ] <- 75


basepay2$guarantee <- 0
basepay2$guarantee[basepay2$plan == 1 |basepay2$plan == 3 ] <- 3800
basepay2$guarantee[basepay2$plan == 2 |basepay2$plan == 4 | basepay2$plan == 7] <- 4800
basepay2$guarantee[basepay2$plan == 5 |basepay2$plan == 8 ] <- 5480

familydata <- read_excel("familydata.xlsx")

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

basepay2 <- stata.merge(familydata, basepay2, by = "FamNum")
basepay2 <- basepay2[-which(basepay2$merge == "master"), ]
basepay2[basepay2 == -9] <- NA
basepay2$chout[is.na(basepay2$chout)] <- 0

basepay2$AC <- as.character(basepay2$AC)
basepay2$incbracket <- substr(basepay2$AC, 2, 3)
basepay2$incbracket <- as.factor(basepay2$incbracket)


length(which(basepay2$plan == 1))
length(which(basepay2$plan == 2))
length(which(basepay2$plan == 3))
length(which(basepay2$plan == 4))
length(which(basepay2$plan == 5))
length(which(basepay2$plan == 7))
length(which(basepay2$plan == 8))
length(which(basepay2$plan == 9))

nrplans <- matrix(c(41,57,42,59,48,52,40,101),ncol=8,byrow=TRUE)
colnames(nrplans) <- c("Plan 1","Plan 2","Plan 3", "Plan 4","Plan 5","Plan 7", "Plan 8","Control")
rownames(nrplans) <- c("Number of households")
print(xtable(nrplans, type = "latex"), file = "nrplans.tex")


base_payrev[base_payrev == -9] <- NA
base_payrev[base_payrev == -7] <- NA
base_payrev[base_payrev == -1] <- NA
base_payrev[base_payrev == "."] <- NA

base_payrev <- base_payrev  %>%
  dplyr::select(FAMNUM, highschf, highschm, yrschm, yrschf)
basepay2$FAMNUM <- basepay2$FamNum
basepay2 <- merge(base_payrev, basepay2, by = "FAMNUM")
basepay2$highschf <- as.factor(basepay2$highschf)
basepay2$highschm <- as.factor(basepay2$highschm)
basepay2$yrschm <- as.numeric(basepay2$yrschm)
basepay2$yrschf <- as.numeric(basepay2$yrschf)

basepay2$malepr <- 0
basepay2$malepr[!is.na(basepay2$MAGE)] <- 1
basepay2$malepr <- as.factor(basepay2$malepr)
basepay2 <- basepay2[-which(is.na(basepay2$age)), ]

saveRDS(basepay2, "basepay2.rds")


check <- subset(basepay2, select=c("FamNum","malepr", "DH"))
check
