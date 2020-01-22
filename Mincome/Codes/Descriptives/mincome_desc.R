getwd()

install.packages("compareGroups")
install.packages("data.table")
install.packages("gtools")
install.packages("haven")
install.packages("dplyr")
install.packages("tidyr") 
install.packages("tidyverse")
install.packages("lubridate") 
install.packages("data.table")
install.packages("foreign")
install.packages("quantmod") 
install.packages("zoo")
install.packages("plm")
install.packages("gplots")
install.packages("stargazer")
install.packages("lfe")
install.packages("Hmisc")
install.packages("readxl")
install.packages("naniar")
install.packages("strex")
install.packages(devtools)
install.packages("tidyselect")
install.packages("backports")
install.packages("survival")

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

#Data Preparation

#let's look at the increases in numbers across sites and treatment groups 
#increases in the number of children 
basepaypanel <- read_dta("W:/WU/Projekte/mincome/Mincome/Data/basepaypanel.dta")
basepaypanel[basepaypanel == -9] <- NA
basepaypanel[basepaypanel == -7] <- NA
basepaypanel[basepaypanel == -1] <- NA
basepaypanel[basepaypanel == "."] <- NA
basepaypanel$individual <- 0 
basepaypanel$individual[basepaypanel$DH == 0 & basepaypanel$SH == 0] <- 1

#compute the number of children from previous period to see 
#if there has been an increase 

basepaypanel <- basepaypanel %>%
  group_by(FAMNUM) %>%
  arrange(month)  %>%
  mutate(prevnrch = dplyr::lag(CH, 1))  %>%
  ungroup()%>%
  arrange(FAMNUM, month)

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
  group_by(FAMNUM) %>%
  arrange(month)  %>%
  mutate(month1 = row_number())  %>%
  ungroup()%>%
  arrange(FAMNUM, month)


#we have 128 increases in the number of children overall 

#for some families, family size, ages of householders are missing because they were not 
#in the baseline survey
#we take the values for these variables in the first month they were enrolled 
#and merge it back to the panel data

faminfo <- subset(basepaypanel, select=c("FAMNUM","FSI", "FS", "month1", "fage", "mage"))
faminfo <- faminfo[which(faminfo$month1 ==1),]
faminfo <- faminfo %>% rename(FAMSI=FSI)
faminfo <- faminfo %>% rename(FAMS=FS)
faminfo <- faminfo %>% rename(FAGE=fage)
faminfo <- faminfo %>% rename(MAGE=mage)

faminfo <- faminfo[,-4]

#we need to transform it back to cross section 
#have a dummy that indicates if there has been any increases

basepaypanel$sum = 0

basepaypanel$FAMNUM <- as.factor(basepaypanel$FAMNUM)
for (i in levels(basepaypanel$FAMNUM)){
  s <- sum(basepaypanel[basepaypanel$FAMNUM == i, "increase"])
basepaypanel$sum[basepaypanel$FAMNUM == i] <- s
}

basepaypanel$if_increase = 0   
basepaypanel$if_increase[basepaypanel$sum != 0] <- 1

#Plan 6 was merged with plan 7 at some point
basepaypanel$plan[basepaypanel$plan == 6] <- 7

#have a control dummy, 0 for treated, 1 for control group 
basepaypanel$control = 0 
basepaypanel$control[basepaypanel$plan == 9] <- 1


#there are families whose treament plan changes 
#and even whether they are in control group or not 
basepaypanel$changetreatment = 0 

basepaypanel <- basepaypanel %>%
  group_by(FAMNUM) %>%
  arrange(month)  %>%
  mutate(prevtreat = dplyr::lag(plan, 1))  %>%
  ungroup()%>%
  arrange(FAMNUM, month)

basepaypanel$changetreatment[basepaypanel$prevtreat !=basepaypanel$plan] = 1 

change <- basepaypanel[which(basepaypanel$changetreatment == 1),]

#152 families 
#family 1768 was on plan 6, was changed to 7 in month 19
#and then to 9 in month 20, and then back to plan 7 month 28 

#2662 first two months control group

#let's remove these families for now and then see 

basepaypanel$summ = 0

basepaypanel$FAMNUM <- as.factor(basepaypanel$FAMNUM)
for (i in levels(basepaypanel$FAMNUM)){
  s <- sum(basepaypanel[basepaypanel$FAMNUM == i, "changetreatment"])
  basepaypanel$summ[basepaypanel$FAMNUM == i] <- s
}

basepaypanel$if_change = 0   
basepaypanel$if_change[basepaypanel$summ != 0] <- 1

basepaypanel<-merge(basepaypanel, faminfo, by = "FAMNUM", all=T)

basepaypanel_rem <- basepaypanel[which(basepaypanel$AC != 0),]

basepaypanel_rem <- basepaypanel_rem[which(basepaypanel_rem$if_change == 0),]

#do everything numeric so to get their means 
basepaypanel$plan = as.numeric(as.character(basepaypanel$plan))
basepaypanel$Winni <- as.numeric(as.character(basepaypanel$Winni))
basepaypanel$Dauphin <- as.numeric(as.character(basepaypanel$Dauphin))
basepaypanel$DH <- as.numeric(as.character(basepaypanel$DH))
basepaypanel$SH <- as.numeric(as.character(basepaypanel$SH))
basepaypanel$mage <- as.numeric(as.character(basepaypanel$FAGE))
basepaypanel$fage <- as.numeric(as.character(basepaypanel$MAGE))
basepaypanel$TOTNHINC73 <- as.numeric(as.character(basepaypanel$TOTNHINC73))
basepaypanel$TOTNHINC74 <- as.numeric(as.character(basepaypanel$TOTNHINC74))
basepaypanel$W <- as.numeric(as.character(basepaypanel$W))
basepaypanel$TRN <- as.numeric(as.character(basepaypanel$TRN))
basepaypanel$FAMSI  <- as.numeric(as.character(basepaypanel$FAMSI))
basepaypanel$FAMS <- as.numeric(as.character(basepaypanel$FAMS))  

basepaypanel_rem$plan = as.numeric(as.character(basepaypanel_rem$plan))
basepaypanel_rem$Winni <- as.numeric(as.character(basepaypanel_rem$Winni))
basepaypanel_rem$Dauphin <- as.numeric(as.character(basepaypanel_rem$Dauphin))
basepaypanel_rem$DH <- as.numeric(as.character(basepaypanel_rem$DH))
basepaypanel_rem$SH <- as.numeric(as.character(basepaypanel_rem$SH))
basepaypanel_rem$FAGE <- as.numeric(as.character(basepaypanel_rem$FAGE))
basepaypanel_rem$MAGE <- as.numeric(as.character(basepaypanel_rem$MAGE))
basepaypanel_rem$TOTNHINC73 <- as.numeric(as.character(basepaypanel_rem$TOTNHINC73))
basepaypanel_rem$TOTNHINC74 <- as.numeric(as.character(basepaypanel_rem$TOTNHINC74))
basepaypanel_rem$W <- as.numeric(as.character(basepaypanel_rem$W))
basepaypanel_rem$TRN <- as.numeric(as.character(basepaypanel_rem$TRN))
basepaypanel_rem$FAMS  <- as.numeric(as.character(basepaypanel_rem$FAMS))
basepaypanel_rem$FAMSI <- as.numeric(as.character(basepaypanel_rem$FAMSI))  


basepaycross_rem <- basepaypanel_rem %>%
  group_by(FAMNUM)%>%
  select(FAMNUM, if_increase, plan, Winni,Dauphin,  DH, SH, individual, control,
         FAGE, MAGE, TOTNHINC73, TOTNHINC74, FAMS, FAMSI, W, TRN) %>%
  summarise(if_increase=mean(if_increase), 
            plan = mean(plan), Winni = mean(Winni), 
            Dauphin = mean(Dauphin), DH = mean(DH),
            SH = mean(SH), individual = mean(individual), control = mean(control),
            FAGE = mean(FAGE), MAGE = mean(MAGE), TOTNHINC73 = mean(TOTNHINC73),
            TOTNHINC74 = mean(TOTNHINC74), 
            meanwage = mean(W), meaninc = mean(TRN),
              FAMS = mean(FAMS), FAMSI = mean(FAMSI))

basepaycross_rem$plan <- as.factor(basepaycross_rem$plan)
basepaycross_rem$control <- as.factor(basepaycross_rem$control)
basepaycross_rem$if_increase <- as.factor(basepaycross_rem$if_increase)
basepaycross_rem$Winni <- as.factor(basepaycross_rem$Winni)
basepaycross_rem$Dauphin <- as.factor(basepaycross_rem$Dauphin)
basepaycross_rem$DH <- as.factor(basepaycross_rem$DH)
basepaycross_rem$SH <- as.factor(basepaycross_rem$SH)
basepaycross_rem$individual <- as.factor(basepaycross_rem$individual)



basepaypanel_winni <- basepaypanel[which(basepaypanel$SITECODE == 1),]
saveRDS(basepaypanel_winni, "basepaypanel_winni.rds")

#family data to calculate the age where women have kids 

famdata_ind <- read_dta("W:/WU/Projekte/mincome/Mincome/Data/famdata_ind.dta")
famdata_ind[famdata_ind == -4] <- NA
famdata_ind <- famdata_ind[, c(1:6)]
famdata_ind <- famdata_ind[!(is.na(famdata_ind$OID)), ]
famdata_ind <- famdata_ind[famdata_ind$RTH == 2 | famdata_ind$RTH == 4 | famdata_ind$RTH == 5, ]

famdata_ind$BIRTH <- as.character(famdata_ind$BIRTH)
famdata_ind$birthyear <- substr(famdata_ind$BIRTH, 1, 2)
famdata_ind$birthday <- substr(famdata_ind$BIRTH, 3, 6)

famdata_ind$birthyear <- as.numeric(famdata_ind$birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(BIRTH))  %>%
  mutate(age_firstch = dplyr::lag(birthyear, 1) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, BIRTH)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(BIRTH))  %>%
  mutate(age_firstch = dplyr::lag(birthyear, 1) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, BIRTH)
famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(BIRTH))  %>%
  mutate(age_secch = dplyr::lag(birthyear, 2) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, BIRTH)
famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(BIRTH))  %>%
  mutate(age_thirdch = dplyr::lag(birthyear, 3) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, BIRTH)
famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(BIRTH))  %>%
  mutate(age_fourthch = dplyr::lag(birthyear, 4) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, BIRTH)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(BIRTH))  %>%
  mutate(age_fifthch = dplyr::lag(birthyear, 5) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, BIRTH)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(BIRTH))  %>%
  mutate(age_sixthch = dplyr::lag(birthyear, 6) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, BIRTH)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(BIRTH))  %>%
  mutate(age_seventhch = dplyr::lag(birthyear, 7) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, BIRTH)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(BIRTH))  %>%
  mutate(age_eigthch = dplyr::lag(birthyear, 8) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, BIRTH)


famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(BIRTH))  %>%
  mutate(age_ninthch = dplyr::lag(birthyear, 9) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, BIRTH)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(BIRTH))  %>%
  mutate(age_tenthch = dplyr::lag(birthyear, 10) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, BIRTH)

famdata_ind <- famdata_ind[famdata_ind$RTH == 2, ]

age_femid15 <- subset(famdata_ind, select=c("FAMNUM","OID"))
age_femid15$age <- 15 
age_femid50 <- subset(famdata_ind, select=c("FAMNUM","OID"))
age_femid50$age <- 50 

age_femid <- rbind(age_femid15, age_femid50)

age_femid <- age_femid %>% dplyr::group_by(FAMNUM, OID) %>%
  mutate(age = as.numeric(age)) %>%
  complete(age = 1:max(age), fill = list())

famdata_ind<-merge(famdata_ind, age_femid, by = c("FAMNUM","OID"), all=T)

famdata_ind <- famdata_ind[famdata_ind$age > 14, ]

famdata_ind$event <- 0

famdata_ind$event[famdata_ind$age_firstch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_secch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_thirdch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_fourthch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_fifthch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_sixthch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_seventhch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_eigthch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_ninthch == famdata_ind$age] <- 1
famdata_ind$event[famdata_ind$age_tenthch == famdata_ind$age] <- 1

famdata_ind$year = famdata_ind$birthyear + famdata_ind$age
famdata_ind$year <- paste("19", famdata_ind$year, sep="")



length(which(famdata_ind$event == 1 & famdata_ind$age == 15))
length(which(famdata_ind$event == 1 & famdata_ind$age == 16))
length(which(famdata_ind$event == 1 & famdata_ind$age == 17))
length(which(famdata_ind$event == 1 & famdata_ind$age == 18))
length(which(famdata_ind$event == 1 & famdata_ind$age == 19))
length(which(famdata_ind$event == 1 & famdata_ind$age == 20))
length(which(famdata_ind$event == 1 & famdata_ind$age == 21))
length(which(famdata_ind$event == 1 & famdata_ind$age == 22))
length(which(famdata_ind$event == 1 & famdata_ind$age == 23))
length(which(famdata_ind$event == 1 & famdata_ind$age == 24))
length(which(famdata_ind$event == 1 & famdata_ind$age == 25))
length(which(famdata_ind$event == 1 & famdata_ind$age == 26))
length(which(famdata_ind$event == 1 & famdata_ind$age == 27))
length(which(famdata_ind$event == 1 & famdata_ind$age == 28))
length(which(famdata_ind$event == 1 & famdata_ind$age == 29))
length(which(famdata_ind$event == 1 & famdata_ind$age == 30))
length(which(famdata_ind$event == 1 & famdata_ind$age == 31))
length(which(famdata_ind$event == 1 & famdata_ind$age == 32))
length(which(famdata_ind$event == 1 & famdata_ind$age == 33))
length(which(famdata_ind$event == 1 & famdata_ind$age == 34))
length(which(famdata_ind$event == 1 & famdata_ind$age == 35))
length(which(famdata_ind$event == 1 & famdata_ind$age == 36))
length(which(famdata_ind$event == 1 & famdata_ind$age == 37))
length(which(famdata_ind$event == 1 & famdata_ind$age == 38))
length(which(famdata_ind$event == 1 & famdata_ind$age == 39))
length(which(famdata_ind$event == 1 & famdata_ind$age == 40))
length(which(famdata_ind$event == 1 & famdata_ind$age == 41))
length(which(famdata_ind$event == 1 & famdata_ind$age == 42))
length(which(famdata_ind$event == 1 & famdata_ind$age == 43))
length(which(famdata_ind$event == 1 & famdata_ind$age == 44))
length(which(famdata_ind$event == 1 & famdata_ind$age == 45))
length(which(famdata_ind$event == 1 & famdata_ind$age == 46))
length(which(famdata_ind$event == 1 & famdata_ind$age == 47))
length(which(famdata_ind$event == 1 & famdata_ind$age == 48))
length(which(famdata_ind$event == 1 & famdata_ind$age == 49))
length(which(famdata_ind$event == 1 & famdata_ind$age == 50))

#here we have the births that happened before the experiment start as well 
#as those that were recorded. we need to manually add the increases in the number of children 
#that we computed using baseline payments data but that were absent from the family information data 

basepaypanel_winni$year <- 0 
basepaypanel_winni$year[basepaypanel_winni$month < 12] <- 1975
basepaypanel_winni$year[basepaypanel_winni$month >= 12 & basepaypanel_winni$month < 24] <- 1976
basepaypanel_winni$year[basepaypanel_winni$month >= 24 & basepaypanel_winni$month <= 37] <- 1977


saveRDS(famdata_ind, file = "famdata_ind.rds")



