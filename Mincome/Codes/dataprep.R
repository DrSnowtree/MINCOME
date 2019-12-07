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


faminfo <- subset(basepaypanel, select=c("FAMNUM","FSI", "FS", "month1", "fage", "mage"))
faminfo <- faminfo[which(faminfo$month1 ==1),]
faminfo <- faminfo %>% rename(FAMSI=FSI)
faminfo <- faminfo %>% rename(FAMS=FS)
faminfo <- faminfo %>% rename(FAGE=fage)
faminfo <- faminfo %>% rename(MAGE=mage)

faminfo <- faminfo[,-4]
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

basepaypanel <- basepaypanel %>%
  group_by(FAMNUM) %>%
  arrange(month)  %>%
  mutate(prevtreat = dplyr::lag(plan, 1))  %>%
  ungroup()%>%
  arrange(FAMNUM, month)

basepaypanel$changetreatment[basepaypanel$prevtreat !=basepaypanel$plan] = 1 

basepaypanel_rem <- basepaypanel[which(basepaypanel$AC != 0),]

basepaypanel_rem <- basepaypanel_rem[which(basepaypanel_rem$if_change == 0),]

basepaypanel_winni <- basepaypanel_rem[which(basepaypanel_rem$SITECODE == 1),]

basepaypanel_winni$year <- 0 
basepaypanel_winni$year[basepaypanel_winni$month < 12] <- 1975
basepaypanel_winni$year[basepaypanel_winni$month >= 12 & basepaypanel_winni$month < 24] <- 1976
basepaypanel_winni$year[basepaypanel_winni$month >= 24 & basepaypanel_winni$month <= 37] <- 1977
saveRDS(basepaypanel_winni, "basepaypanel_winni.rds")

#organising the data in women´s fertility histories with each row corresponding to a year between ages 15 and 50 
famdata_ind <- read_dta("W:/WU/Projekte/mincome/Mincome/Data/famdata_ind.dta")
famdata_ind[famdata_ind == -4] <- NA
famdata_ind <- famdata_ind[, c(1:6)]
famdata_ind <- famdata_ind[!(is.na(famdata_ind$OID)), ]
famdata_ind <- famdata_ind[famdata_ind$
                             RTH == 2 | famdata_ind$RTH == 4 | famdata_ind$RTH == 5, ]

famdata_ind$BIRTH <- as.character(famdata_ind$BIRTH)
famdata_ind$birthyear <- substr(famdata_ind$BIRTH, 1, 2)
famdata_ind$birthday <- substr(famdata_ind$BIRTH, 3, 6)
famdata_ind$birthyear <- paste("19", famdata_ind$birthyear, sep="")


famdata_ind$birthyear <- as.numeric(famdata_ind$birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_firstch = dplyr::lag(birthyear, 1) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_firstch = dplyr::lag(birthyear, 1) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, BIRTH)
famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_secch = dplyr::lag(birthyear, 2) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, birthyear)
famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_thirdch = dplyr::lag(birthyear, 3) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, birthyear)
famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_fourthch = dplyr::lag(birthyear, 4) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_fifthch = dplyr::lag(birthyear, 5) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_sixthch = dplyr::lag(birthyear, 6) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_seventhch = dplyr::lag(birthyear, 7) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_eigthch = dplyr::lag(birthyear, 8) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_ninthch = dplyr::lag(birthyear, 9) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, birthyear)

famdata_ind <- famdata_ind%>%
  group_by(FAMNUM) %>%
  arrange(desc(birthyear))  %>%
  mutate(age_tenthch = dplyr::lag(birthyear, 10) - birthyear)  %>%
  ungroup()%>%
  arrange(FAMNUM, birthyear)

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

#here we have the births that happened before the experiment start as well 
#as those that were recorded. we need to manually add the increases in the number of children 
#that we computed using baseline 
#payments data but that were absent from the family information data 

#information on basepay panel is monthly and we want to see if there has been an increase in that given year 

basepaypanel_winni$FAMNUM <- as.factor(basepaypanel_winni$FAMNUM)
basepaypanel_winni$year <- as.factor(basepaypanel_winni$year)
basepaypanel_winni$increase <- as.numeric(basepaypanel_winni$increase)

for (i in levels(basepaypanel_winni$FAMNUM)){
    for (j in levels(basepaypanel_winni$year)){
  s <- sum(basepaypanel_winni[basepaypanel_winni$FAMNUM == i & basepaypanel_winni$year == j, "increase"])
  basepaypanel_winni$summm[basepaypanel_winni$FAMNUM == i & basepaypanel_winni$year == j] <- s
    }
}

basepaypanel_winni$if_birth = 0   
basepaypanel_winni$if_birth[basepaypanel_winni$summm != 0] <- 1

basepaypanel_winni$plan <- as.numeric(as.character(basepaypanel_winni$plan))
basepaypanel_winni$control <- as.numeric(as.character(basepaypanel_winni$control))

basepaypanel_winni_year <- basepaypanel_winni %>%
  group_by(FAMNUM, year)%>%
  select(FAMNUM, if_birth, year, control, plan) %>%
  summarise(if_birth=mean(if_birth), control = mean(control), plan =mean(plan))

class(basepaypanel_winni_year$FAMNUM)
famdata_ind$FAMNUM <- as.factor(as.character(famdata_ind$FAMNUM))

famdata_ind$year = famdata_ind$birthyear + famdata_ind$age
famdata_ind <- merge(basepaypanel_winni_year, famdata_ind, by =c("FAMNUM", "year"), all=T)
famdata_ind$year = famdata_ind$birthyear + famdata_ind$age

famdata_ind$event[famdata_ind$if_birth == 1] <- 1
famdata_ind <- famdata_ind%>%
  arrange(OID, age) 

#for all women who reach the age 50 by the end of experiment, we have the full birth history
#for women who are under 50 by the end of experiment, the years after the experiment are censored 

famdata_ind$censored <-0
famdata_ind$censored[famdata_ind$year > 1977] <- 1
length(which(famdata_ind$event == 1))


#have the control variable be 1 or 0 for all years 
famdata_ind$OID <- as.factor(famdata_ind$OID)
famdata_ind$control <- as.numeric(famdata_ind$control)

for (i in levels(famdata_ind$FAMNUM)){
  s <- sum(famdata_ind[famdata_ind$FAMNUM == i, "control"], na.rm=TRUE)
  famdata_ind$sumcontrol[famdata_ind$FAMNUM == i] <- s
}

famdata_ind$treated = 0   
famdata_ind$treated[famdata_ind$sumcontrol != 0] <- 1

#now we want treated to be 1 only during the experiment, so we create an experiment dummy that takes the value of 1 
#during 1975, 1976, 1977, which we then interact with treated 
famdata_ind$experiment = 0 
famdata_ind$experiment[famdata_ind$year == 1975 | famdata_ind$year == 1976 | famdata_ind$year == 1977] = 1 
famdata_ind$treated_exp = famdata_ind$experiment*famdata_ind$treated

saveRDS(famdata_ind, "famdata_ind.rds")

#remove censored observations 

famdata_ind_nocens <- famdata_ind[famdata_ind$censored == 0, ]

#add intervals now 
famdata_ind$int1 = famdata_ind$age_firstch - 15 




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

