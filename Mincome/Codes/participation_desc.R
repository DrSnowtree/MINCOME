#code to compare the households who dropped out within the first two years 
#and those that continued

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

#download data
base_pay <- read_excel("Downloads/base_pay.data_revised_Dec 11, 2019.xlsx")
View(base_pay)
baseline <- read_excel("Downloads/baseline.data_labeled.xlsx")

#households that are in baseline data but not in baseline_pay are the ones for whom we don't have 
#payment info so they dropped out within the first two years 

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


base_pay %>% 
  rename(
    FAMNUM = `Fam Num`,
    HHEAD = `Double Head = 1...94`, 
    SHEAD = `Single Head = 1...95`
    AGEM = `Age Male Head...96`
    AGEF = `Age Female Head...97`
    FAMSIZE = FSI37
    NUMADULTS = 
    
  )

mergedbasepay <- stata.merge(base_pay,baseline, by = c("FAMNUM", "HHEAD", "SHEAD",
                                                       "AGEM", "AGEF", "FAMSIZE", "NUMADULTS", "NUMCHILD"
                                                       , "FAMSIZEI", "HMEOWNER", "TREAT", "TOTNHINC74",
                                                       "TOTNHINC73", "TOTFAMINC74", "MHGROSSEARN", "FHGROSSEARN"))


#matched are the ones that have continued for at least two years, using the ones that dropped out within 
#two years, master the ones that joined after (probably supplemental sample)

#we can compare the matched and the using to see if they are fundamentally different

mergedbasepay1 <- dplyr::filter(mergedbasepay, merge == "matched" | merge == "using") 
mergedbasepay1$enrolled <- 0
mergedbasepay1$enrolled[mergedbasepay1$merge == "matched"] <- 1
mergedbasepay1[mergedbasepay1 == -9] <- NA
mergedbasepay1[mergedbasepay1 == -7] <- NA

mergedbasepay1 <- mergedbasepay1[-c(2174, 2175, 2176, 2177),] 
mergedbasepay1$individual <- 0 
mergedbasepay1$individual[mergedbasepay1$HHEAD == 0 & mergedbasepay1$SHEAD == 0] <- 1

mergedbasepay1$MHNOTLOOK.y <- as.factor(mergedbasepay1$MHNOTLOOK.y)
mergedbasepay1$MHLABPART.y <- as.factor(mergedbasepay1$MHLABPART.y)
mergedbasepay1$MHJOBSAT.y <- as.factor(mergedbasepay1$MHJOBSAT.y)
mergedbasepay1$MH_SCH.y <- as.factor(mergedbasepay1$MH_SCH.y)
mergedbasepay1$MHJOBS74.y <- as.factor(mergedbasepay1$MHJOBS74.y)

mergedbasepay1$FHNOTLOOK.y <- as.factor(mergedbasepay1$FHNOTLOOK.y)
mergedbasepay1$FHLABPART.y <- as.factor(mergedbasepay1$FHLABPART.y)
mergedbasepay1$FHJOBSAT.y <- as.factor(mergedbasepay1$FHJOBSAT.y)
mergedbasepay1$FH_SCH.y <- as.factor(mergedbasepay1$FH_SCH.y)
mergedbasepay1$AGEF <- as.numeric(mergedbasepay1$AGEF)
mergedbasepay1$AGEM <- as.numeric(mergedbasepay1$AGEM)

mergedbasepay1$FHTOTERN73.y <- as.numeric(mergedbasepay1$FHTOTERN73.y)
mergedbasepay1$FHTOTERN74.y <- as.numeric(mergedbasepay1$FHTOTERN74.y)
mergedbasepay1$FHHOURS.y <- as.numeric(mergedbasepay1$FHHOURS.y)
mergedbasepay1$FHYRSSCHL.y <- as.numeric(mergedbasepay1$FHYRSSCHL.y)
mergedbasepay1$FHCHDCARE.y <- as.numeric(mergedbasepay1$FHCHDCARE.y)
mergedbasepay1$FHWEEK74.y <- as.numeric(mergedbasepay1$FHWEEK74.y) 
mergedbasepay1$FHWEEKS73.y <- as.numeric(mergedbasepay1$FHWEEKS73.y) 
mergedbasepay1$FAMSIZEI <- as.numeric(mergedbasepay1$FAMSIZEI) 
mergedbasepay1$FAMSIZE <- as.numeric(mergedbasepay1$FAMSIZE) 
mergedbasepay1$TOTFAMINC74 <- as.numeric(mergedbasepay1$TOTFAMINC74)

label(mergedbasepay1$HHEAD) <- "Double-Headed Household"
label(mergedbasepay1$SHEAD) <- "Single-Headed Household"
label(mergedbasepay1$individual) <- "Single Individual"
label(mergedbasepay1$NUMADULTS) <- "Number of Adults in the Household"
label(mergedbasepay1$NUMCHILD) <- "Number of Children in the Household"
label(mergedbasepay1$HMEOWNER) <- "Homeowner Dummy"
label(mergedbasepay1$TREAT) <- "Assigned Treatment Group"
label(mergedbasepay1$enrolled) <- "Enrolled and Continued"
label(mergedbasepay1$LIQASSETS.y) <- "Liquid Assets"
label(mergedbasepay1$DEBTS.y) <- "Debt"
label(mergedbasepay1$MHLABPART.y) <- "Labor Participation (Male Head)"
label(mergedbasepay1$MHNOTLOOK.y) <- "Reason why male head did not look for work"
label(mergedbasepay1$MHHOURS.y) <- "Number of Hours Worked Past Week (Male Head)"
label(mergedbasepay1$MHTOTERN74.y) <- "Total Earnings in 1974 (Male Head)"
label(mergedbasepay1$MHTOTERN73.y) <- "Rotal Earnings in 1973 (Male Head)"
label(mergedbasepay1$MHJOBSAT.y) <- "Job Satisfaction Index (Male Head)"
label(mergedbasepay1$MHCHDCARE.y) <- "Expected or actual childcare cost (Male head)"
label(mergedbasepay1$MH_SCH.y) <- "Completed high school (Male head)"
label(mergedbasepay1$FHLABPART.y) <- "Labor Participation (Female Head)"
label(mergedbasepay1$FHNOTLOOK.y) <- "Reason why female head did not look for work"
label(mergedbasepay1$FHHOURS.y) <- "Number of Hours Worked Past Week (Female Head)"
label(mergedbasepay1$FHTOTERN74.y) <- "Total Earnings in 1974 (Female Head)"
label(mergedbasepay1$FHTOTERN73.y) <- "Total Earnings in 1973 (Female Head)"
label(mergedbasepay1$FHJOBSAT.y) <- "Job Satisfaction Index (Female Head)"
label(mergedbasepay1$FHCHDCARE.y) <- "Expected or actual childcare cost (Female head)"
label(mergedbasepay1$FH_SCH.y) <- "Completed high school (Female head)"
label(mergedbasepay1$FHYRSSCHL.y) <- "Years of schooling (Female head)"
label(mergedbasepay1$FAMSIZE) <- "Family Size"
label(mergedbasepay1$FAMSIZEI) <- "Family Size Index"
label(mergedbasepay1$TOTFAMINC74) <- "Total Family Income in 1974"

compare = compareGroups::compareGroups(enrolled ~ .,
                                       data=mergedbasepay1,  method = NA)

compare_table <-  compareGroups::createTable(compare)
print(compare_table)
export2latex(createTable(compare))
