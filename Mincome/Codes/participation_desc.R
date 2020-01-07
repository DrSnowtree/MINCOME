library(readxl)
library("haven")
library("dplyr")
library("tidyr") 
library("tidyverse")
library("lubridate") 
library("data.table")
library("foreign")
library("haven")
library("gplots")
library("stargazer")
library("Hmisc")



base_payrev <- read_excel("W:/WU/Projekte/mincome/Mincome/Data/base_pay.data_revised_Dec 11, 2019.xlsx")
View(base_payrev)
baseline <- read_excel("W:/WU/Projekte/mincome/Mincome/Data/baseline.xlsx")
View(baseline)
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



base_payrev$FAMNUM <- base_payrev$`Fam Num`


nonpart <- stata.merge(base_payrev, baseline, by = "FAMNUM")

nonpart <- nonpart %>% 
  filter(`Site Code` == 1|`SITE CODE` == 1) 


nonpart$part <- 0 
nonpart$part[nonpart$merge == "matched"] <- 1 
nonpart$AGEF <- as.numeric(nonpart$AGEF)
nonpart[nonpart == -9] <- NA
nonpart[nonpart == -7] <- NA
nonpart[nonpart == -1] <- NA
nonpart[nonpart == "."] <- NA
nonpart$individual <- 0 
nonpart$individual[nonpart$HHEAD== 0 & nonpart$SHEAD == 0] <- 1


nonpart$HHEAD <- as.factor(nonpart$HHEAD)
nonpart$SHEAD <- as.factor(nonpart$SHEAD)
nonpart$individual <- as.character(nonpart$individual)
nonpart$TOTFAMINC74 <- as.numeric(nonpart$TOTFAMINC74)
nonpart$MHYRSSCHL <- as.numeric(nonpart$MHYRSSCHL)
nonpart$FHYRSSCHL <- as.numeric(nonpart$FHYRSSCHL)
nonpart$MH_SCH <- as.factor(nonpart$MH_SCH)
nonpart$FH_SCH <- as.factor(nonpart$FH_SCH)
nonpart$MHLABPART <- as.factor(nonpart$MHLABPART)
nonpart$MHJOBSAT <- as.factor(nonpart$MHJOBSAT)
nonpart$FHLABPART <- as.factor(nonpart$FHLABPART)
nonpart$FHJOBSAT <- as.factor(nonpart$FHJOBSAT)
nonpart$MHCHDCARE <- as.numeric(nonpart$MHCHDCARE)
nonpart$FHCHDCARE <- as.numeric(nonpart$FHCHDCARE)
nonpart$NUMCHILD <- as.factor(nonpart$NUMCHILD)
nonpart$FHTOTERN74 <- as.numeric(nonpart$FHTOTERN74)
nonpart$FHTOTERN73 <- as.numeric(nonpart$FHTOTERN73)
nonpart$MHTOTERN74 <- as.numeric(nonpart$MHTOTERN74)
nonpart$MHTOTERN73 <- as.numeric(nonpart$MHTOTERN73)
nonpart$NUMADULTS <- as.factor(nonpart$NUMADULTS)
nonpart$TREAT <- as.factor(nonpart$TREAT)

label(nonpart$HHEAD) <- "Households with double householders"
label(nonpart$SHEAD) <- "Households with single householders"
label(nonpart$individual) <- "Single individual"
label(nonpart$NUMADULTS) <- "Number of adults in the household"
label(nonpart$NUMCHILD) <- "Number of children in the household"
label(nonpart$part) <- "Remained in the experiment"
label(nonpart$MHLABPART) <- "Labor participation in the past year (male householder)"
label(nonpart$MHJOBSAT) <- "Job Satisfaction Index (male householder)"
label(nonpart$MHCHDCARE) <- "Expected or actual childcare cost (male householder)"
label(nonpart$MH_SCH) <- "Completed high school (male householder)"
label(nonpart$MHYRSSCHL) <- "Years of schooling (male householder)"
label(nonpart$FHLABPART) <- "Labor participation in the past year (female householder)"
label(nonpart$FHTOTERN74) <- "Total Earnings in 1974 (female householder)"
label(nonpart$FHTOTERN73) <- "Total Earnings in 1973 (female householder)"
label(nonpart$MHTOTERN74) <- "Total Earnings in 1974 (male householder)"
label(nonpart$MHTOTERN73) <- "Total Earnings in 1973 (male householder)"
label(nonpart$FHJOBSAT) <- "Job Satisfaction Index (female householder)"
label(nonpart$FHCHDCARE) <- "Expected or actual childcare cost (female householder)"
label(nonpart$FH_SCH) <- "Completed high school (female householder)"
label(nonpart$FHYRSSCHL) <- "Years of schooling (female householder)"
label(nonpart$TOTFAMINC74) <- "Total family income in 1974"
label(nonpart$AGEM) <- "Age of the male householder"
label(nonpart$AGEF) <- "Age of the female householder"
label(nonpart$TREAT) <- "Treatment plan"
label(nonpart$TOTNHINC73) <- "Total non-householder earned income 1973"
label(nonpart$TOTNHINC74) <- "Total non-householder earned income 1974"



compare = compareGroups::compareGroups(part ~ AGEF + AGEM + NUMADULTS 
                                       + NUMCHILD
                                       + HHEAD + SHEAD + individual
                                       + TOTFAMINC74
                                       + MHYRSSCHL + FHYRSSCHL
                                       + MH_SCH + FH_SCH
                                       + MHLABPART + FHLABPART
                                       + FHTOTERN74 + FHTOTERN73
                                       + MHTOTERN74 + MHTOTERN73
                                       + MHCHDCARE + FHCHDCARE
                                       + MHJOBSAT + FHJOBSAT
                                    + TOTNHINC73
                                       + TOTNHINC74
                                       , data = nonpart,
                                              max.ylev = 10 )
export2latex(createTable(compare))  


