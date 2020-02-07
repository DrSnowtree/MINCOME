library(dplyr)
library(compareGroups)
library("readxl")

basepayfam <- basepay %>% select(FAMNUM, treated)
bp <- read_excel("W:/WU/Projekte/mincome/Mincome/Data/raw or not complete data/bp.xlsx")
basepaycomp <- merge(basepayfam, bp, by = "FAMNUM", all = F)
basepaycomp$individual <- 0 
basepaycomp$individual[basepaycomp$`Double Head = 1...4`== 0 & basepaycomp$`Single Head = 1...5` == 0] <- 1

basepaycomp[basepaycomp == -9] <- NA
basepaycomp[basepaycomp == -7] <- NA
basepaycomp[basepaycomp == -1] <- NA
basepaycomp[basepaycomp == "."] <- NA

basepaycomp <- as.data.frame(sapply(basepaycomp, as.factor))
numerics <- names(basepaycomp)[sapply(basepaycomp, nlevels) > 30]
numericdf <- basepaycomp %>% select(numerics, FAMNUM, treated, yrschf, 
                                    fmotheduc, ffathereduc, mfathereduc, mmotheduc, yrschm, 
                                    "fChild care cost", "mChild care cost","fYears FT", "myearsFT", 
                                    `fNum Job`)



numericdf$FAMNUM <- as.numeric(as.character(numericdf$FAMNUM))
numericdf$`Age Male Head...95` <- as.numeric(as.character(numericdf$`Age Male Head...95`))
numericdf$`Age Female Head...96` <- as.numeric(as.character(numericdf$`Age Female Head...96`))
numericdf$`Home Val` <- as.numeric(as.character(numericdf$`Home Val`))
numericdf$Mortgae <- as.numeric(as.character(numericdf$Mortgae))
numericdf$Rent <- as.numeric(as.character(numericdf$Rent))
numericdf$`Val Vehic`<- as.numeric(as.character(numericdf$`Val Vehic`))
numericdf$`Liq Assets` <- as.numeric(as.character(numericdf$`Liq Assets`))
numericdf$`Durables Val` <- as.numeric(as.character(numericdf$`Durables Val`))
numericdf$`Tot UIC 74` <- as.numeric(as.character(numericdf$`Tot UIC 74` ))
numericdf$`Tot UIC 73` <- as.numeric(as.character(numericdf$`Tot UIC 73`))
numericdf$`Tot Wel 74` <- as.numeric(as.character(numericdf$`Tot Wel 74`))
numericdf$`Wel Mun_Prov 74`  <- as.numeric(as.character(numericdf$`Wel Mun_Prov 74`))
numericdf$`Tot Oth Unear 73` <- as.numeric(as.character(numericdf$`Tot Oth Unear 73` ))
numericdf$`Tot Fam Inc 74` <- as.numeric(as.character(numericdf$`Tot Fam Inc 74`))
numericdf$mnumjob <- as.numeric(as.character(numericdf$mnumjob))
numericdf$mhrspaid <- as.numeric(as.character(numericdf$mhrspaid))
numericdf$mnumjob <- as.numeric(as.character(numericdf$mnumjob))
numericdf$mwagerate <- as.numeric(as.character(numericdf$mwagerate))
numericdf$mgrossearnings <- as.numeric(as.character(numericdf$mgrossearnings))
numericdf$`mTot Earn` <- as.numeric(as.character(numericdf$`mTot Earn`))
numericdf$`mTot Earn 74` <- as.numeric(as.character(numericdf$`mTot Earn 74`))
numericdf$`mNum Weeks 74` <- as.numeric(as.character(numericdf$`mNum Weeks 74`))
numericdf$`mNum Weeks 73` <- as.numeric(as.character(numericdf$`mNum Weeks 73`))
numericdf$`mAvg week hrs x10` <- as.numeric(as.character(numericdf$`mAvg week hrs x10`))
numericdf$myearsFT <- as.numeric(as.character(numericdf$myearsFT))
numericdf$`fHours P  x10` <- as.numeric(as.character(numericdf$`fHours P  x10`))
numericdf$`fWage Rate  x100` <- as.numeric(as.character(numericdf$`fWage Rate  x100`))
numericdf$`fGross Earnings` <- as.numeric(as.character(numericdf$`fGross Earnings`))
numericdf$`fTot Earn` <- as.numeric(as.character(numericdf$`fTot Earn`))
numericdf$`fTot Earn 74` <- as.numeric(as.character(numericdf$`fTot Earn 74`))
numericdf$`fNum Weeks 74` <- as.numeric(as.character(numericdf$`fNum Weeks 74`))
numericdf$`fNum weeks 73`<- as.numeric(as.character(numericdf$`fNum weeks 73`))
numericdf$`fAvg week hrs x10` <- as.numeric(as.character(numericdf$`fAvg week hrs x10`))
numericdf$'fYears FT'<- as.numeric(as.character(numericdf$'fYears FT'))
numericdf$yrschf<- as.numeric(as.character(numericdf$yrschf))
numericdf$yrschm<- as.numeric(as.character(numericdf$yrschm))
numericdf$fmotheduc<- as.numeric(as.character(numericdf$fmotheduc))
numericdf$ffathereduc<- as.numeric(as.character(numericdf$ffathereduc))
numericdf$mfathereduc<- as.numeric(as.character(numericdf$mfathereduc))
numericdf$mmotheduc<- as.numeric(as.character(numericdf$mmotheduc))
numericdf$`fChild care cost`<- as.numeric(as.character(numericdf$`fChild care cost`))
numericdf$`mChild care cost`<- as.numeric(as.character(numericdf$`mChild care cost`))
numericdf$ `fNum Job`<- as.numeric(as.character(numericdf$ `fNum Job`))

## do this procedure for all variables 

factors <- names(basepaycomp)[sapply(basepaycomp, nlevels) < 31]
factordf <- basepaycomp %>% select(factors, FAMNUM)

##add treated dummy 
compare = compareGroups::compareGroups(treated ~ ., data = numericdf,
                                            max.xlev = 31)
export2latex(createTable(compare))

comparefac = compareGroups::compareGroups(treated ~ ., data = factordf,
                                       max.xlev = 31)
export2latex(createTable(comparefac))


basepaycomp$FAMNUM <- as.numeric(as.character(basepaycomp$FAMNUM))
basepaycomp$`Age Male Head...95` <- as.numeric(as.character(basepaycomp$`Age Male Head...95`))
basepaycomp$`Age Female Head...96` <- as.numeric(as.character(basepaycomp$`Age Female Head...96`))
basepaycomp$`Home Val` <- as.numeric(as.character(basepaycomp$`Home Val`))
basepaycomp$Mortgae <- as.numeric(as.character(basepaycomp$Mortgae))
basepaycomp$Rent <- as.numeric(as.character(basepaycomp$Rent))
basepaycomp$`Val Vehic`<- as.numeric(as.character(basepaycomp$`Val Vehic`))
basepaycomp$`Liq Assets` <- as.numeric(as.character(basepaycomp$`Liq Assets`))
basepaycomp$`Durables Val` <- as.numeric(as.character(basepaycomp$`Durables Val`))
basepaycomp$`Tot UIC 74` <- as.numeric(as.character(basepaycomp$`Tot UIC 74` ))
basepaycomp$`Tot UIC 73` <- as.numeric(as.character(basepaycomp$`Tot UIC 73`))
basepaycomp$`Tot Wel 74` <- as.numeric(as.character(basepaycomp$`Tot Wel 74`))
basepaycomp$`Wel Mun_Prov 74`  <- as.numeric(as.character(basepaycomp$`Wel Mun_Prov 74`))
basepaycomp$`Tot Oth Unear 73` <- as.numeric(as.character(basepaycomp$`Tot Oth Unear 73` ))
basepaycomp$`Tot Fam Inc 74` <- as.numeric(as.character(basepaycomp$`Tot Fam Inc 74`))
basepaycomp$mnumjob <- as.numeric(as.character(basepaycomp$mnumjob))
basepaycomp$mhrspaid <- as.numeric(as.character(basepaycomp$mhrspaid))
basepaycomp$mnumjob <- as.numeric(as.character(basepaycomp$mnumjob))
basepaycomp$mwagerate <- as.numeric(as.character(basepaycomp$mwagerate))
basepaycomp$mgrossearnings <- as.numeric(as.character(basepaycomp$mgrossearnings))
basepaycomp$`mTot Earn` <- as.numeric(as.character(basepaycomp$`mTot Earn`))
basepaycomp$`mTot Earn 74` <- as.numeric(as.character(basepaycomp$`mTot Earn 74`))
basepaycomp$`mNum Weeks 74` <- as.numeric(as.character(basepaycomp$`mNum Weeks 74`))
basepaycomp$`mNum Weeks 73` <- as.numeric(as.character(basepaycomp$`mNum Weeks 73`))
basepaycomp$`mAvg week hrs x10` <- as.numeric(as.character(basepaycomp$`mAvg week hrs x10`))
basepaycomp$myearsFT <- as.numeric(as.character(basepaycomp$myearsFT))
basepaycomp$`fHours P  x10` <- as.numeric(as.character(basepaycomp$`fHours P  x10`))
basepaycomp$`fWage Rate  x100` <- as.numeric(as.character(basepaycomp$`fWage Rate  x100`))
basepaycomp$`fGross Earnings` <- as.numeric(as.character(basepaycomp$`fGross Earnings`))
basepaycomp$`fTot Earn` <- as.numeric(as.character(basepaycomp$`fTot Earn`))
basepaycomp$`fTot Earn 74` <- as.numeric(as.character(basepaycomp$`fTot Earn 74`))
basepaycomp$`fNum Weeks 74` <- as.numeric(as.character(basepaycomp$`fNum Weeks 74`))
basepaycomp$`fNum weeks 73`<- as.numeric(as.character(basepaycomp$`fNum weeks 73`))
basepaycomp$`fAvg week hrs x10` <- as.numeric(as.character(basepaycomp$`fAvg week hrs x10`))
basepaycomp$'fYears FT'<- as.numeric(as.character(basepaycomp$'fYears FT'))
basepaycomp$yrschf<- as.numeric(as.character(basepaycomp$yrschf))
basepaycomp$yrschm<- as.numeric(as.character(basepaycomp$yrschm))
basepaycomp$fmotheduc<- as.numeric(as.character(basepaycomp$fmotheduc))
basepaycomp$ffathereduc<- as.numeric(as.character(basepaycomp$ffathereduc))
basepaycomp$mfathereduc<- as.numeric(as.character(basepaycomp$mfathereduc))
basepaycomp$mmotheduc<- as.numeric(as.character(basepaycomp$mmotheduc))
basepaycomp$`fChild care cost`<- as.numeric(as.character(basepaycomp$`fChild care cost`))
basepaycomp$`mChild care cost`<- as.numeric(as.character(basepaycomp$`mChild care cost`))
basepaycomp$ `fNum Job`<- as.numeric(as.character(basepaycomp$ `fNum Job`))

basepaycomp <- basepaycomp %>% rename(numvehic = `Num Vehic`, 
                                      valvehic = `Val Vehic`, 
                                      totfaminc = `Tot Fam Inc 74`, 
                                      fhrspaid = `fHours P  x10`, mtotearn = `mTot Earn`, ftotearn= `fTot Earn`)



basepaycomp <- basepaycomp %>% select (FAMNUM, numvehic, valvehic, mill, fill, totfaminc, minsch, finsch, 
                                      fhrspaid, mtotearn, ftotearn, mhrspaid, fmotheduc, ffathereduc)
basepay <- merge(basepay, basepaycomp, by = "FAMNUM")

