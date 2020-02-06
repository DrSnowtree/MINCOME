library(dplyr)
library(compareGroups)
basepayfam <- basepay %>% select(FAMNUM, treated)
bp <- read_excel("W:/WU/Projekte/mincome/Mincome/Data/raw or not complete data/bp.xlsx")
basepaycomp <- merge(basepayfam, bp, by = "FAMNUM", all = F)
basepaycomp$individual <- 0 
basepaycomp$basepaycomp[basepaycomp$`Double Head = 1...4`== 0 & basepaycomp$`Single Head = 1...5` == 0] <- 1

basepaycomp[basepaycomp == -9] <- NA
basepaycomp[basepaycomp == -7] <- NA
basepaycomp[basepaycomp == -1] <- NA
basepaycomp[basepaycomp == "."] <- NA

basepaycomp <- as.data.frame(sapply(basepaycomp, as.factor))
numerics <- names(basepaycomp)[sapply(basepaycomp, nlevels) > 30]
numericdf <- basepaycomp %>% select(numerics, FAMNUM, treated, yrschf, 
                                    fmotheduc, ffathereduc, mfathereduc, fmotheduc, yrschm, `fChild care cost`, `mChild care cost`)
numericdf$FAMNUM <- as.numeric(as.character(numericdf$FAMNUM))
numericdf$`Age Male Head...95` <- as.numeric(as.character(numericdf$`Age Male Head...95` ))
numericdf$`Age Female Head...96` <- as.numeric(as.character(numericdf$`Age Female Head...96`))
numericdf$FAMNUM <- as.numeric(as.character(numericdf$FAMNUM))
## do this procedure for all variables 


numericdf <- as.data.frame(sapply(numericdf, as.numeric))

factors <- names(basepaycomp)[sapply(basepaycomp, nlevels) < 31]
factordf <- basepaycomp %>% select(factors, FAMNUM)

##add treated dummy 
compare = compareGroups::compareGroups(treated ~ ., data = numericdf,
                                            max.xlev = 31)
export2latex(createTable(compare))

comparefac = compareGroups::compareGroups(treated ~ ., data = factordf,
                                       max.xlev = 31)
export2latex(createTable(comparefac))


basepaycomp <- basepaycomp %>% select(FAMNUM, 'fJob Sat', fill, mill, )

basepay <- merge(basepaycomp, basepay, all= F) 

levels(basepaycomp$`Double Head = 1...4`)
