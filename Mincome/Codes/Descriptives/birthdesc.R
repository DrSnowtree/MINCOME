library(stargazer)
library(dplyr)
library(compareGroups)
basepay[basepay == -9] <- NA
basepay[basepay == -1] <- NA
basepay[basepay == -4] <- NA
basepay$individual <- as.factor(basepay$individual)
basepay$DH <- as.factor(basepay$DH)
basepay$SH <- as.factor(basepay$SH)
basepay$NumChild <- as.factor(basepay$NumChild)
basepay$highschf <- as.factor(basepay$highschf)
basepay$highschm <- as.factor(basepay$highschm)

basepay_desc <- basepay %>% select(age, MAGE, 
                                   NumChild, highschm, highschf,
                                   yrschm, yrschf, treated, if_birth, if_birth9,
                                   DH, SH, individual)

basepay_desc$age <- as.numeric(as.character(basepay_desc$age))
label(basepay_desc$age) <- "Age (female)"
label(basepay_desc$MAGE) <- "Age (male)"
label(basepay_desc$NumChild) <- "Number of children in the household"
label(basepay_desc$highschm) <- "High school completed (male)"
label(basepay_desc$highschf) <- "High school completed (female)"
label(basepay_desc$yrschm) <- "Years of schooling (male)"
label(basepay_desc$yrschf) <- "Years of schooling (female)"
label(basepay_desc$if_birth9) <- "If there has been birth in the first nine months"


basepay_desc$if_birth9 <- as.character(basepay_desc$if_birth9)
controltreat = compareGroups::compareGroups(treated ~ . , data = basepay_desc,
                                              max.ylev = 10 )
export2latex(createTable(controltreat))   

compbirth = compareGroups::compareGroups(if_birth ~ . , data = basepay_desc,
                                            max.ylev = 10 )

export2latex(createTable(compbirth))   

length(which(basepay$treated == 1 & basepay$if_birth == 1))*1/length(which(basepay$treated == 1))