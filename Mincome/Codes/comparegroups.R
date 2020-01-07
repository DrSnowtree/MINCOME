#Tables and comparative descriptive statistics 
basepaycross_rem <- basepaycross_rem[which(basepaycross_rem$Winni == 1),]
comparechild = compareGroups::compareGroups(plan ~ if_increase, data = basepaycross_rem,
                                            max.ylev = 10 )


comparechild_table <-  compareGroups::createTable(comparechild)
print(comparechild_table)
export2latex(createTable(comparechild))
basepaycross_rem19 <- basepaycross_rem[basepaycross_rem$plan == 9| basepaycross_rem$plan 
                                       == 1, ]
comparechild19 = compareGroups::compareGroups(plan ~ if_increase, data = basepaycross_rem19,
                         max.ylev = 10 )
export2latex(createTable(comparechild19))         

basepaycross_rem29 <- basepaycross_rem[basepaycross_rem$plan == 9| basepaycross_rem$plan 
                                    == 2, ]
comparechild29 = compareGroups::compareGroups(plan ~ if_increase, data = basepaycross_rem29,
                                              max.ylev = 10 )
export2latex(createTable(comparechild29))  

basepaycross_rem39 <- basepaycross_rem[basepaycross_rem$plan == 9| basepaycross_rem$plan 
                                       == 3, ]
comparechild39 = compareGroups::compareGroups(plan ~ if_increase, data = basepaycross_rem39,
                                              max.ylev = 10 )
export2latex(createTable(comparechild39)) 

basepaycross_rem49 <- basepaycross_rem[basepaycross_rem$plan == 9| basepaycross_rem$plan 
                                       == 4, ]
comparechild49 = compareGroups::compareGroups(plan ~ if_increase, data = basepaycross_rem49,
                                              max.ylev = 10 )

export2latex(createTable(comparechild49)) 

basepaycross_rem59 <- basepaycross_rem[basepaycross_rem$plan == 9| basepaycross_rem$plan 
                                       == 5, ]
comparechild59 = compareGroups::compareGroups(plan ~ if_increase, data = basepaycross_rem59,
                                              max.ylev = 10 )

export2latex(createTable(comparechild59)) 

basepaycross_rem79 <- basepaycross_rem[basepaycross_rem$plan == 9| basepaycross_rem$plan 
                                       == 7, ]
comparechild79 = compareGroups::compareGroups(plan ~ if_increase, data = basepaycross_rem79,
                                              max.ylev = 10 )

export2latex(createTable(comparechild79)) 

basepaycross_rem89 <- basepaycross_rem[basepaycross_rem$plan == 9| basepaycross_rem$plan 
                                       == 8, ]
comparechild89 = compareGroups::compareGroups(plan ~ if_increase, data = basepaycross_rem89,
                                              max.ylev = 10 )

export2latex(createTable(comparechild89)) 







comparechild2 = compareGroups::compareGroups(control ~ if_increase, data = basepaycross_rem,
)

comparechild2_table <-  compareGroups::createTable(comparechild2)
print(comparechild2_table)
export2latex(createTable(comparechild2))

basepaycross_rem$FAMSfactor <- as.factor(as.character(basepaycross_rem$FAMS))
basepaycross_rem$DH <- as.factor(as.character(basepaycross_rem$DH))
basepaycross_rem$SH <- as.factor(as.character(basepaycross_rem$SH))
basepaycross_rem$individual <- as.factor(as.character(basepaycross_rem$individual))

comparefamst = compareGroups::compareGroups(plan ~ DH + SH +  individual
                                             + MAGE + FAGE +
                                              FAMS + 
                                              FAMSfactor + meaninc, 
                                            data = basepaycross_rem,
                                            max.ylev = 10 )

comparefamst_table <-  compareGroups::createTable(comparefamst)
print(comparefamst_table)
export2latex(createTable(comparefamst))

boxplot(basepaycross_rem$FAMSIZE, horizontal = TRUE)
boxplot(basepaycross_rem$AGEF, horizontal = TRUE)
boxplot(basepaycross_rem$AGEM, horizontal = TRUE)