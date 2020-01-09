length(which(basepay$plan == 1))
length(which(basepay$plan == 2))
length(which(basepay$plan == 3))
length(which(basepay$plan == 4))
length(which(basepay$plan == 5))
length(which(basepay$plan == 7))
length(which(basepay$plan == 8))
length(which(basepay$plan == 9))

length(which(basepay$plan == 1 & basepay$birth == 1))
length(which(basepay$plan == 2 & basepay$birth == 1))
length(which(basepay$plan == 3 & basepay$birth == 1))
length(which(basepay$plan == 4 & basepay$birth == 1))
length(which(basepay$plan == 5 & basepay$birth == 1))
length(which(basepay$plan == 7 & basepay$birth == 1))
length(which(basepay$plan == 8 & basepay$birth == 1))
length(which(basepay$plan == 9 & basepay$birth == 1))


nrplans <- matrix(c(41,57,42,59,48,52, 40,101, 8, 6, 6, 7, 5, 10, 5, 8),ncol=8,byrow=TRUE)
colnames(nrplans) <- c("Plan 1","Plan 2","Plan 3", "Plan 4","Plan 5","Plan 7", "Plan 8","Control")
rownames(nrplans) <- c("Number of households", "Number of births")
percent <- as.data.frame(t(nrplans))
percent$`Birth rate` <- percent$`Number of births`*1/percent$`Number of households`

print(xtable(percent, type = "latex"), file = "percent.tex")
