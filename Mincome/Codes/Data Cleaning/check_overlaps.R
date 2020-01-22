
#there are over 500 families in baseline payments data and 1114 female householders in family composition data 
#compare by merging a la Stata 
bp <- read_excel("W:/WU/Projekte/mincome/Mincome/Data/base_pay.data_revised_Dec 11, 2019.xlsx")
familydata <- read_excel("W:/WU/Projekte/mincome/Mincome/Data/familydata.xlsx")


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


bp$FAMNUM <- bp$`Fam Num`
bp$FAMNUM <- as.factor(bp$FAMNUM)
familydata$FAMNUM<- familydata$FAMNUM...1
familydata$FAMNUM <- as.factor(familydata$FAMNUM)

bp <- bp[which(bp$`WPG Site = 1` == 1), ]
CHECK <- stata.merge(familydata,bp, by = "FAMNUM")

CHECK <- CHECK[which(CHECK$merge != "matched"), ]



length(which(CHECK$merge == "using"))
length(which(CHECK$merge == "master"))
length(which(CHECK$merge == "matched"))
CHECK$FAMNUM[which(CHECK$merge == "using")]
CHECK$FAMNUM[which(CHECK$merge == "master")]
