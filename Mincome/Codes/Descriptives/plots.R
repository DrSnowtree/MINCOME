library("mosaic")
library("lattice")
library("ggplot2")
library("wesanderson")
library("rcartocolor")
library("patchwork")
library("compareGroups")
basepay$age <- as.character(basepay$age)
basepay$age <- as.numeric(basepay$age)
basepay1 <- basepay[which(basepay$plan == 1), ]
basepay2 <- basepay[which(basepay$plan == 2), ]
basepay3 <- basepay[which(basepay$plan == 3), ]
basepay4 <- basepay[which(basepay$plan == 4), ]
basepay5 <- basepay[which(basepay$plan == 5), ]
basepay7 <- basepay[which(basepay$plan == 7), ]
basepay8 <- basepay[which(basepay$plan == 8), ]
basepay9 <- basepay[which(basepay$plan == 9), ]

age1 <- densityplot(basepay1$age, basepay1)
age2 <- densityplot(basepay2$age, basepay2)
age3 <- densityplot(basepay3$age, basepay3)
age4 <- densityplot(basepay4$age, basepay4)
age5 <- densityplot(basepay5$age, basepay5)
age7 <- densityplot(basepay7$age, basepay7)
age8 <- densityplot(basepay8$age, basepay8)
age9 <- densityplot(basepay9$age, basepay9)

basepay$plan<- as.factor(basepay$plan)
basepay$treated<- as.factor(basepay$treated)

p <-ggplot(basepay, aes(x=age, 
                        color=plan)) 

p <- p + labs(fill= "Treatment plan")
p <- p + labs(x = "Age of the female householder", y = "Density")
p <- p + theme_bw() + theme(panel.border = element_blank(), 
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
p



theme_set(theme_classic(base_size = 14) + theme(panel.background = element_rect(fill = "#f6f1eb")))

set.seed(123)
df <- data.frame(x = rep(1:5, 8), 
                 value = sample(1:100, 40), 
                 variable = rep(paste0("category", 1:8), each = 5))

safe_pal <- carto_pal(12, "Safe")

# https://github.com/clauswilke/colorblindr/blob/master/R/palettes.R
palette_OkabeIto <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                      "#0072B2", "#D55E00", "#CC79A7", "#999999")
palette_OkabeIto_black <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", 
                            "#0072B2", "#D55E00", "#CC79A7", "#000000")

basepay$age <- as.numeric(basepay$age)
basepay$plan <- as.factor(basepay$plan)
bp<-ggplot(basepay, aes(x=plan, y=age, fill=plan)) +
  geom_boxplot(alpha=0.2)
bp
bp<- bp + labs(fill= "Treatment:")
bp <- bp + labs(x = "Treatment plan", y = "Age of the female householder")
bp <- bp + theme_bw() + theme(panel.border = element_blank(), 
                            panel.grid.major = element_blank(), 
                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
bp

t <-ggplot(basepay, aes(x=age, fill=treated)) +
  geom_density(alpha=0.2)
t <- t + labs(fill= "Treatment:")
t <- t + labs(x = "Age of the female householder", y = "Density")

t <- t + labs(x = "Age of the female householder", y = "Density") 
t <- t + theme_bw() + theme(panel.border = element_blank(), 
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
t <- t + scale_fill_manual(values=wes_palette(name="GrandBudapest1"))
t
basepay$FamSize <- as.numeric(basepay$FamSize)
basepay$DH<- as.factor(basepay$DH)
basepay$SH<- as.factor(basepay$SH)
basepay$individual<- as.factor(basepay$individual)
basepaytreated$NumChild<- as.numeric(basepaytreated$NumChild)
basepaytreated <- basepay[which(basepay$treated == 1), ]
compareplans = compareGroups::compareGroups(plan ~ NumChild, 
                                            data = basepaytreated,
                                            max.ylev = 10 )

compareplans_table <-  compareGroups::createTable(compareplans)
print(compareplans_table)
export2latex(createTable(compareplans))



tp <- ggplot(basepay, aes(x=firstplan, y = totpay*1/1000, fill=firstplan,outline=FALSE)) +
  geom_boxplot(alpha=0.2)
tp <- tp + labs(fill= "Treatment plan")
tp <- tp + labs(x = "Plan, first allocation", y = "Total payment received")
tp <- tp + theme_bw() + theme(panel.border = element_blank(), 
                            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
tp
