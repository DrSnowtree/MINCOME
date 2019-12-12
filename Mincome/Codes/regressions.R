install.packages("survminer")
library(dplyr)
library(survival)
library(survminer)
install.packages("glmmML")
library(glmmML)
library(fastDummies)
install
library("plm")
install.packages("digest")
library(ggplot2)
library(dplyr)
install.packages("viridisLite")
library(viridis)
install.packages("hrbrthemes")
library(hrbrthemes)


## plots 
# age at which first child was born by year 
# age at second child by year 
#number of events by birth year 

famdata_ind_nocens$totbirths <- 0
for (i in levels(famdata_ind_nocens$OID)){
  s <- sum(famdata_ind_nocens[famdata_ind_nocens$OID == i, "event"])
  famdata_ind_nocens$totbirths[famdata_ind_nocens$OID == i] <- s
}


plot1 <- famdata_ind_nocens %>%
  mutate(woman = factor(OID)) %>%
  ggplot(aes(x=birthyear, y=age_firstch, size = totbirths,  color = totbirths)) +
  geom_point(alpha=0.02) +
  ylab("Age of female householder during first childbirth ") +
  xlab("Birth year of female householder") +
  scale_size(range = c(.1, 24), name="Total Births") +
  theme_bw()
   
plot1

#partially interactive model 
#logit(b_x = 1) = a + j_x +A_xB_1 + CB_2 + C_xB_3 +(j_x+A_x)B_6




logittry <- glm(event ~ j + age_15 + age_16 + age_17 + age_18 + age_19 + age_20 
                + age_21 + age_22 + age_23 + age_24 + age_25 + age_26 + age_27 + age_28 + age_29 
                + age_30 + age_31 + age_32 +age_33 + age_34 + age_35 + age_36 + age_37 + age_38 + age_39 
                + age_40 + age_41 + age_42 +age_43 + age_44 + age_45 + age_46 + age_47 + age_48 + age_49, 
                data = famdata_ind_nocens, family = "binomial")
summary(logittry)



logittry <- glm(event ~ j +  age + age*j + treated_exp + treated_exp*j*age, 
                data = famdata_ind_nocens, family = "binomial")
summary(logittry)


fe <- clogit(event ~ age_15 + age_16 + age_17 + age_18 + age_19 + age_20 
             + age_21 + age_22 + age_23 + age_24 + age_25 + age_26 + age_27 + age_28 + age_29 
             + age_30 + age_31 + age_32 +age_33 + age_34 + age_35 + age_36 + age_37 + age_38 + age_39 
             + age_40 + age_41 + age_42 +age_43 + age_44 + age_45 + age_46 + age_47 + age_48 + age_49 
            + strata(year), data = famdata_ind_nocens) 

summary(fe)

re <- glmmML(event ~ age_15 + age_16 + age_17 + age_18 + age_19 + age_20 
             + age_21 + age_22 + age_23 + age_24 + age_25 + age_26 + age_27 + age_28 + age_29 
             + age_30 + age_31 + age_32 +age_33 + age_34 + age_35 + age_36 + age_37 + age_38 + age_39 
             + age_40 + age_41 + age_42 +age_43 + age_44 + age_45 + age_46 + age_47 + age_48 + age_49, cluster = OID, data = famdata_ind_nocens)

re <- glmmML(event ~ age1519 + age2024 + age2429 + age3034 + age3539
             + age4044 + age4550, cluster = OID, data = famdata_ind_nocens)
summary(re)


