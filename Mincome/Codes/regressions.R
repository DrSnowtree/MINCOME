install.packages("survminer")
library(dplyr)
library(survival)
library(survminer)
install.packages("glmmML")
library(glmmML)
library(fastDummies)

logittry <- glm(event ~ age_15 + age_16 + age_17 + age_18 + age_19 + age_20 
                + age_21 + age_22 + age_23 + age_24 + age_25 + age_26 + age_27 + age_28 + age_29 
                + age_30 + age_31 + age_32 +age_33 + age_34 + age_35 + age_36 + age_37 + age_38 + age_39 
                + age_40 + age_41 + age_42 +age_43 + age_44 + age_45 + age_46 + age_47 + age_48 + age_49 
                + treated_exp, data = famdata_ind_nocens, family = "binomial")
summary(logittry)

fe <- clogit(event ~ age_15 + age_16 + age_17 + age_18 + age_19 + age_20 
             + age_21 + age_22 + age_23 + age_24 + age_25 + age_26 + age_27 + age_28 + age_29 
             + age_30 + age_31 + age_32 +age_33 + age_34 + age_35 + age_36 + age_37 + age_38 + age_39 
             + age_40 + age_41 + age_42 +age_43 + age_44 + age_45 + age_46 + age_47 + age_48 + age_49 
             + treated_exp + strata(year), data = famdata_ind_nocens) 

summary(fe)

re <- glmmML(event ~ age_15 + age_16 + age_17 + age_18 + age_19 + age_20 
             + age_21 + age_22 + age_23 + age_24 + age_25 + age_26 + age_27 + age_28 + age_29 
             + age_30 + age_31 + age_32 +age_33 + age_34 + age_35 + age_36 + age_37 + age_38 + age_39 
             + age_40 + age_41 + age_42 +age_43 + age_44 + age_45 + age_46 + age_47 + age_48 + age_49 
             + treated_exp, cluster = OID, data = famdata_ind_nocens)
summary(re)


famdata_ind_nocens <- fastDummies::dummy_columns(famdata_ind_nocens, select_columns = "age")
