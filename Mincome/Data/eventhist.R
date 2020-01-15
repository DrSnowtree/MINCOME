
data_personperioda <- data_personperiod %>% 
                     filter(birthyear >= 1940)
  
  
reg121 <- glm(birth ~ treated*experiment + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + strata(year) + strata(OID), 
             family = binomial(link = "logit"), data = data_personperiod)
summary(reg121)
