library("glm2")
library("oddsratio")
library("margins")
library("mfx")
library(jtools)
library(erer)
library(ggplot2)
library("ggeffects")
reg1 <- glm(formula = birth ~ treated, 
            family = binomial(link = "logit"), 
            x = TRUE, data = basepay)
p <- ggpredict(reg1, "treated")
plot(p)
basepay$plan_2 <- as.factor(basepay$plan_2)
reg2 <- glm(formula = birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
              plan_5 + plan_7 + plan_8, family = binomial(link = "logit"), data = basepay)
p <- ggpredict(reg2, c("plan_7","plan_1"))
plot(p)


reg11 <- glm(formula = birth ~ treated + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550  + NumChild  +
               yrschf + yrschm +  MAGE+
               chout + if_birth9 + costch + femhome
             , family = binomial(link = "logit"), data = basepay)
m11 = margins(reg11) 
summary(m11) 

reg12 <- glm(formula = birth ~ plan_1 + plan_2 + plan_3 + plan_4 + 
               plan_5 + plan_7 + plan_8 + age1519 + age2024 + age2429 + 
               age3034 + age3539 + age4044 + age4550 + NumChild  +  yrschf + yrschm + MAGE +
               chout + if_birth9 + costch + femhome, 
             family = binomial(link = "logit"), data = basepay)
m12 = margins(reg12) 
m12

plot(m11)


plot_summs(m, m11, scale = TRUE, plot.distributions = FALSE, 
           model.names = c("Without controls", "With controls"),
           coefs = c("Treated" ="treated"),  
           inner_ci_level = .9, colors = "Qual3")
