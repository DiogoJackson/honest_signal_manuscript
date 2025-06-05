#Analise de regressão beta para variavel força
#author: Diogo Silva
# Fri Dec 23 13:57:47 2022 ------------------------------

#Packages ----
library(tidyverse)
library(cowplot)
library(glmmTMB)

#1. import data ----
data <- read.csv("data/artigo_ii/processed/04_data_master.csv")
head(data)
str(data)

data_qt <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw") %>% 
  mutate(max_force_log = log10(max_force))

data_lep <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw") %>%
  filter(claw_type == "Leptochelous")

data_brac <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw") %>% 
  filter(claw_type == "Brachychelous")

#Explory distribution of family data ----
#exponencial data ----
exp <- rexp(1000, 10)
hist(exp)
plot(density(exp))
hist(exp)
f1 <- fitdist(exp, "exp")
summary(f1)
plot(f1, demp = TRUE)
descdist(exp, discrete=FALSE)

#max force data ----
hist(data_qt$max_force)
plot(density(data_qt$max_force))
hist(data_qt$max_force)
f1 <- fitdist(data_qt$max_force, "exp")
summary(f1)
plot(f1, demp = TRUE)
descdist(data_qt$max_force, discrete=FALSE)

#GLM e LM ----
mlm1 <- glm(max_force ~ weight_g + size + chromatic_contrast + luminance + saturation,
          family = "gaussian",
          data = data_qt)

glm1 <- glm(max_force ~ weight_g + size + chromatic_contrast + luminance + saturation,
             family = "gaussian",
             data = data_qt)

#O GLM e o LM familia gaussiana (normal) é a mesma coisa.
coef(mlm1)
coef(glm1)

#Fit glm exponencial model ----
glm1 <- glm(max_force ~ claw_type + carapace_color + weight_g + size + chromatic_contrast + luminance + saturation,
            family = "Gamma",
            data = data_qt)

summary(glm1)

glm2 <- glm(max_force ~ carapace_color + chromatic_contrast + luminance + saturation,
            family = "Gamma",
            data = data_qt)

summary(glm2)

#Null deviance: 4046.6;      é a variacao total dos dados.
#Residual deviance: 3318.1;  é a variacao que nao consegue explicar o modelo.

#proporcao de variacao explicada pelo modelo (pseudo R2)----
(4046.6 - 3318.1) / 4046.6

#glmtmb

glm2 <- glmmTMB(luminance ~ size + weight_g + carapace_color + chromatic_contrast + saturation + hue,
            family = beta_family(link = "logit"),
            data = data_brac)
summary(glm2)

