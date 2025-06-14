# Analysis of Mate Choice
# Author: Diogo Silva
# Tue Jan 16 14:01:37 2024 ------------------------------

# Last update: Tue Jan 16 14:01:44 2024 ------------------------------

# Packages ----
library(tidyverse)
library(cowplot)
library(ggpubr)
library(readxl)
library(car)

# 1. Import data ----
data <- read.csv("data/raw/dados_mestrado_bia.csv")
head(data)
str(data)

# 2. Adjusting dataset ----
data_agg <- aggregate(female_choice ~ claw_type, 
                      FUN = sum, 
                      data = data)
data_agg

38/62*100

# 3. Binomial GLM ----
dados_semNA <- remove_missing(data, vars = "claw_type") 

# 3.1. Fitting model ----
m1 <- glm(female_choice ~ claw_type + claw_size + chromatic_contrast + S8 + B2, data = dados_semNA, family = binomial(link = "logit"))
summary(m1)

m2 <- glm(female_choice ~ claw_type, data = dados_semNA, family = binomial(link = "logit"))
summary(m2)

# 3.2. Comparing models ----
anova(m1, m2, test = "LRT")

# 3.3. Selected model ----
m2 <- glm(female_choice ~ claw_type, data = dados_semNA, family = binomial(link = "logit"))
summary(m2)

# THE END ---------------------------------------------------------------------
