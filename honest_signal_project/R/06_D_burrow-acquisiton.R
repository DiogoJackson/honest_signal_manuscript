# Proportion analysis of burrow acquisition
# Author: Diogo Silva
# Date: Fri Jun  6 15:32:15 2025
# Last update: Fri Jun  6 15:32:31 2025

# Packages ----
library(readxl)
library(gridExtra)
library(tidyverse)
library(cowplot)

# Import data ----
data_km <- read_excel("data/raw/data_fight.xlsx",
                      sheet = "Kaplan-meier")
head(data_km)

# Proportion analysis ----
toca <- data_km %>%
  dplyr::count(aquisicao_de_toca, tipo_quela) %>%
  group_by(aquisicao_de_toca) %>%
  mutate(percent = n / sum(n) * 100) %>% 
  mutate(tipo_quela = fct_recode(tipo_quela,
                                 "Brachychelous" = "brac",
                                 "Leptochelous" = "lep"))
toca

# Proportion analysis for burrow acquisition through combat  ----
prop_test <- prop.test(x = c(11, 3), n = c(30, 30))  
prop_test

# Proportion analysis for burrow acquisition through theft  ----
prop_test2 <- prop.test(x = c(4, 5), n = c(30, 30))
prop_test2

# # Proportion analysis for burrow acquisition through empty burrow  ----
prop_test3 <- prop.test(x = c(7, 8), n = c(30, 30))
prop_test3

# # Proportion analysis for no burrow acquisition  ----
prop_test4 <- prop.test(x = c(10, 15), n = c(30, 30))
prop_test4


# FIM ---------------------------------------------------------------------


