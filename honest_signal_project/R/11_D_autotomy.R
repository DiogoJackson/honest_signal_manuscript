#Autotomy trial
#Diogo Silva
# Fri Sep 29 10:51:08 2023 ------------------------------

#packages ----
library(tidyverse)

#Import data ----
data <- read.csv("data/processed/04_data_master.csv")
head(data)
str(data)

#Filtering ----
data2 <- data %>% 
  filter(autotomy_exp_type == "by_trial") %>% 
  filter(vismodel == "Peafowl") %>% 
  filter(body_region == "Claw")

#exploratory ----
boxplot(data2$autotomy_trial~ data2$carapace_color)
boxplot(data2$autotomy_trial~ data2$claw_type)
plot(data2$autotomy_trial~ data2$size)
