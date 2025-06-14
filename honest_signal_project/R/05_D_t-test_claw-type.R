# Analysis of Colorimetric Variables Using the Mann-Whitney Test
# Author: Diogo Silva
# Date: Thu Jul 21 13:32:34 2022 
# Lastupdate: Sat Jun 14 17:55:50 2025

options(scipen=999) # Remove scientific notation from test results

# Packages ----
library(tidyverse)
library(broom)
library(rstatix)
library(cowplot)

# 1. import data ----
data <- read.csv("data/processed/04_data_master.csv")
head(data)
str(data)

# 2. Analyzing Claw Type ------------------------------------------------------
# Subset of the crab visual modeling considering only claw values

data_qt <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw")

head(data_qt) #qt = quela type
tail(data_qt)

# 2.1. Analyzing Data Normality ------------------------------------------------
shapiro.test(data_qt$saturation)          #nao-normal
shapiro.test(data_qt$mean_brightness)     #normal
shapiro.test(data_qt$chromatic_contrast)
shapiro.test(data_qt$luminance)

# 3. Mann-Whitney test ---------------------------------------------------------
boxplot(data_qt$saturation ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Saturation")
wilcox.test(data_qt$saturation ~ data_qt$claw_type)

boxplot(data_qt$mean_brightness ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Mean brightness")
wilcox.test(data_qt$mean_brightness ~ data_qt$claw_type)

boxplot(data_qt$chromatic_contrast ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Chromatic contrast")
wilcox.test(data_qt$chromatic_contrast ~ data_qt$claw_type)

boxplot(data_qt$luminance ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Luminance")
wilcox.test(data_qt$luminance ~ data_qt$claw_type)

# THE END ---------------------------------------------------------------------

