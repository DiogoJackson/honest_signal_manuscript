# Script to calculate saturation, brightness, and hue
# Author: Diogo Silva
# Date: Sat Aug 06 13:17:36 2022
# Last update: Thu Jun  5 14:02:22 2025 ------------------------------

# Packages ----
library(pavo)
library(dplyr)
library(tibble)
library(colorspec)

# 1. Import rspec data ----
reflet <- read.csv("data/raw/refletancias/00_refletancias.csv")
reflet <- fixspec(reflet)

# 2. Calculating brightness, hue, and saturation ----
summary_cor <- summary(reflet, subset = c("S8","B2","H1","S1")) %>% 
  tibble::rownames_to_column(var = "ID2") %>% 
   dplyr::rename(saturation = S8,
                 mean_brightness = B2,
                 hue = H1) %>% 
  dplyr::filter(ID2 != "substrato_barradorio")
summary_cor

# 3. Saving ----
write.csv(summary_cor,
          "outputs/tables/01_summary_cor.csv",
          row.names = F)

# THE END ----------------------------------------------------------------------
