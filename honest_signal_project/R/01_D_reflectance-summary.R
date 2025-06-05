#Script to calc saturation, brightness and hue
#author: Diogo Silva
#Data: Sat Aug  6 13:17:36 2022 

#Packages ----
library(pavo)
library(dplyr)
library(tibble)
library(colorspec)

#Import rspec data ----
reflet <- read.csv("data/raw/refletancias/00_refletancias.csv")
reflet <- fixspec(reflet)

#Extraindo Brilho, matiz, saturacao----
summary_cor <- summary(reflet, subset = c("S8","B2","H1","S1")) %>% 
  tibble::rownames_to_column(var = "ID2") %>% 
   dplyr::rename(saturation = S8,
                 mean_brightness = B2,
                 hue = H1) %>% 
  dplyr::filter(ID2 != "substrato_barradorio")
summary_cor

#save ----
write.csv(summary_cor,
          "outputs/tables/01_summary_cor.csv",
          row.names = F)

