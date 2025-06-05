# Script to import reflectance files (procspec) and create a spreadsheet
# author: Diogo Silva
# date: Mon Jul 18 17:36:55 2022
# last update:
# Mon Jul 18 17:37:18 2022 ------------------------------

#Packages ----
library(tidyverse)
library(pavo)
library(colorspec)

#Import procspec data ----
refletancias <- getspec("data/raw/refletancias",                 
                                ext=c("procspec","txt"), 
                                decimal=",", 
                                lim=c(300,700))
refletancias <- fixspec(refletancias)

#Save reflectances ----
write.csv(refletancias,
          "data/raw/refletancias/00_refletancias.csv", 
          row.names = FALSE)

#test saved data ----
reflet_test <- read.csv("data/raw/refletancias/00_refletancias.csv")
reflet_test <- fixspec(reflet_test)
head(reflet_test)

# FIM ----