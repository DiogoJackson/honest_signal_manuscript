# Script to run a visual model (vismodel)
# Author: Diogo Silva
# Date: Wed Jul 20 11:35:29 2022
# Last update: Wed Jul 20 11:35:46 2022

# Packages ----
library(pavo)
library(tidyverse)
library(colorspec)

# 1. Import reflectances ----
reflet <- read.csv("data/raw/refletancias/00_refletancias.csv")
reflet <- fixspec(reflet)

# 2. Visual models ----
vis.fiddler <- vis.fiddler(reflet, "substrato_barradorio", illum = "D65")

# 3. Saving tables ----
write.csv(vis.fiddler,
          "outputs/tables/01_vis.fiddler.csv",
          row.names = F)

# 4. Testing tables ----
vis.fiddler.test <- read.csv("outputs/tables/01_vis.fiddler.csv")
head(vis.fiddler.test)

# THE END ---------------------------------------------------------------------
