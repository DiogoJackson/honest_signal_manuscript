#Script to do a vismodel
#author: Diogo Silva
#Data: Wed Jul 20 11:35:29 2022
#Last update: Wed Jul 20 11:35:46 2022

#Packages ----
library(pavo)
library(tidyverse)
library(colorspec)

#Import reflectances ----
reflet <- read.csv("data/raw/refletancias/00_refletancias.csv")
reflet <- fixspec(reflet)

#Vismodels ----
vis.fiddler <- vis.fiddler(reflet, "substrato_barradorio")
vis.peafowl <- vis.peafowl(reflet, "substrato_barradorio")
vis.bluetit <- vis.bluetit(reflet, "substrato_barradorio")

#Unite outputs ----
vis.all <- bind_rows(vis.peafowl, vis.bluetit, vis.fiddler)

#save tables ----
write.csv(vis.all,
          "outputs/tables/01_vis-all.csv",
          row.names = F)

#test tables ----
vis.all.test <- read.csv("outputs/tables/01_vis-all.csv")
head(vis.all.test)

# FIM ---------------------------------------------------------------------
