# Script para criar graficos aggplot
# author: Diogo Silva
# Data: Tue Jul 19 18:22:33 2022 
# last update: Tue Jul 19 18:22:48 2022 


# Packages ----------------------------------------------------------------
library(tidyverse)
library(pavo)
library(colorspec)

# Import reflectance data -------------------------------------------------
reflet_claw_type     <- read.csv("data/processed/01_reflet_claw_type.csv")
reflet_claw_type     <- fixspec(reflet_claw_type)

reflet_carapace_type <- read.csv("data/processed/01_reflet_carapace_type.csv")
reflet_carapace_type <- fixspec(reflet_carapace_type)

reflet_claw_color <- read.csv("data/processed/01_reflet_claw_color.csv")
reflet_claw_color <- fixspec(reflet_claw_color)

#Aggplot graphics ----

#aggplot claw type (brac and leptochela) ----
plotspec(reflet_claw_type, ylim = c(0, 100))

#aggplot carapace type (bright and dark) ----
plotspec(reflet_carapace_type, ylim = c(0, 100))

#aggplot carapace type (bright and dark) ----
plotspec(reflet_claw_color, ylim = c(0, 100))

#Saving plots ----

#Save claw type plot----
pdf(file = "outputs/figures/01_reflet_claw_type.pdf",
    width = 4.5,
    height = 4)

plotspec(reflet_claw_type, ylim = c(0, 100), legend = T)

dev.off()

png(file = "outputs/figures/01_reflet_claw_type.png",
    units = "in",
    res = 300,
    width = 4.5,
    height = 4)

plotspec(reflet_claw_type, ylim = c(0, 100), legend = T)

dev.off()

#Save claw color ----
png(file = "outputs/figures/01_reflet_claw_cara-type.png",
    units = "in",
    res = 300,
    width = 4.5,
    height = 4)

plotspec(reflet_claw_color, ylim = c(0, 100), legend = T, main = "Claw reflectance")

dev.off()

#Save carapace type plot----
png(file = "outputs/figures/01_reflet_cara-type.png",
    units = "in",
    res = 300,
    width = 4.5,
    height = 4)

plotspec(reflet_carapace_type, ylim = c(0, 100), legend = T, main = "Carapace reflectance")

dev.off()

# FIM ---------------------------------------------------------------------
