# Fitting Kaplan-Meier survival curves in R
# Author: Diogo Silva
# Date: Fri Jun  6 15:10:54 2025
# Last update: Fri Jun  6 15:10:54 2025

# Packages ----
library(survival)
library(survminer)
library(readxl)
library(gridExtra)
library(tidyverse)
library(cowplot)

# Import data ----
data_km <- read_excel("data/raw/data_fight.xlsx",
                         sheet = "Kaplan-meier")
head(data_km)

# Fit Kaplan-Meier survival curves ----
kmcurve <- survfit(Surv(tempo_s, toca) ~ tipo_quela, data = data_km)
summary(kmcurve, times=seq(0, 600, 100))

#Plot survival curves ----
p <- ggsurvplot(kmcurve,
           xlim = c(0, 600),  break.x.by = 100,
           linetype = 1,
           ylab = "Burrow occupation (%)", xlab = "Time (s)",
           pval = TRUE, 
           risk.table = FALSE,
           cumevents = FALSE,
           cumcensor = FALSE,
           risk.table.title = "",
           legend.labs = c("Brachychelous", "Leptochelous"), 
           legend.title = "",  
           surv.scale = "percent", 
           palette = c("#ffb560","#18617b"),
           title="",  risk.table.height=.25,
          fun = "event") #fun = event faz com que a curva seja plotada uphill e nao downhill
p

# Save plot ----
png(file = "outputs/figures/survival_curve.png",
    res = 300,
    units = "in",
    width = 4.5,
    height = 4)

ggsurvplot(kmcurve,
                xlim = c(0, 600),  break.x.by = 100,
                linetype = 1,
                ylab = "Burrow occupancy (%)", xlab = "Time (s)",
                pval = TRUE, 
                risk.table = FALSE,
                cumevents = FALSE,
                cumcensor = FALSE,
                risk.table.title = "",
                legend.labs = c("Brachychelous", "Leptochelous"), 
                legend.title = "",  
                surv.scale = "percent", 
                palette = c("#ffb560","#18617b"),
                title="",  risk.table.height=.25,
                fun = "event") #fun = "event" makes the curve go uphill instead of downhill

dev.off()

# THE END -----------------------------------------------------------------