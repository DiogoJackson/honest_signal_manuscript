# Script to import and clean reflectance data
# author: Diogo Silva
# date: Mon Jul 18 17:36:55 2022
# last update:
# Mon Jul 18 17:37:18 2022

# Packages ----
library(pavo)
library(dplyr)
library(colorspec)

# 1. Import reflectance data ----
reflet <- read.csv("data/raw/refletancias/00_refletancias.csv")
reflet <- fixspec(reflet)

# 2. Separate into reflectance datasets ----
# 2.1. Separating claw and carapace ----
reflet_q <- reflet %>%
  select(wl, contains("_q_"))

reflet_c <- reflet %>%
  select(wl, contains("_c_"))

# 2.2. Separating claws into brachychelous and leptochelous ----
reflet_claw_type <- reflet_q %>%
  rename(Brachychelous = contains("_brac_")) %>% 
  rename(Leptochelous = contains("_lep_"))

# 2.3. Separating carapace color into light and dark ----
reflet_carapace_type <- reflet_c %>%
  rename(Bright = contains("_claro")) %>% 
  rename(Dark = contains("_escuro"))

# 2.4. Separating claw color into light and dark ----
reflet_claw_color <- reflet_q %>%
  rename(Bright = contains("_claro")) %>% 
  rename(Dark = contains("_escuro"))

# 3. Saving tables ----
write.csv(reflet_claw_type,
          "data/processed/01_reflet_claw_type.csv", 
          row.names = FALSE)

write.csv(reflet_carapace_type,
          "data/processed/01_reflet_carapace_type.csv", 
          row.names = FALSE)

write.csv(reflet_claw_color,
          "data/processed/01_reflet_claw_color.csv", 
          row.names = FALSE)

# 4. Test tables ----
reflet_test  <- read.csv("data/processed/01_reflet_claw_type.csv")
reflet_test  <- fixspec(reflet_test)

reflet_test2 <- read.csv("data/processed/01_reflet_carapace_type.csv")
reflet_test2 <- fixspec(reflet_test2)

reflet_test3 <- read.csv("data/processed/01_reflet_claw_color.csv")
reflet_test3 <- fixspec(reflet_test2)

# THE END ---------------------------------------------------------------------


