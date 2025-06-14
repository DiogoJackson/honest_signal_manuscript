# Script to merge spreadsheets and create data_master
# Author: Diogo Silva
# Date: Wed Jul 20 18:04:32 2022
# Last update: Wed Jul 20 18:04:49 2022

# Packages ----
library(tidyverse)

# 1. Import data ----
vis_results  <- read.csv("outputs/tables/02_vis.results.clean.csv")
data_force <- read.csv("data/processed/03_data-force_clean.csv")
nrow(vis_results)
nrow(data_force)

# 2. Merging data_force with vismodel_results ----
# Selecting relevant variables to avoid duplicated columns
data_vis_force <- bind_cols(vis_results, data_force)
data_vis_force <- dplyr::select(data_vis_force,
                         -body_region...19,
                         -identidade,
                         -carapace_type,
                         -claw_type...20,
                         -data) %>% 
  rename(claw_type = claw_type...23,
         body_region = body_region...36,
         weight_g = weight_mg,
         weight_mg = weight_g,
         backgroun = substrato)

data_master <- data_vis_force
head(data_master)

# 3. Saving data ----
write.csv(data_master,
          "data/processed/04_data_master.csv",
          row.names = F)

# 4. Testing data ----
data_master_test <- read.csv("data/processed/04_data_master.csv")
head(data_master_test)

# THE END ---------------------------------------------------------------------
