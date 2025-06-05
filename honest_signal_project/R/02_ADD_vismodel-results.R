# Script to combine vismodel and summary_cor outputs
# Author: Diogo Silva
# Date: Sat Aug 06 13:39:58 2022
# Last update: Thu Jun 05 14:10:31 2025 ------------------------------

# Packages ----
library(dplyr)

# Import datasets ----
vis.fiddler <- read.csv("outputs/tables/01_vis.fiddler.csv")
summary <- read.csv("outputs/tables/01_summary_cor.csv") %>% 
  rename(ID = ID2) # Renamed ID2 to ID for use in the join function

# Join data frames using join() ----
visual.results <- dplyr::left_join(vis.fiddler, summary, by = "ID")
head(visual.results)

#Saving visual results ----
write.csv(visual.results,
          "outputs/tables/01_visual-results.csv",
          row.names = F)

# Testing ----
vis.results.test <- read.csv("outputs/tables/01_visual-results.csv")
head(vis.results.test)

# THE END ---------------------------------------------------------------------
