#Script to clean JND
#author: Diogo Silva
#Data: Wed Jul 20 11:35:29 2022
#Last update: Wed Jul 20 14:35:01 2022 ------------------------------

# Packages ----
library(tidyverse)
library(inspectdf)
library(forcats)

# 1. CLEAN JND uca ----

# 1.1. Load JND uca data ----
vis_results    <- read.csv("outputs/tables/01_visual-results.csv")

# 1.2 Basic checks - QI uca ----------------------------------------------------
nrow(vis_results)             # How many rows
str(vis_results)              # Variables classes
attributes(vis_results)       # Attributres
head(vis_results)             # First rows
any(duplicated(vis_results))  # There is any duplicated rows?
any(is.na(vis_results))       # There are NAs in the data?

# Adicionando variáveis ----
vis_results <- vis_results %>%
  mutate(
    body_region = case_when(
      str_detect(ID, "_q_") ~ "Claw",
      str_detect(ID, "_c_") ~ "Carapace",
      TRUE ~ "NA"
    ),
    claw_type = case_when(
      str_detect(ID, "_brac_") ~ "Brachychelous",
      str_detect(ID, "_lep_") ~ "Leptochelous",
      TRUE ~ "NA"
    ),
    carapace_type = case_when(
      str_detect(ID, "claro") ~ "Bright",
      str_detect(ID, "escuro") ~ "Dark",
      TRUE ~ "NA"
    )
  )

head(vis_results)

#save table JND Uca-pavo ----
write.csv(vis_results,
          "outputs/tables/02_vis.results.clean.csv",
          row.names = F)

#test tables ----
vis_results_test  <- read.csv("outputs/tables/02_vis.results.clean.csv")
head(vis_results_test)

# FIM ---------------------------------------------------------------------
