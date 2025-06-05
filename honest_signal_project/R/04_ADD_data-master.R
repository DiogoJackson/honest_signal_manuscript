#Script para unir planilha e criar data_master
#Author: Diogo Silva
#Data: Wed Jul 20 18:04:32 2022
#Last update: Wed Jul 20 18:04:49 2022

#Packages ----
library(tidyverse)

#Import data force duas vezes ----
vis_results  <- read.csv("outputs/tables/02_vis.results.clean.csv")
data_force <- read.csv("data/processed/03_data-force_clean.csv")
nrow(vis_results)
nrow(data_force)

#triplicar data force para dar match com nrows de vismodel results
data_force2  <- data_force
data_force3  <- data_force

#unindo dados triplicados para unir com planilha de vismodel_results ----
#rename para nao dar sobreposicao de nomes de variavel
data_forcex3 <- bind_rows(data_force, data_force2, data_force3) %>% 
  rename(claw_typex = claw_type,
         body_regionx = body_region,
         )

head(data_forcex3)

#Unindo data_force com vismodel_results ----
#selecionando variaveis para eliminar variaveis duplicadas
data_vis_force <- bind_cols(vis_results, data_forcex3)
data_vis_force <- dplyr::select(data_vis_force,
                         -body_regionx,
                         -identidade,
                         -carapace_type,
                         -claw_typex,
                         -data)


data_master <- data_vis_force
head(data_master)

#Save data ----
write.csv(data_master,
          "data/processed/04_data_master.csv",
          row.names = F)

#test data ----
data_master_test <- read.csv("data/processed/04_data_master.csv")
head(data_master_test)

# Fim ---------------------------------------------------------------------
