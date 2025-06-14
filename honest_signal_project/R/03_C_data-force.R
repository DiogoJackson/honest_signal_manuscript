# Script to clean data_force
# Author: Diogo Silva
# Date: Wed Jul 20 15:46:02 2022
# Last update: Wed Jul 20 15:46:02 2022

# Packages ----
library(tidyverse)
library(readxl)

# 1. Load data ---------------------------------------------------------------
data_force <- read_excel("data/raw/data_force.xlsx")

# 2. Basic checks --------------------------------------------------------------
nrow(data_force)             # How many rows
str(data_force)              # Variables classes
attributes(data_force)       # Attributres
head(data_force)             # First rows
any(duplicated(data_force))  # There is any duplicated rows?
any(is.na(data_force))       # There are NAs in the data?

# 3. Rename columns names-------------------------------------------------------
dat <- dplyr::rename(data_force, 
                     identidade  = ID,
                     Carapace = carapace_size,
                     Claw = claw_size,
                     )
head(dat)
str(dat)

# 4. Fix factor variables-------------------------------------------------------

levels(dat$claw_type)                               #horario is not a factor, then return NULL
dat$claw_type <- as.factor(dat$claw_type)
levels(dat$claw_type)                           # Now horario is a factor

levels(dat$carapace_color) 
dat$carapace_color <- as.factor(dat$carapace_color)
levels(dat$carapace_color) 

levels(dat$autotomy) 
dat$autotomy <- as.factor(dat$autotomy)
levels(dat$autotomy)

# 4. fix factor names---
levels(dat$claw_type)[levels(dat$claw_type) == "brachy"] <- "Brachychelous"
levels(dat$claw_type)[levels(dat$claw_type) == "lepto"] <- "Leptochelous"

levels(dat$carapace_color)[levels(dat$carapace_color) == "b"] <- "Bright"
levels(dat$carapace_color)[levels(dat$carapace_color) == "e"] <- "Dark"

# 5. check factors
levels(dat$claw_type)
levels(dat$carapace_color)

# 6. inspect NAs-----------------------------------------------------------------
na_check <- inspectdf::inspect_na(dat) # Percentage of NA in each columns
na_check

# 7. Transform claw and carapace size to just one variable ----
dat_longer <- pivot_longer(dat, 
                           cols =c("Carapace","Claw"), 
                           names_to = "body_region", 
                           values_to = "size" )
dat_longer

dat_clean <- dat_longer
head(dat_clean)

# 8. Saving processed data -----------------------------------------------------
write.csv(x = dat_clean, 
          file = "data/processed/03_data-force_clean.csv", 
          row.names = FALSE)

# 9. Testing saved data
dat_test <- read.csv("data/processed/03_data-force_clean.csv")
str(dat_test)
head(dat_test)
nrow(dat_test)

# THE END ---------------------------------------------------------------------
