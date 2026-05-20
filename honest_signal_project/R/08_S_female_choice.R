# Creating a Graph of Female Mate Choice
# Author: Diogo Silva
# Tue Jan 16 14:01:37 2024 
# Last update: Tue Jan 16 14:01:44 2024 

# Packages ----
library(tidyverse)
library(cowplot)
library(ggpubr)
library(readxl)
library(car)

# 1. Import data ----
data <- read.csv("data/raw/dados_mestrado_bia.csv")
head(data)
str(data)

# 2. Adjusting dataset ----
data_agg <- aggregate(female_choice ~ claw_type, 
                      FUN = sum, 
                      data = data)
data_agg
sum(data_agg$female_choice)

# 3. Bar plot ----
p1 <- ggplot(data_agg, aes(x = claw_type, y = female_choice, fill = claw_type)) +
  geom_bar(color = "black", stat = "identity", show.legend = F, width = 0.5) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#ffb560","#18617b")) +
  scale_x_discrete(limits = c('Brachychelous','Leptochelous'))+
  labs(x = "Claw type", 
       y = "Female choice", 
       title = '')+
  theme_classic(base_size = 18)+
  annotate(geom = "text", x = 1.5, y = max(data_agg$female_choice) + 1, size = 7, label = "*")
p1

# 4. Save ----
ggsave(plot = p1, 
       filename = "outputs/figures/05_claw-type_choice.png",
       width = 6, 
       height = 5.5, 
       dpi = 300)

# THE END ---------------------------------------------------------------------

