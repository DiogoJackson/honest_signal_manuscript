# Script to create the graph of the GLM results for force and claw weight (Figure 3)
# Author: Diogo Silva
# Date: Fri Dec 23 17:40:13 2022 
# Last update: Thu Jun 05 17:30:32 2025 

# Packages ----
library(tidyverse)
library(cowplot)

# 1. import data ----
data <- read.csv("data/processed/04_data_master.csv")
glimpse(data)

data_qt <- data %>%  #qt = quela type
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw")

# 2. Visualization of claw type weight and size
m1.3 <- glm(weight_mg ~ size + claw_type, 
            family = "gaussian", data = data_qt)
summary(m1.3)

p1 <- ggplot(data_qt, aes(x = size, y = weight_mg, 
                            color = claw_type)) +
  geom_point(alpha = 0.8, size = 3) +
  geom_smooth(aes(color = claw_type), method = "glm", formula = y ~ x, linetype = 1, size = 1) +
  scale_color_manual(
    labels = c("Brachychelous", "Leptochelous"),
    values = c("#ffb560", "#18617b")) +
  labs(x = "Claw size (mm)",
       y = "Claw mass (mg)",
       color = "Claw type")+
  theme_classic(base_size = 20)
p1

# 3. Visualization of the relationship between claw weight and force ----
m2.2 <- glm(max_force ~ weight_mg + size + claw_type, 
            family = Gamma(link = "log"), data = data_qt)
summary(m2.2)

p2 <- ggplot(data_qt, aes(x = size, y = max_force, 
                            color = claw_type, size = weight_mg)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "glm", aes(color = claw_type), size = 1) +
  scale_color_manual(
    labels = c("Brachychelous", "Leptochelous"),
    values = c("#ffb560", "#18617b")) +
  labs(
    x = "Claw size (mm)", 
    y = "Grip force (N)", 
    color = "Claw type",
    size = "Claw mass (mg)"
  ) +
  theme_classic(base_size = 20)
p2

# 4. Plot grid ----
p <- plot_grid(p1, p2, 
               ncol = 1,
               align = "hv",
               labels = "AUTO")
p

#5. Saving figure 3 ----
ggsave("outputs/figures/figure_3.png",
       p,
       height = 10,
       width = 8)

# THE END ---------------------------------------------------------------------


