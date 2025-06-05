# Script para criacao dos graficos
# Author: Diogo Silva
# Data: Wed May 17 13:24:28 2023
# Last update:
# Wed May 17 13:24:42 2023 ------------------------------

#Packages ----
library(tidyverse)
library(cowplot)
library(ggpubr)
library(colorspec)

#1. import data ----
data <- read.csv("data/processed/04_data_master.csv")
head(data)
str(data)

data_qt <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw")

#1.1. Import claws reflectance ----
reflet <- read.csv("data/processed/01_reflet_claw_type.csv")
reflet <- fixspec(reflet)

#Comparations ----
group <- list(c("Brachychelous","Leptochelous"))
stat <- stat_compare_means(comparisons = group, tip.length=0.01,
                           label = "p.signif", 
                           symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                              symbols = c("****", "***", "**", "*", "ns")))

#my_colors
my_colors <- c("#ffb560","#18617b")

# Plot graphs ----

b2_plot <- ggplot(data_qt, aes(x = claw_type, y = mean_brightness)) +
  geom_boxplot(fill = "white", width = 0.4) +
  geom_jitter(aes(color = claw_type), alpha = 0.5, size = 1.5, show.legend = FALSE, width = 0.06)+
  scale_color_manual(values = c(my_colors))+
  labs(x = 'Claw type', 
       y = 'Mean brightness')+
  theme_classic(base_size = 13) +
  stat
b2_plot

sat_plot <- ggplot(data_qt, aes(x = claw_type, y = saturation)) +
  geom_boxplot(fill = "white", width = 0.4) +
  geom_jitter(aes(color = claw_type), alpha = 0.5, size = 1.5, show.legend = FALSE, width = 0.06)+
  scale_color_manual(values = c(my_colors))+
  labs(x = 'Claw type', 
       y = 'Saturation')+
  theme_classic(base_size = 13) +
  stat
sat_plot

claw <- reflet %>%
  rowwise() %>%
  mutate(
    media1 = mean(c_across(contains("Brachychelous")), na.rm = TRUE),
    sd1 = sd(c_across(contains("Brachychelous")), na.rm = TRUE),
    media2 = mean(c_across(contains("Leptochelous")), na.rm = TRUE),
    sd2 = sd(c_across(contains("Leptochelous")), na.rm = TRUE)
  ) %>%
  ungroup() %>%
  ggplot(aes(x = wl)) +
  geom_ribbon(aes(ymin = media1 - sd1, ymax = media1 + sd1, fill = "Brachy"), 
              alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = media1, color = "Brachy"), show.legend = FALSE) +
  geom_ribbon(aes(ymin = media2 - sd2, ymax = media2 + sd2, fill = "Lepto"), 
              alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = media2, color = "Lepto"), show.legend = FALSE) +
  
  scale_fill_manual(
    values = c("Brachy" = "#ffb560", "Lepto" = "#18617b"), 
    name = "") +
  scale_color_manual(
    values = c("Brachy" = "#ffb560", "Lepto" = "#18617b"), 
    name = ""
  ) +
  
  labs(
    x = "Wavelength (nm)",
    y = "Reflectance (%)"
  ) +
  theme_classic(base_size = 13)
claw


# Plot grid ----
fig2 <- plot_grid(claw, sat_plot, b2_plot, ncol = 3, 
                  labels = "AUTO", 
                  align = "vh",
                  label_size = 15)
fig2

# Saving plot ----
ggsave(plot = fig2, 
       filename = "outputs/figures/04_Figure_2.png",
       width = 9.5, 
       height = 3, 
       dpi = 300)

# THE END ---------------------------------------------------------------------

