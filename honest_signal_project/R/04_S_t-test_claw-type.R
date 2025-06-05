# Script para criacao dos graficos
# Author: Diogo Silva
# Data: Wed May 17 13:24:28 2023
# Last update:
# Wed May 17 13:24:42 2023 ------------------------------

#Packages ----
library(tidyverse)
library(cowplot)
library(mytools)
library(ggpubr)

#1. import data ----
data <- read.csv("data/processed/04_data_master.csv")
head(data)
str(data)

data_qt <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw")

data_brac <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw") %>% 
  filter(claw_type ==  "Brachychelous")

data_brac_filted <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw") %>% 
  filter(claw_type ==  "Brachychelous") %>% 
  filter(max_force > 3)

#Comparations ----
group <- list(c("Brachychelous","Leptochelous"))
stat <- stat_compare_means(comparisons = group, tip.length=0.01,
                           label = "p.signif", 
                           symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                              symbols = c("****", "***", "**", "*", "ns")))

#my_colors
my_colors <- c("#ffb560","#18617b")

#ggplot ----
ds_plot <- ggplot(data_qt, aes(x = claw_type, y = chromatic_contrast)) +
  geom_boxplot(fill = "white", width = 0.4) +
  geom_jitter(aes(color = claw_type), alpha = 0.5, size = 1.5, show.legend = FALSE, width = 0.06)+
  scale_color_manual(values = c(my_colors))+
  labs(x = 'Claw type', 
       y = 'Contraste cromático (dS)')+
  theme_classic(base_size = 13) +
  stat
ds_plot

lum_plot <- ggplot(data_qt, aes(x = claw_type, y = luminance)) +
  geom_boxplot(fill = "white", width = 0.4) +
  geom_jitter(aes(color = claw_type), alpha = 0.5, size = 1.5, show.legend = FALSE, width = 0.06)+
  scale_color_manual(values = c(my_colors))+
  labs(x = 'Claw type', 
       y = 'Luminance')+
  theme_classic(base_size = 13) +
  stat
lum_plot

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

size_plot <- ggplot(data_qt, aes(x = claw_type, y = size)) +
  geom_boxplot(fill = "white", width = 0.4) +
  geom_jitter(aes(color = claw_type), alpha = 0.5, size = 1.5, show.legend = FALSE, width = 0.06)+
  scale_color_manual(values = c(my_colors))+
  labs(x = 'Claw type', 
       y = 'Claw size (mm)')+
  theme_classic(base_size = 13)+
  stat
size_plot

weight_plot <- ggplot(data_qt, aes(x = claw_type, y = weight_g)) + #esta escrito errado, grama eh miligrama.
  geom_boxplot(fill = "white", width = 0.4) +
  geom_jitter(aes(color = claw_type), alpha = 0.5, size = 1.5, show.legend = FALSE, width = 0.06)+
  scale_color_manual(values = c(my_colors))+
  labs(x = 'Claw type', 
       y = 'Claw weight (mg)')+
  theme_classic(base_size = 13) +
  stat
weight_plot

str_plot <- ggplot(data_qt, aes(x = claw_type, y = max_force)) +
  geom_boxplot(fill = "white", width = 0.4) +
  geom_jitter(aes(color = claw_type), alpha = 0.5, size = 1.5, show.legend = FALSE, width = 0.06)+
  scale_color_manual(values = c(my_colors))+
  labs(x = 'Claw type', 
       y = 'Maximum strength (N)')+
  theme_classic(base_size = 13) +
  stat
str_plot

sat_str <- ggplot(data_qt, aes(x = saturation, y = max_force, color = claw_type)) +
  geom_point(show.legend = F, alpha = 0.8) +
  geom_smooth(method = lm, show.legend = F)+
  scale_color_manual(values = c(my_colors))+
  labs(x = 'Saturation', 
       y = 'Maximum strength (N)')+
  theme_classic(base_size = 13)
sat_str

sat_g <- ggplot(data_qt, aes(x = saturation, y = weight_g, color = claw_type)) +
  geom_point(show.legend = F, alpha = 0.8) +
  geom_smooth(method = lm, show.legend = F)+
  scale_color_manual(values = c(my_colors))+
  labs(x = 'Saturation', 
       y = 'Claw weight (mg)')+
  theme_classic(base_size = 13)
sat_g

s1B_g <- ggplot(data_qt, aes(x = S1B, y = weight_g, color = claw_type)) +
  geom_point(show.legend = F, alpha = 0.8) +
  geom_smooth(method = lm, show.legend = F)+
  scale_color_manual(values = c(my_colors))+
  labs(x = 'Saturation', 
       y = 'Claw weight (mg)')+
  theme_classic(base_size = 13)
s1B_g

s1B_g2 <- ggplot(data_qt, aes(x = weight_g, y = S1R, color = claw_type)) +
  geom_point(show.legend = F, alpha = 0.8) +
  geom_smooth(method = lm, show.legend = F)+
  scale_color_manual(values = c(my_colors))+
  labs(x = 'Claw weight (mg)', 
       y = 'Saturation')+
  theme_classic(base_size = 13)
s1B_g2

m1 <- glm(max_force ~ saturation * claw_type, family = "gaussian", data = data_qt)
summary(m1)

m1.2 <- glm(max_force ~ saturation + claw_type, family = "gaussian", data = data_qt)
summary(m1.2)

m2 <- glm(weight_g ~ saturation * claw_type, family = "gaussian", data = data_qt)
summary(m2)

m2.2 <- glm(weight_g ~ saturation + claw_type, family = "gaussian", data = data_qt)
summary(m2.2)

#Plot mean reflectance ----

#1.1. Import claws reflectance ----
reflet <- read.csv("data/processed/01_reflet_claw_type.csv")
reflet <- fixspec(reflet)

claw <- reflet %>%
  rowwise() %>%
  
  mutate(media1 = mean(c_across(contains("Brachychelous")), na.rm = TRUE),
         sd1 = sd(c_across(contains("Brachychelous")), na.rm = TRUE)) %>%
  
  mutate(media2 = mean(c_across(contains("Leptochelous")), na.rm = TRUE),
         sd2 = sd(c_across(contains("Leptochelous")), na.rm = TRUE)) %>%
  
  ungroup() %>%
  
  ggplot(aes(x = wl)) +
  
  geom_ribbon(aes(ymin = media1 - sd1, ymax = media1 + sd1, fill = "Leptochelous"), alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = media1, color = "Leptochelous")) +
  
  geom_ribbon(aes(ymin = media2 - sd2, ymax = media2 + sd2, fill = "Brachychelous"), alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = media2, color = "Brachychelous")) +
  
  scale_fill_manual(values = c("Leptochelous" = "#18617b", 
                               "Brachychelous" = "#ffb560"),name = "") +
  scale_color_manual(values = c("Leptochelous" = "#18617b", 
                                "Brachychelous" = "#ffb560"), name = "") +
  labs(x = "Wavelenght (nm)",
       y = "Reflectance (%)") +
  theme_classic(base_size = 14)+
  theme(legend.position = "top")
claw

#save ggplot ----

#uq presentation
fig2 <- plot_grid(claw, sat_plot, ncol = 2, 
                  labels = "AUTO", 
                  align = "vh",
                  label_size = 15)
fig2

fig3 <- plot_grid(str_plot, weight_plot, sat_str, sat_g, ncol = 2, 
                  labels = "AUTO", 
                  align = "vh",
                  label_size = 15)
fig3

ggsave(plot = fig2, 
       filename = "outputs/figures/04_Fig02_reflet_sat.png",
       width = 7, 
       height = 3.5, 
       dpi = 300)

ggsave(plot = fig3, 
       filename = "outputs/figures/04_Fig03_cor_force.png",
       width = 7, 
       height = 6, 
       dpi = 300)
# FIM ---------------------------------------------------------------------

