#Script para fazer os graficos das diferenças de carapaça nupcial e escura
#Author: Diogo Silva
# Fri Dec 23 17:40:13 2022 ------------------------------

#HALFEYE

#Packages ----
library(tidyverse)
library(cowplot)
library(ggpubr)
library(ggdist)

#1. import data ----
data <- read.csv("data/processed/04_data_master.csv")
head(data)
str(data)

#2. Analisando tipo de quela ------------------------------------------------------
#subset da modelagem visual do caranguejo considerando apenas os valores de quela
data_qt <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw") #qt = quela type

data_brac <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw") %>% 
  filter(claw_type == "Brachychelous") %>% 
  mutate(max_force_log = log(max_force))

data_cara <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Carapace") %>% 
  filter(claw_type == "Brachychelous")

head(data_qt) 
tail(data_qt)


#Comparations ----
group <- list(c("Bright","Dark"))
stat <- stat_compare_means(comparisons = group, tip.length=0.01,
                           label = "p.signif", 
                           symnum.args = list(cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, 1), 
                                              symbols = c("****", "***", "**", "*", "ns")))

#3. Test t - nupcial color ----

#Claw ----
# Size difference ----
wilcox.test(data_qt$size ~ data_qt$carapace_color)

size <- ggplot(data_brac, aes(carapace_color, size, fill = carapace_color))+
  stat_halfeye(alpha = 0.5, justification = -0.2, width = 0.5, .width = 0, adjust = 0.4)+
  stat_dots(side = "left",justification = 1.1, binwidth = 0.2)+
  geom_boxplot(width = 0.1, show.legend = F)+
  scale_fill_manual(values=c("#d9e4eb", 
                             "#9ebbcb"))+
  labs(x = "",
       y = "Claw size (mm)")+
  theme_classic(base_size = 16)+
  theme(legend.position = "none")+
  coord_flip()+
  stat
size

#Weight diferences ----
wilcox.test(data_brac$weight_g ~ data_brac$carapace_color)

weight <- ggplot(data_brac, aes(carapace_color, weight_mg, fill = carapace_color))+
  stat_halfeye(alpha = 0.5, justification = 0, width = 0.5)+
  geom_boxplot(width = 0.25, show.legend = F)+
  geom_jitter(alpha = 0.3, size = 2.5,position = position_jitter(width = 0.05))+
  scale_fill_manual(values=c("#d9e4eb", 
                             "#9ebbcb"))+
  labs(x = "Carapace coloration",
       y = "Claw weigth (mg)")+
  theme_classic(base_size = 16)+
  theme(legend.position = "none")+
  stat
weight

#Force difference ----
#Filtering ----
data_brac_filted <- data_brac %>% 
  filter(size > 18) %>% 
  filter(max_force > 3)

wilcox.test(data_brac$max_force ~ data_brac$carapace_color)
wilcox.test(data_brac_filted$max_force ~ data_brac_filted$carapace_color)

str <- ggplot(data_brac, aes(carapace_color, max_force, fill = carapace_color))+
  stat_halfeye(alpha = 0.5, justification = 0, width = 0.6, adjust = 1)+
  geom_boxplot(width = 0.25, show.legend = F)+
  scale_fill_manual(values=c("#d9e4eb", 
                             "#9ebbcb"))+
  labs(x = "Carapace coloration",
       y = "Maximum strength (N)")+
  geom_jitter(size = 2.5, alpha = 0.5, width = 0.06,show.legend = F)+
  theme_classic(base_size = 16)+
  theme(legend.position = "none")+
  stat
str

###########################
size <- ggplot(data_brac, aes(carapace_color, size, fill = carapace_color))+
  stat_halfeye(alpha = 0.5, justification = 0, width = 0.5, adjust = 0.4)+
  stat_dots(side = "left",justification = 1.1, binwidth = 0.2)+
  geom_boxplot(width = 0.1, show.legend = F)+
  scale_fill_manual(values=c("#d9e4eb", 
                             "#9ebbcb"))+
  labs(x = "",
       y = "Claw size (mm)")+
  theme_classic(base_size = 16)+
  theme(legend.position = "none")+
  coord_flip()+
  stat
size

#########################
str2 <- ggplot(data_brac_filted, aes(carapace_color, max_force, fill = carapace_color))+
  geom_boxplot(width = 0.4, show.legend = F)+
  scale_fill_manual(values=c("white", 
                             "#999999"))+
  labs(x = "Carapace coloration",
       y = "Maximum strength (N)")+
  geom_jitter(size = 1.5, alpha = 0.5, width = 0.06,show.legend = F)+
  theme_classic()+
  stat
str2

#Saturation ----
wilcox.test(data_brac$luminance ~ data_brac$carapace_color)

sat <- ggplot(data_brac, aes(carapace_color, saturation, fill = carapace_color))+
  geom_boxplot(width = 0.4, show.legend = F)+
  scale_fill_manual(values=c("white", 
                             "#999999"))+
  labs(x = "Carapace coloration",
       y = "Claw saturation")+
  geom_jitter(size = 1.5, alpha = 0.5, width = 0.06,show.legend = F)+
  theme_classic()+
  stat
sat

#Luminance ----
wilcox.test(data_brac$luminance ~ data_brac$carapace_color)

lum <- ggplot(data_brac, aes(carapace_color, luminance, fill = carapace_color))+
  geom_boxplot(width = 0.4, show.legend = F)+
  scale_fill_manual(values=c("white", 
                             "#999999"))+
  labs(x = "Carapace coloration",
       y = "Claw luminance")+
  geom_jitter(size = 1.5, alpha = 0.5, width = 0.06,show.legend = F)+
  theme_classic()+
  stat
lum

#Chromatic contrast ----
wilcox.test(data_brac$chromatic_contrast ~ data_brac$carapace_color)

ds <- ggplot(data_brac, aes(carapace_color, chromatic_contrast, fill = carapace_color))+
  geom_boxplot(width = 0.4, show.legend = F)+
  scale_fill_manual(values=c("white", 
                             "#999999"))+
  labs(x = "Carapace coloration",
       y = "Claw - Chromatic contrast")+
  geom_jitter(size = 1.5, alpha = 0.5, width = 0.06,show.legend = F)+
  theme_classic()+
  stat
ds

#Brightness ----
wilcox.test(data_brac$mean_brightness ~ data_brac$carapace_color)

b2 <- ggplot(data_brac, aes(carapace_color, mean_brightness, fill = carapace_color))+
  geom_boxplot(width = 0.4, show.legend = F)+
  scale_fill_manual(values=c("white", 
                             "#999999"))+
  labs(x = "Carapace coloration",
       y = "Mean brightness")+
  geom_jitter(size = 1.5, alpha = 0.5, width = 0.06,show.legend = F)+
  theme_classic()+
  stat
b2

#Carapace brightness ----
wilcox.test(data_cara$mean_brightness ~ data_cara$carapace_color)

b2c <- ggplot(data_cara, aes(carapace_color, mean_brightness, fill = carapace_color))+
  geom_boxplot(width = 0.4, show.legend = F)+
  scale_fill_manual(values=c("white", 
                             "#999999"))+
  labs(x = "Carapace coloration",
       y = "Mean brightness of carapace")+
  geom_jitter(size = 1.5, alpha = 0.5, width = 0.06,show.legend = F)+
  theme_classic()+
  stat
b2c

#Carapace luminance ----
wilcox.test(data_cara$luminance ~ data_cara$carapace_color)

lumc <- ggplot(data_cara, aes(carapace_color, luminance, fill = carapace_color))+
  geom_boxplot(width = 0.4, show.legend = F)+
  scale_fill_manual(values=c("white", 
                             "#999999"))+
  labs(x = "Carapace coloration",
       y = "Carapace luminance")+
  geom_jitter(size = 1.5, alpha = 0.5, width = 0.06,show.legend = F)+
  theme_classic()+
  stat
lumc

#Carapace chromatic contrast ----
wilcox.test(data_cara$luminance ~ data_cara$carapace_color)

dsc <- ggplot(data_cara, aes(carapace_color, chromatic_contrast, fill = carapace_color))+
  geom_boxplot(width = 0.4, show.legend = F)+
  scale_fill_manual(values=c("white", 
                             "#999999"))+
  labs(x = "Carapace coloration",
       y = "Carapace - Chromatic contrast")+
  geom_jitter(size = 1.5, alpha = 0.5, width = 0.06,show.legend = F)+
  theme_classic()+
  stat
dsc

#GLM ---------------------------------------------------------------------------
#Peso ----
m <- glm(weight_g ~ size + carapace_color, family = "gaussian",
         data = data_brac)
summary(m)

sw <- ggplot(data_brac, aes(size, weight_g, color = carapace_color))+
  geom_point(alpha = 0.9)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("orange", "#010b13"))+
  labs(x = "Claw size (mm)",
       y = "Claw weight (g)",
       color = "Carapace coloration")+
  theme_classic()+
  theme(legend.position = "top")
sw

#Strength ----
#Sem ajuste ----
m2 <- glm(max_force ~ size + carapace_color, family = "gaussian",
          data = data_brac)
summary(m2)

ss <- ggplot(data_brac, aes(size, max_force, color = carapace_color))+
  geom_point(alpha = 0.9)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("orange", "#010b13"))+
  labs(x = "Claw size (mm)",
       y = "Maximum strength (N)",
       color = "Carapace coloration")+
  theme_classic()+
  theme(legend.position = "top")
ss

#Com ajuste ----
m2.2 <- glm(max_force ~ size + carapace_color, family = "gaussian",
            data = data_brac_filted)
summary(m2.2)

ss2 <- ggplot(data_brac_filted, aes(size, max_force, color = carapace_color))+
  geom_point(alpha = 0.9)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("orange", "#010b13"))+
  labs(x = "Claw size (mm)",
       y = "Maximum strength (N)",
       color = "Carapace coloration")+
  theme_classic()+
  theme(legend.position = "top")
ss2

#forca e peso ----
#sem ajuste ----
m3 <- glm(weight_g ~ max_force  + carapace_color, family = "gaussian",
          data = data_brac)
summary(m3)

strw <- ggplot(data_brac, aes(max_force, weight_mg, color = carapace_color))+
  geom_point(alpha = 0.9)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("orange", "#010b13"))+
  labs(x = "Maximum strength (N)",
       y = "Weight (mg)",
       color = "Carapace coloration")+
  theme_classic()+
  theme(legend.position = "top")
strw

#com ajuste ----
m3.2 <- glm(max_force ~ weight_g + carapace_color, family = "poisson",
            data = data_brac_filted)
summary(m3.2)

strw2 <- ggplot(data_brac_filted, aes(weight_mg, max_force, color = carapace_color))+
  geom_point(alpha = 0.9)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("orange", "#010b13"))+
  labs(x = "Weigth (mg)",
       y = "Maximum strength (N)",
       color = "Carapace coloration")+
  theme_classic()+
  theme(legend.position = "top")
strw2

#Mean brightness ----
m4 <- glm(mean_brightness ~ weight_mg + size + max_force, family = "gaussian",
          data = data_brac)
summary(m4)

pp4 <- ggplot(data_brac, aes(weight_mg, mean_brightness))+
  geom_point()+
  geom_smooth(method = "glm")
pp4

pp4.2 <- ggplot(data_brac, aes(max_force, mean_brightness))+
  geom_point()+
  geom_smooth(method = "glm")
pp4.2

#Carapace----
m5 <- glm(mean_brightness ~ weight_g + size + max_force, family = "gaussian",
          data = data_brac)
summary(m5)

pp5 <- ggplot(data_cara, aes(mean_brightness, weight_mg))+
  geom_point()+
  geom_smooth(method = "glm")
pp5

#Figure 1 ----
f1 <- plot_grid(size, str, weight, sw, ncol = 2, 
                labels = "AUTO", 
                align = "vh",
                label_size = 15)
f1

f2 <- plot_grid(lum, lumc, ds, dsc, ncol = 2, 
                labels = "AUTO", 
                align = "vh",
                label_size = 15)
f2

ggsave(plot = f1, 
       filename = "outputs/figures/04_carapace-color_status.png",
       width = 7, 
       height = 6, 
       dpi = 300)

ggsave(plot = f2, 
       filename = "outputs/figures/04_carapace-color_color.png",
       width = 7, 
       height = 6, 
       dpi = 300)

# FIM ---------------------------------------------------------------------


