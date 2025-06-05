#analise das diferenças de carapaça nupcial e escura
#Author: Diogo Silva
# Fri Dec 23 17:40:13 2022 ------------------------------

#Packages ----
library(tidyverse)
library(cowplot)

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
  filter(claw_type == "Brachychelous") #qt = quela type

data_brac2 <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(claw_type == "Brachychelous")

data_cara <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Carapace") %>% 
  filter(claw_type == "Brachychelous")

#Estatistica descritiva ----
summary_stats <- data_brac %>% 
  group_by(carapace_color) %>% 
  get_summary_stats(size, type = "common")
summary_stats

#3. Test t - nupcial color ----

#Claw ----
boxplot(data_brac$size ~ data_brac$carapace_color,
        xlab = "Carapace color",
        ylab = "Claw size")
wilcox.test(data_qt$size ~ data_qt$carapace_color)

boxplot(data_brac$weight_g ~ data_brac$carapace_color,
        xlab = "Carapace color",
        ylab = "Claw weight (g)")
wilcox.test(data_brac$weight_g ~ data_brac$carapace_color)

boxplot(data_brac$max_force ~ data_brac$carapace_color,
        xlab = "Carapace color",
        ylab = "Max force")
wilcox.test(data_brac$max_force ~ data_brac$carapace_color)

boxplot(data_brac$saturation ~ data_brac$carapace_color,
        xlab = "Carapace color",
        ylab = "Claw saturation")
wilcox.test(data_brac$saturation ~ data_brac$carapace_color)

boxplot(data_brac$chromatic_contrast ~ data_brac$carapace_color,
        xlab = "Carapace color",
        ylab = "Chromatic contrast of claw")
wilcox.test(data_brac$chromatic_contrast ~ data_brac$carapace_color)

boxplot(data_brac$luminance ~ data_brac$carapace_color,
        xlab = "Carapace color",
        ylab = "Claw's luminance")
wilcox.test(data_brac$luminance ~ data_brac$carapace_color)

boxplot(data_brac$hue ~ data_brac$carapace_color,
        xlab = "Carapace color",
        ylab = "Claw's hue")
wilcox.test(data_brac$hue ~ data_brac$carapace_color)

boxplot(data_brac$mean_brightness ~ data_brac$carapace_color,
        xlab = "Carapace color",
        ylab = "Claw's brightness")
wilcox.test(data_brac$mean_brightness ~ data_brac$carapace_color)

#Carapace ----
boxplot(data_cara$mean_brightness ~ data_cara$carapace_color,
        xlab = "Carapace color",
        ylab = "Carapace's brightness")
wilcox.test(data_cara$mean_brightness ~ data_cara$carapace_color)

boxplot(data_cara$luminance ~ data_cara$carapace_color,
        xlab = "Carapace color",
        ylab = "Carapace's luminance")
wilcox.test(data_cara$luminance ~ data_cara$carapace_color)

boxplot(data_cara$size ~ data_cara$carapace_color,
        xlab = "Carapace color",
        ylab = "Carapace's size")
wilcox.test(data_cara$size ~ data_cara$carapace_color)

boxplot(data_cara$max_force ~ data_cara$carapace_color,
        xlab = "Carapace color",
        ylab = "Carapace's brightness")
wilcox.test(data_cara$mean_brightness ~ data_cara$carapace_color)

p0 <- ggplot(data_brac, aes(size, mean_brightness, color = carapace_color))+
  geom_point(size = 2)+
  geom_smooth(method = "lm")
p0

m <- lm(size ~ luminance + max_force + weight_g + carapace_color,
             data = data_brac)
summary(m)

# Analisando nupcial color ----

p <- ggplot(data_brac, aes(size, weight_g, color = carapace_color))+
  geom_point()+
  geom_smooth(method = "glm")
p

p2 <- ggplot(data_brac, aes(size, max_force, color = carapace_color))+
  geom_point()+
  geom_smooth(method = "glm")
p2

data_brac_1 <- data_brac %>% 
  filter(max_force > 3) %>% 
  filter(size > 18)
  
p2.2 <- ggplot(data_brac_1, aes(size, max_force, color = carapace_color))+
  geom_point()+
  geom_smooth(method = "glm")
p2.2

boxplot(data_brac_1$max_force ~ data_brac_1$carapace_color)
wilcox.test(data_brac_1$max_force ~ data_brac_1$carapace_color)

p3 <- ggplot(data_brac_1, aes(weight_g, max_force, color = carapace_color))+
  geom_point()+
  geom_smooth(method = "glm")
p3

p3 <- ggplot(data_brac, aes(size, mean_brightness, color = carapace_color))+
  geom_point()+
  geom_smooth(method = "glm")
p3

p4 <- ggplot(data_brac, aes(mean_brightness, max_force, color = carapace_color))+
  geom_point()+
  geom_smooth(method = "glm")
p4

p5 <- ggplot(data_cara, aes(size, max_force, color = carapace_color))+
  geom_point()+
  geom_smooth(method = "glm")
p5

p6 <- ggplot(data_cara, aes(mean_brightness, max_force, color = carapace_color))+
  geom_point()+
  geom_smooth(method = "glm")
p6

m <- glm(size ~ weight_g + carapace_color, family = "gaussian",
        data = data_brac)
summary(m)

m2 <- glm(size ~ max_force + carapace_color, family = "gaussian",
         data = data_brac)
summary(m2)

m3 <- glm(weight_g ~ max_force + carapace_color, family = "gaussian",
         data = data_brac)
summary(m3)

m4 <- glm(mean_brightness ~ max_force * weight_g + carapace_color, family = "gaussian",
          data = data_brac)
summary(m4)

m5 <- glm(mean_brightness ~ max_force + weight_g + size + carapace_color, family = "gaussian",
          data = data_cara)
summary(m5)

shapiro.test(data_brac$mean_brightness)
