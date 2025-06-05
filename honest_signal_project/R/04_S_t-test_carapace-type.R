#Script para fazer os graficos das diferenças de carapaça nupcial e escura
#Author: Diogo Silva
# Fri Dec 23 17:40:13 2022 ------------------------------

#Packages ----
library(tidyverse)
library(cowplot)
library(ggpubr)
library(colorspec)
library(pavo)

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
  geom_boxplot(width = 0.4, show.legend = F)+
  scale_fill_manual(values=c("white", 
                             "grey80"))+
  labs(x = "Carapace coloration",
       y = "Claw size (mm)")+
  geom_jitter(size = 1.5, alpha = 0.5, width = 0.06,show.legend = F)+
  theme_classic()+
  stat
size

#Weight diferences ----
wilcox.test(data_brac$weight_g ~ data_brac$carapace_color)

weight <- ggplot(data_brac, aes(carapace_color, weight_mg, fill = carapace_color))+
  geom_boxplot(width = 0.4, show.legend = F)+
  scale_fill_manual(values=c("white", 
                             "grey75"))+
  labs(x = "Carapace coloration",
       y = "Claw weigth (mg)")+
  geom_jitter(size = 1.5, alpha = 0.5, width = 0.06,show.legend = F)+
  theme_classic()+
  stat
weight

#Force difference ----
#Filtering ----
wilcox.test(data_brac$max_force ~ data_brac$carapace_color)

str <- ggplot(data_brac, aes(carapace_color, max_force, fill = carapace_color))+
  geom_boxplot(width = 0.4, show.legend = F)+
  scale_fill_manual(values=c("white", 
                             "#999999"))+
  labs(x = "Carapace coloration",
       y = "Maximum strength (N)")+
  geom_jitter(size = 1.5, alpha = 0.5, width = 0.06,show.legend = F)+
  theme_classic()+
  stat
str

#Saturation ----
wilcox.test(data_brac$saturation ~ data_brac$carapace_color)

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
  theme_classic(base_size = 14)+
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

#Claw Brightness ----
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
  theme_classic(base_size = 14)+
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
shapiro.test(data_brac$weight_g) #normal gaussian

m1 <- glm(weight_g ~ size + carapace_color + max_force, family = "gaussian",
         data = data_brac)
summary(m)

m1.2 <- glm(weight_g ~ size + carapace_color + max_force, family = "gaussian",
         data = data_brac_f)
summary(m1.2)

weight_size <- ggplot(data_brac, aes(size, weight_g, color = carapace_color))+
  geom_point(alpha = 0.9)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("orange", "#010b13"))+
  labs(x = "Claw size (mm)",
       y = "Claw weight (g)",
       color = "Carapace coloration")+
  theme_classic()+
  theme(legend.position = "top")
weight_size

weight_str <- ggplot(data_brac, aes(max_force, weight_g, color = carapace_color))+
  geom_point(alpha = 0.9)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("orange", "#010b13"))+
  labs(x = "Maximum strenght (N)",
       y = "Claw weight (g)",
       color = "Carapace coloration")+
  theme_classic()+
  theme(legend.position = "top")
weight_str

weight_str_size <- ggplot(data_brac, aes(max_force, weight_g, color = carapace_color))+
  geom_point(alpha = 0.7, aes(size = size))+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("orange", "#010b13"))+
  labs(x = "Maximum strenght (N)",
       y = "Claw weight (g)",
       color = "Carapace coloration")+
  theme_classic(base_size = 24)
weight_str_size

weight_str_size_f <- ggplot(data_brac_f, aes(max_force, weight_g, color = carapace_color))+
  geom_point(alpha = 0.7, aes(size = size))+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("orange", "#010b13"))+
  labs(x = "Maximum strenght (N)",
       y = "Claw weight (g)",
       color = "Carapace coloration")+
  theme_classic(base_size = 24)
weight_str_size_f

#Strength ----

#Ver distribuicao da variavel resposta
shapiro.test(data_brac$max_force)
plot(density(data_brac$max_force))  #ver a densidade e a distribui??o dos dados
hist(data_brac$max_force)  #mesma coisa
f1 <- fitdist(data_brac$max_force, "gamma") #comando ta dentro do pacote fitdistrplus, ele mostra os residuos dos dados.
plot(f1, demp = TRUE)  #verificar se a distribui??o ? normal
summary(f1) #OBSERVAR o AIC para ver Qual distribui??o usar. Assim, quando na d?vida, rodar o AIC dos dois modelos e verifica o menor AIC, utilizar A distribui??o Do menor AIC.
descdist(data_brac$max_force, discrete=FALSE)

#Vamos utilizar a distribuicao GAMMA
#Filtrando maiores que 16 (pois menores que isso nao preensam direito a plaquinha)
data_brac_f <- data_brac %>% 
  filter(size > 16)

#Modelo incluindo tudo
m2.1 <- glm(max_force ~ weight_g + carapace_color + size, family = Gamma,
          data = data_brac)
summary(m2.1)

#Modelo tirando os pequenos
m2.2 <- glm(max_force ~ weight_g + carapace_color + size, family = Gamma,
         data = data_brac_f)
summary(m2.2)

#Graficos
str_weight_size_f <- ggplot(data_brac_f, aes(x = weight_g, y = max_force, color = carapace_color))+
  geom_point(alpha = 0.7, aes(size = size))+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("orange", "#010b13"))+
  labs(x = "Weigth (g)",
       y = "Maximum strength (N)",
       color = "Carapace coloration")+
  theme_classic(base_size = 24)
str_weight_size_f

ss <- ggplot(f, aes(x = size, y = max_force, color = carapace_color))+
  geom_point(alpha = 0.9)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("orange", "#010b13"))+
  labs(x = "Size (mm)",
       y = "Maximum strength (N)",
       color = "Carapace coloration")+
  theme_classic()+
  theme(legend.position = "top")
ss

#forca e peso ----
#sem ajuste ----
m3 <- glm(weight_g ~ max_force  + carapace_color, family = "gaussian",
            data = data_brac)
summary(m3)

wstr <- ggplot(data_brac, aes(x = max_force, y = weight_g, color = carapace_color))+
  geom_point(alpha = 0.9)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("orange", "#010b13"))+
  labs(x = "Maximum strength (N)",
       y = "Weight (mg)",
       color = "Carapace coloration")+
  theme_classic()+
  theme(legend.position = "top")
wstr

m4 <- glm(max_force ~ weight_g  + carapace_color, family = "gaussian",
          data = data_brac)
summary(m4)

strw <- ggplot(data_brac, aes(x = weight_mg, y = max_force, color = carapace_color))+
  geom_point(alpha = 0.9)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("orange", "#010b13"))+
  labs(x =  "Weight (mg)",
       y = "Maximum strength (N)",
       color = "Carapace coloration")+
  theme_classic()+
  theme(legend.position = "top")
strw

#refletancias
# 1.Import reflectance data -------------------------------------------------
#1.1. Import ----
reflet <- read.csv("data/processed/01_reflet_carapace_type.csv")
reflet <- fixspec(reflet)

#Plot means ----
cara <- reflet %>%
  rowwise() %>%
  
  mutate(media1 = mean(c_across(contains("Bright")), na.rm = TRUE),
         sd1 = sd(c_across(contains("Bright")), na.rm = TRUE)) %>%
  
  mutate(media2 = mean(c_across(contains("Dark")), na.rm = TRUE),
         sd2 = sd(c_across(contains("Dark")), na.rm = TRUE)) %>%
  
  ungroup() %>%
  
  ggplot(aes(x = wl)) +
  
  geom_ribbon(aes(ymin = media1 - sd1, ymax = media1 + sd1, fill = "Bright"), alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = media1, color = "Bright")) +
  
  geom_ribbon(aes(ymin = media2 - sd2, ymax = media2 + sd2, fill = "Dark"), alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = media2, color = "Dark")) +
  
  scale_fill_manual(values = c("Bright" = "orange", 
                               "Dark" = "black"),name = "Groups") +
  scale_color_manual(values = c("Bright" = "orange", 
                                "Dark" = "black"), name = "Groups") +
  labs(x = "Wavelenght (nm)",
       y = "Reflectance (%)") +
  theme_classic(base_size = 14)+
  theme(legend.position = "top")
cara

#1.1. Import claws reflectance ----
reflet2 <- read.csv("data/processed/01_reflet_claw_color.csv")
reflet2 <- fixspec(reflet2)

#Plot means ----
claw <- reflet2 %>%
  rowwise() %>%
  
  mutate(media1 = mean(c_across(contains("Bright")), na.rm = TRUE),
         sd1 = sd(c_across(contains("Bright")), na.rm = TRUE)) %>%
  
  mutate(media2 = mean(c_across(contains("Dark")), na.rm = TRUE),
         sd2 = sd(c_across(contains("Dark")), na.rm = TRUE)) %>%
  
  ungroup() %>%
  
  ggplot(aes(x = wl)) +
  
  geom_ribbon(aes(ymin = media1 - sd1, ymax = media1 + sd1, fill = "Bright"), alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = media1, color = "Bright")) +
  
  geom_ribbon(aes(ymin = media2 - sd2, ymax = media2 + sd2, fill = "Dark"), alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = media2, color = "Dark")) +
  
  scale_fill_manual(values = c("Bright" = "orange", 
                               "Dark" = "black"),name = "Groups") +
  scale_color_manual(values = c("Bright" = "orange", 
                                "Dark" = "black"), name = "Groups") +
  labs(x = "Wavelenght (nm)",
       y = "Reflectance (%)") +
  theme_classic(base_size = 14)+
  theme(legend.position = "top")
claw

#Figure 1 ----
fig2 <- plot_grid(lum, claw, lumc, cara,
                  ncol = 2,
                  labels = "AUTO",
                  align = "vh",
                  label_size = 15)
fig2

fig3 <- plot_grid(weight_str_size_f, str_weight_size_f,
                  ncol = 1,
                  labels = "AUTO",
                  align = "vh",
                  label_size = 15)
fig3

#Save plots ----
ggsave(plot = fig2, 
       filename = "outputs/figures/04_figure_2_lum.png",
       width = 7, 
       height = 6, 
       dpi = 300)

ggsave(plot = fig3, 
       filename = "outputs/figures/04_figure_3_glm.png",
       width = 10, 
       height = 12, 
       dpi = 300)

# FIM ---------------------------------------------------------------------


