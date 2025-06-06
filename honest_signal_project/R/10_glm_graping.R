#' GLM poisson
#' Referencia: https://www.youtube.com/watch?v=fDtj-9vBtng&t=337s
#'
# Mon May 15 15:06:43 2023 ------------------------------

#Packages ----
library(readxl)
library(tidyverse)
library(cowplot)
library(pscl)

#Import data ----
data_fight <- read_excel("data/raw/data_fight.xlsx")
head(data_fight)

data_km <- read_excel("data/raw/data_fight.xlsx",
                      sheet = "Kaplan-meier")

#Nesse caso, a variavel de taxa, a melhor distribuicao eh a gamma.
#porem como eu possuo valores de zero, o modelo so aceita valores positivos. Por isso, eu adicionei uma constante 0.001.

data_fight2 <- data_fight %>%
  mutate(
    roubo_min = tentativa_roubo / (tempo_s / 60) + 0.001,
    grappling_min = tentativa_graping / (tempo_s / 60) + 0.001,
  )

#Grafico
p1.1 <- ggplot(data_fight2, aes(quela1, grappling_min, color = tipo_quela1))+
  geom_point(size = 3, alpha = 0.9, show.legend = T)+
  geom_smooth(aes(color = tipo_quela1), method = "glm", linetype = 1, size = 1,show.legend = F)+
  scale_color_manual(labels = c("Brachychelous", "Leptochelous"),
                     values = c("#ffb560","#18617b"))+
  labs(x = "Claw size (mm)", 
       y = "Grappling per minute",
       color = "Claw type")+
  theme_test(base_size = 16)+
  theme(legend.position = "top")
p1.1

p1.2 <- ggplot(data_fight2, aes(quela1, grappling_min, color = tipo_quela1))+
  geom_point(size = 3, alpha = 0.9)+
  geom_smooth(aes(color = tipo_quela1), method = "glm", linetype = 1, size = 1,show.legend = F)+
  scale_color_manual(labels = c("Brachychelous", "Leptochelous"),
                     values = c("#ffb560","#18617b"))+
  labs(x = "Claw size (mm)", 
       y = "Grappling per minute")+
  guides(color = guide_legend(title = "Claw type"))+
  theme_test(base_size = 20)+
  theme(legend.position = "top")
p1.2

m1 <- glm(grappling_min ~ quela1 + tipo_quela1, family = Gamma(link = "log"), data = data_fight2)
summary(m1)

####################################################################
# GLM Tentativa roubo -----#########################################
####################################################################

# Criando o gráfico

p2.1 <- ggplot(data_fight2, aes(quela1, roubo_min, color = tipo_quela1))+
  geom_point(size = 3, alpha = 0.9)+
  geom_smooth(aes(color = tipo_quela1), method = "glm", linetype = 1, size = 1)+
  scale_color_manual(labels = c("Brachychelous", "Leptochelous"),
                     values = c("#ffb560","#18617b"))+
  labs(x = "Claw size (mm)", y = "Robbery attempt per minute")+
  guides(color = guide_legend(title = "Claw type"))+
  theme_test(base_size = 20)+
  theme(legend.position = c(0.78, 0.85))+
  theme(legend.key.width = unit(1, "cm"))+
  theme(legend.box = "rect", 
        legend.box.background = element_rect(color = "black", fill = NA, size = 1),
        legend.title = element_text(size = 14),  # Tamanho do título da legenda
        legend.text = element_text(size = 12))
p2.1

m2 <- glm(roubo_min ~ tipo_quela1 + quela1, family = Gamma(link = "log"), data = data_fight2)
summary(m2)


#Proportion

#Plot barplot ----

toca <- data_km %>%
  dplyr::count(aquisicao_de_toca, tipo_quela) %>%
  group_by(aquisicao_de_toca) %>%
  mutate(percent = n / sum(n) * 100) %>% 
  mutate(tipo_quela = fct_recode(tipo_quela,
                                 "Brachychelous" = "brac",
                                 "Leptochelous" = "lep"))

p3.1 <- ggplot(data = toca, aes(x = aquisicao_de_toca, y = percent, fill = tipo_quela)) + 
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_dodge(width = 0.9), vjust = -0.25) +
  scale_fill_manual(values = c("#ffb560", "#18617b")) +
  scale_x_discrete(limits = c('combate', 'roubo', 'toca_vazia', 'sem_toca'),
                   labels = c('Combat', 'Theft', 'Empty burrow', 'No burrow')) +
  labs(y = "Percentage",
       x = "Type of burrow acquisition",
       fill = "Claw type") +
  theme_classic(base_size = 16)+
  theme(legend.position = "top")+
  annotate(geom = "text", label = "*", size = 7, x = 1, y = 100)
p3.1

#Save graphics
fig <- plot_grid(p3.1, p1.1, ncol = 2, 
                  labels = "AUTO", 
                  align = "vh",
                  label_size = 18)

fig

ggsave(plot = fig, 
       filename = "outputs/figures/competitive_strategy.png",
       width = 11, 
       height = 5.5, 
       dpi = 300)


##################################

