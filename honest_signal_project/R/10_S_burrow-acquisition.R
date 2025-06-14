# Instale o pacote survival, caso ainda não tenha instalado
#install.packages("survival")
#install.packages("survminer")

# Carregue o pacote survival e o conjunto de dados lung
library(readxl)
library(gridExtra)
library(tidyverse)
library(cowplot)

#Import data ----
data_fight <- read_excel("data/raw/data_fight.xlsx") %>% 
  select(ID,
         quela1, 
         carap1, 
         tentativa_roubo, 
         tentativa_graping, 
         aquisicao_de_toca, 
         tempo_s, 
         tipo_quela1) %>% 
  rename(claw_size = quela1,
         carapace_size = carap1,
         theft = tentativa_roubo,
         grappling = tentativa_graping,
         burrow_acquisition = aquisicao_de_toca,
         time_seconds = tempo_s,
         claw_type = tipo_quela1,
         )
head(data_fight)

data_km <- read_excel("data/raw/data_fight.xlsx",
                      sheet = "Kaplan-meier")
data_km

#Nesse caso, a variavel de taxa, a melhor distribuicao eh a gamma.
#porem como eu possuo valores de zero, o modelo so aceita valores positivos. Por isso, eu adicionei uma constante 0.001.

data_fight2 <- data_fight %>%
  mutate(
    theft_per_min = theft / (time_seconds / 60) + 0.001,
    grappling_per_min = grappling / (time_seconds / 60) + 0.001,
  )
head(data_fight2)

#Grafico
p1.1 <- ggplot(data_fight2, aes(claw_size, grappling_per_min, color = claw_type))+
  geom_point(size = 3, alpha = 0.9, show.legend = T)+
  geom_smooth(aes(color = claw_type), method = "glm", linetype = 1, size = 1,show.legend = F)+
  scale_color_manual(labels = c("Brachychelous", "Leptochelous"),
                     values = c("#ffb560","#18617b"))+
  labs(x = "Claw size (mm)", 
       y = "Grappling per minute",
       color = "Claw type")+
  theme_test(base_size = 16)+
  theme(legend.position = "top")
p1.1

m1 <- glm(grappling_per_min ~ claw_size + claw_type, family = Gamma(link = "log"), data = data_fight2)
summary(m1)

library(DHARMa)  # Se não tiver: install.packages("DHARMa")

sim_res <- simulateResiduals(m1)
plot(sim_res)  # Se os resíduos seguirem a linha reta, o modelo está OK
####################################################################
# GLM Tentativa roubo -----#########################################
####################################################################

# Criando o gráfico

p2.1 <- ggplot(data_fight2, aes(claw_size, theft_per_min, color = claw_type))+
  geom_point(size = 3, alpha = 0.9)+
  geom_smooth(aes(color = claw_type), method = "glm", linetype = 1, size = 1)+
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
       filename = "outputs/figures/Fig_8_competitive_strategy.png",
       width = 11, 
       height = 5.5, 
       dpi = 300)


# THE END -----------------------------------------------------------------
