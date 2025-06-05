#Script para fazer os graficos das diferenças de carapaça nupcial e escura
#Author: Diogo Silva
# Fri Dec 23 17:40:13 2022 ------------------------------

#Packages ----
library(tidyverse)
library(cowplot)
library(ggpubr)
library(glmmTMB) #beta regression
library(foreign) #Vif
library(car)
library(fitdistrplus)
library(randomForest)
library(caret)
library(AER)

#1. import data ----
data <- read.csv("data/processed/04_data_master.csv")
glimpse(data)

#1.1 convertendo dados
data$max_force <- as.numeric(data$max_force)

#transformando em beta ----

#Se os dados apresentarem distribuicao beta, mas possuir algum valor que nao esteja entre 0 e 1.
#Transforma-se em beta usando-se a formula abaixo:

# beta = xi / (xmax + constante)

#xi = cada valor
#xmax = valor maximo da variavel (nesse caso forca)
#constante = valor criado 0.001

data.b <- data %>%
  dplyr::mutate(force.b = max_force/(max(max_force) + 0.001))

#2. Analisando tipo de quela ------------------------------------------------------
#subset da modelagem visual do caranguejo considerando apenas os valores de quela
data_qt <- data.b %>%  #qt = quela type
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw") %>% 
  filter(!is.na(weight_mg))

#Fitting model for claw weight -------------------------------------------------
#GLM peso da quela ----
m1 <- glm(weight_mg ~ size + claw_type + saturation + mean_brightness + luminance, 
          family = "gaussian", data = data_qt)

vif(m1) # colinearity among variables #1 = nao correlacionado, de 1 a 5 = moderadamente correlacionada, 

m1.2 <- glm(weight_mg ~ size + claw_type + saturation + mean_brightness, 
            family = "gaussian", data = data_qt)
vif(m1.2)

m1.3 <- glm(weight_mg ~ size + claw_type, 
            family = "gaussian", data = data_qt)

#Comparando modelos ----
anova(m1, m1.2, test = "LRT") # Apesar de luminance ter um efeito significativo no modelo, ela possui alta colinearidade com mean brightness, por tanto foi retirada do modelo.
anova(m1.2, m1.3, test = "LRT") # saturation e mean brightness nao possuem efeito significativo

#Avaliando os pressupostos do modelo selecionado ----

m1.3 <- glm(weight_mg ~ size + claw_type, 
            family = "gaussian", data = data_qt)

# O modelo para peso da quela usa a família gaussiana, então precisamos verificar:
# Normalidade dos resíduos
# Homocedasticidade (variância constante dos resíduos)
# Independência dos resíduos

# Resíduos padronizados
par(mfrow = c(2,2)) # Para visualizar múltiplos gráficos

# Resíduos vs. valores ajustados (ver homocedasticidade)
plot(m1.3, which = 1)

# Q-Q plot dos resíduos (ver normalidade)
qqnorm(resid(m1.3))
qqline(resid(m1.3))

# Histogram dos resíduos (ver forma da distribuição)
hist(resid(m1.3), breaks = 30, main = "Histograma dos Resíduos")

# Resíduos vs. ordem das observações (ver independência)
plot(resid(m1.3) ~ seq_along(resid(m1.3)), type = "p", main = "Resíduos ao longo das observações")
abline(h = 0, col = "red", lty = 2)

dev.off()

# Resultado da analise dos pressupostos ----

# O modelo parece atender aos pressupostos do GLM Gaussiano:
# ✔ Normalidade dos resíduos – Pequenas variações nas caudas, mas nada crítico.
# ✔ Homoscedasticidade – Resíduos distribuídos sem padrão claro.
# ✔ Ausência de autocorrelação – Resíduos aleatórios ao longo das observações.

#Modelo final:
m1.3 <- glm(weight_mg ~ size + claw_type, 
            family = "gaussian", data = data_qt)
summary(m1.3)

p1.3 <- ggplot(data_qt, aes(x = size, y = weight_mg, 
                            color = claw_type)) +
  geom_point(alpha = 0.8, size = 3) +
  geom_smooth(aes(color = claw_type), method = "glm", formula = y ~ x, linetype = 1, size = 1) +
  scale_color_manual(
    labels = c("Brachychelous", "Leptochelous"),
    values = c("#ffb560", "#18617b")) +
  labs(x = "Comprimento da quela (mm)",
       y = "Massa da quela (g)",
       color = "Tipo de quela")+
  theme_test(base_size = 20)
p1.3


# Fitting model for claw force -------------------------------------------------
# GLM peso da quela ----
m2.1 <- glm(max_force ~ weight_mg + size + claw_type + saturation + mean_brightness, 
            family = Gamma(link = "log"), data = data_qt)
summary(m1.1)
vif(m2.1) # colinearity among variables 

m2.2 <- glm(max_force ~ weight_mg + size + claw_type, 
            family = Gamma(link = "log"), data = data_qt)
summary(m2.2)

# Comparando modelos ----
anova(m2.1, m2.2, test = "LRT") 

# Avaliando os pressupostos do modelo selecionado ----
# Modelo GLM Gamma ----
m2.2 <- glm(max_force ~ weight_mg + size + claw_type, 
            family = Gamma(link = "log"), data = data_qt)
summary(m2.2)

# Diagnóstico do modelo ----

# 1. Resíduos vs Valores Ajustados
par(mfrow = c(2, 2))
plot(m1.2)  # gráficos padrão: resíduos vs ajustados, QQ, escala-location, etc.

# 2. Gráfico personalizado: Resíduos Deviance vs Valores Ajustados
residuals_dev <- residuals(m1.2, type = "deviance")
fitted_vals <- fitted(m1.3)

plot(fitted_vals, residuals_dev,
     xlab = "Valores Ajustados",
     ylab = "Resíduos Deviance",
     main = "Resíduos vs Ajustados")
abline(h = 0, lty = 2)

# 3. QQ-Plot dos resíduos
qqnorm(residuals_dev, main = "QQ-Plot dos Resíduos Deviance")
qqline(residuals_dev)

# 4. Distância de Cook
cook <- cooks.distance(m1.3)
plot(cook, type = "h", main = "Distâncias de Cook",
     ylab = "Cook's Distance")
abline(h = 4/length(cook), col = "red", lty = 2)

# 5. Verificação da dispersão
disp <- summary(m1.3)$dispersion
cat("Dispersão estimada:", round(disp, 3), "\n")

# 6. Influência e alavancagem
influencePlot(m1.3, main = "Gráfico de Influência", id.method = "identify")

dev.off()

# Visualização da relação entre peso da quela e força ----
m2.2 <- glm(max_force ~ weight_mg + size + claw_type, 
            family = Gamma(link = "log"), data = data_qt)
summary(m2.2)

p2.6 <- ggplot(data_qt, aes(x = size, y = max_force, 
                            color = claw_type, size = weight_mg)) +
  geom_point(alpha = 0.8) +
  geom_smooth(method = "glm", aes(color = claw_type), size = 1) +
  scale_color_manual(
    labels = c("Brachychelous", "Leptochelous"),
    values = c("#ffb560", "#18617b")) +
  labs(
    x = "Comprimento da quela (mm)", 
    y = "Força máxima (N)", 
    color = "Tipo de quela",
    size = "Massa da quela (g)"
  ) +
  theme_test(base_size = 20)
p2.6

#Save plot ----
p <- plot_grid(p1.3, p2.6, 
               ncol = 1,
               align = "hv",
               labels = "AUTO")
p

ggsave("outputs/figures/figure_3.png",
       p,
       height = 10,
       width = 9)


# FIM ---------------------------------------------------------------------


