# Script to run GLM analyses for force and claw weight
# Author: Diogo Silva
# Date: Fri Dec 23 17:40:13 2022 
# Last update: Thu Jun 05 17:25:48 2025 

# Packages ----
library(tidyverse)
library(car)

# 1. import data ----
data <- read.csv("data/processed/04_data_master.csv")
glimpse(data)

data_qt <- data %>%  #qt = quela type
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw")

# 2. Fitting model for claw weight ----------------------------------------------
m1.1 <- glm(weight_mg ~ size + claw_type + saturation + mean_brightness + luminance, 
          family = "gaussian", data = data_qt)

car::vif(m1.1) # colinearity among variables #1 = nao correlacionado, de 1 a 5 = moderadamente correlacionada, 

m1.2 <- glm(weight_mg ~ size + claw_type + saturation + mean_brightness, 
            family = "gaussian", data = data_qt)
car::vif(m1.2)
summary(m1.2)

m1.3 <- glm(weight_mg ~ size + claw_type, 
            family = "gaussian", data = data_qt)
summary(m1.3)

# 2.1. Comparando modelos ----
anova(m1.2, m1.3, test = "LRT") # saturation e mean brightness nao possuem efeito significativo

# 2.2. Modelo selecionado ----
m1.3 <- glm(weight_mg ~ size + claw_type, 
            family = "gaussian", data = data_qt)

# 2.3. Avaliando os pressupostos do modelo selecionado ----

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

# O modelo parece atender aos pressupostos do GLM Gaussiano:
# ✔ Normalidade dos resíduos – Pequenas variações nas caudas, mas nada crítico.
# ✔ Homoscedasticidade – Resíduos distribuídos sem padrão claro.
# ✔ Ausência de autocorrelação – Resíduos aleatórios ao longo das observações.

# 3. Fitting model for claw force -------------------------------------------------
m2.1 <- glm(max_force ~ weight_mg + size + claw_type + saturation + mean_brightness, 
            family = Gamma(link = "log"), data = data_qt)
summary(m1.1)
car::vif(m2.1) # colinearity among variables 

m2.2 <- glm(max_force ~ weight_mg + size + claw_type, 
            family = Gamma(link = "log"), data = data_qt)
summary(m2.2)

# 3.1. Comparando modelos ----
anova(m2.1, m2.2, test = "LRT") 

# 3.2. Avaliando os pressupostos do modelo selecionado ----
m2.2 <- glm(max_force ~ weight_mg + size + claw_type, 
            family = Gamma(link = "log"), data = data_qt)
summary(m2.2)

# 3.3. Diagnóstico do modelo ----

# Resíduos vs Valores Ajustados
par(mfrow = c(2, 2))
plot(m1.2)  # gráficos padrão: resíduos vs ajustados, QQ, escala-location, etc.

# Gráfico personalizado: Resíduos Deviance vs Valores Ajustados
residuals_dev <- residuals(m1.2, type = "deviance")
fitted_vals <- fitted(m1.3)

plot(fitted_vals, residuals_dev,
     xlab = "Valores Ajustados",
     ylab = "Resíduos Deviance",
     main = "Resíduos vs Ajustados")
abline(h = 0, lty = 2)

# QQ-Plot dos resíduos
qqnorm(residuals_dev, main = "QQ-Plot dos Resíduos Deviance")
qqline(residuals_dev)

# Distância de Cook
cook <- cooks.distance(m1.3)
plot(cook, type = "h", main = "Distâncias de Cook",
     ylab = "Cook's Distance")
abline(h = 4/length(cook), col = "red", lty = 2)

# Verificação da dispersão
disp <- summary(m1.3)$dispersion
cat("Dispersão estimada:", round(disp, 3), "\n")

# Influência e alavancagem
influencePlot(m1.3, main = "Gráfico de Influência", id.method = "identify")

dev.off()

# THE END---------------------------------------------------------------------
