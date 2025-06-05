#Analise dos dados de bia - preferencia por tipo de quela
#Diogo Silva
# Tue Jan 16 14:01:37 2024 ------------------------------

#last update:
# Tue Jan 16 14:01:44 2024 ------------------------------

#Packages ----
library(tidyverse)
library(cowplot)
library(ggpubr)
library(readxl)
library(car)

#1. import data ----
data <- read.csv("data/raw/dados_mestrado_bia.csv")
head(data)
str(data)

#2. Ajustando dataset ----
data_agg <- aggregate(female_choice ~ claw_type, 
                      FUN = sum, 
                      data = data)
data_agg

38/62*100

data_agg$female_choice <- as.numeric(data_agg$female_choice)

n1 <- data_agg[1,2]
n2 <- data_agg[2,2]

n <- n1 + n2
x <- max(n1, n2)

#3. Analise estatistica ----

#3.1. Analise binomial ----
binomial_1 <- binom.test(x, n) 
binomial_1 

#3.2. GLM binomial ----
dados_semNA <- remove_missing(data, vars = "claw_type") 

#Analisando multicolinearidade
m0 <- lm(female_choice ~ claw_type + claw_size + chromatic_contrast + achromatic_contrast + luminance + S8 + B2 + H1, data = dados_semNA)
vif(m0)

#Analisando multicolinearidade sem variaveis ----
m0.1 <- lm(female_choice ~ claw_type + claw_size + chromatic_contrast + S8 + B2 + H1, data = dados_semNA)
vif(m0.1)

#Fitting model ----
m1 <- glm(female_choice ~ claw_type + claw_size + chromatic_contrast + S8 + B2 + H1, data = dados_semNA, family = binomial(link = "logit"))
summary(m1)

m2 <- glm(female_choice ~ claw_type, data = dados_semNA, family = binomial(link = "logit"))
summary(m2)

#Resultado principal ----
m2 <- glm(female_choice ~ claw_type, data = dados_semNA, family = binomial(link = "logit"))
summary(m2)

anova(m1,m2, test = "LRT")
AIC(m1, m2) #de acordo com o AIC o modelo 2 se ajusta melhor. Portanto, esse sera nosso resultado.

#4. Grafico de barras ----
p1 <- ggplot(data = data_agg, aes(x = claw_type, y = female_choice, fill = claw_type)) +
  geom_bar(color = "black", stat = "identity", show.legend = F, width = 0.5) +
  geom_label(aes(label = female_choice), fill = "white") +
  scale_fill_manual(values = c("#ffb560","#18617b")) +
  scale_x_discrete(limits = c('Brachychelous','Leptochelous'))+
  labs(x = "Tipo de quela", 
       y = "Escolha da fêmea", 
       title = '')+
  theme_classic(base_size = 18)+
  annotate(geom = "text", x = 1.5, y = max(data_agg$female_choice) + 1, size = 7, label = "*")
p1

#Save ----
ggsave(plot = p1, 
       filename = "outputs/figures/05_claw-type_choice_pt.png",
       width = 6, 
       height = 5.5, 
       dpi = 300)

# FIM ---------------------------------------------------------------------
