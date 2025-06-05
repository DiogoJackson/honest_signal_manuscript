# Instale o pacote survival, caso ainda não tenha instalado
#install.packages("survival")
#install.packages("survminer")

# Carregue o pacote survival e o conjunto de dados lung
library(survival)
library(survminer)
library(readxl)
library(gridExtra)
library(tidyverse)
library(cowplot)

#import data ----
data_km <- read_excel("data/raw/data_fight.xlsx",
                         sheet = "Kaplan-meier")
head(data_km)

#fit model ----
kmcurve <- survfit(Surv(tempo_s, toca) ~ tipo_quela, data = data_km)
summary(kmcurve, times=seq(0, 600, 100))

#Plot survivel curve ----
p <- ggsurvplot(kmcurve,
           xlim = c(0, 600),  break.x.by = 100,
           linetype = 1,
           ylab = "Aquisição de toca (%)", xlab = "Tempo (s)",
           pval = TRUE, 
           risk.table = FALSE,
           cumevents = FALSE,
           cumcensor = FALSE,
           risk.table.title = "",
           legend.labs = c("Brachychelous", "Leptochelous"), 
           legend.title = "",  
           surv.scale = "percent", 
           palette = c("#ffb560","#18617b"),
           title="",  risk.table.height=.25,
          fun = "event") #fun = event faz com que a curva seja plotada uphill e nao downhill
p

#Plot barplot ----
toca <- data_km %>%
  dplyr::count(aquisicao_de_toca, tipo_quela)
toca

p2 <- ggplot(data = toca, aes(x = aquisicao_de_toca, y = n, fill = tipo_quela)) + 
  geom_bar(position = "dodge",stat = "identity") +
  geom_text(aes(label=n), position=position_dodge(width=0.9), vjust=-0.25)+
  scale_fill_manual(values = c("#ffb560","#18617b"))+
  scale_x_discrete(limits = c('combate','roubo','toca_vazia','sem_toca'),
                   labels = c('Combat', 'Theft', 'Empty burrow', 'No burrow')) +
  
  labs(y = "Count",
       x = "Type of burrow acquisition")+
  theme_classic(base_size = 16)
p2

toca <- data_km %>%
  dplyr::count(aquisicao_de_toca, tipo_quela) %>%
  group_by(aquisicao_de_toca) %>%
  mutate(percent = n / sum(n) * 100) %>% 
  mutate(tipo_quela = fct_recode(tipo_quela,
                                 "Brachychelous" = "brac",
                                 "Leptochelous" = "lep"))

p2.2 <- ggplot(data = toca, aes(x = aquisicao_de_toca, y = percent, fill = tipo_quela)) + 
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
p2.2

# Proporções de 'Combate' entre 'brac' e 'lep'
prop_test <- prop.test(x = c(11, 3), n = c(30, 30))  # Supondo 14 de cada grupo
prop_test

# Proporções de 'Theft' entre 'brac' e 'lep'
prop_test2 <- prop.test(x = c(4, 5), n = c(30, 30))  # Supondo 14 de cada grupo
prop_test2

# Proporções de 'Theft' entre 'brac' e 'lep'
prop_test3 <- prop.test(x = c(7, 8), n = c(30, 30))  # Supondo 14 de cada grupo
prop_test3

# Proporções de 'No borrow' entre 'brac' e 'lep'
prop_test4 <- prop.test(x = c(10, 15), n = c(30, 30))  # Supondo 14 de cada grupo
prop_test4


#Save plots ----

#p1 ----
png(file = "outputs/figures/survival_curve.png",
    res = 300,
    units = "in",
    width = 4.5,
    height = 4)

ggsurvplot(kmcurve,
           xlim = c(0, 600),  break.x.by = 100,
           linetype = 1,
           ylab = "Aquisição de toca (%)", xlab = "Tempo (s)",
           pval = TRUE, 
           risk.table = FALSE,
           cumevents = FALSE,
           cumcensor = FALSE,
           risk.table.title = "",
           legend.labs = c("Brachychelous", "Leptochelous"), 
           legend.title = "",  
           surv.scale = "percent", 
           palette = c("#ffb560","#18617b"),
           title="",  risk.table.height=.25,
           fun = "event") #fun = event faz com que a curva seja plotada uphill e nao downhill

dev.off()

#plot grid ----
fig <- plot_grid(p2.2, p2.3, ncol = 2, 
                  labels = "AUTO", 
                  align = "vh",
                  label_size = 16)
fig

#p2 ----
ggsave(plot = p2, 
       filename = "outputs/figures/barplot.png",
       width = 8, 
       height = 5, 
       dpi = 300)

ggsave(plot = p2.2, 
       filename = "outputs/figures/barplot_percent.png",
       width = 8, 
       height = 5, 
       dpi = 300)

ggsave(plot = fig, 
       filename = "outputs/figures/fig_competition.png",
       width = 10, 
       height = 5, 
       dpi = 300)

###########################
png(file = "outputs/figures/Fig6_competition.png",
    res = 300,
    units = "in",
    width = 14,
    height = 6)

grid.arrange(p$plot, 
             p2.2, 
             ncol = 2)
dev.off()
# FIM ---------------------------------------------------------------------


