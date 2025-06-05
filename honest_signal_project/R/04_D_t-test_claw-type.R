#Script to analyse data_master
#author: Diogo Silva
#data: Thu Jul 21 13:32:34 2022 

#options(scipen=999) #retirar notacao cientifica dos resultados de teste

#Packages ----
library(tidyverse)
library(broom)
library(rstatix)
library(cowplot)

#1. import data ----
data <- read.csv("data/processed/04_data_master.csv")
head(data)
str(data)

#2. Analisando tipo de quela ------------------------------------------------------
#subset da modelagem visual do caranguejo considerando apenas os valores de quela
data_qt <- data %>% 
  filter(vismodel == "Fiddler crab") %>% 
  filter(body_region == "Claw")

head(data_qt) #qt = quela type
tail(data_qt)

#2.1. analisando normalidade dos dados ----------------------------------------------
shapiro_ds     <- shapiro.test(data_qt$chromatic_contrast)  #nao-normal
shapiro_max_f  <- shapiro.test(data_qt$max_force)           #nao-normal
shapiro_size   <- shapiro.test(data_qt$size)                #nao-normal
shapiro_weight <- shapiro.test(data_qt$weight_g)           #normal
shapiro_lum    <- shapiro.test(data_qt$luminance)                 #quase-normal, p = 0.03
shapiro_sat    <- shapiro.test(data_qt$saturation)          #nao-normal
shapiro_bright <- shapiro.test(data_qt$mean_brightness)     #normal
shapiro_hue    <- shapiro.test(data_qt$hue)                 #nao se aplica
shapiro_S1U    <- shapiro.test(data_qt$S1U) 
shapiro_S1V    <- shapiro.test(data_qt$S1V)
shapiro_S1B    <- shapiro.test(data_qt$S1B) 
shapiro_S1G    <- shapiro.test(data_qt$S1G) 
shapiro_S1Y    <- shapiro.test(data_qt$S1Y) 
shapiro_S1R    <- shapiro.test(data_qt$S1R) 

#2.2. transformando em tabela tidy ----
table_shapiro_ds     <- broom::tidy(shapiro_ds) 
table_shapiro_max_f  <- broom::tidy(shapiro_max_f)
table_shapiro_size   <- broom::tidy(shapiro_size)
table_shapiro_weight <- broom::tidy(shapiro_weight)
table_shapiro_lum    <- broom::tidy(shapiro_lum)
table_shapiro_sat    <- broom::tidy(shapiro_sat)
table_shapiro_bright <- broom::tidy(shapiro_bright)
table_shapiro_hue    <- broom::tidy(shapiro_hue)
table_shapiro_S1U    <- broom::tidy(shapiro_S1U)
table_shapiro_S1V    <- broom::tidy(shapiro_S1V)
table_shapiro_S1B    <- broom::tidy(shapiro_S1B)
table_shapiro_S1G    <- broom::tidy(shapiro_S1G)
table_shapiro_S1Y    <- broom::tidy(shapiro_S1Y)
table_shapiro_S1R    <- broom::tidy(shapiro_S1R)

#2.3. adicionando variavel ----
table_shapiro_ds['variable'] = "dS"
table_shapiro_max_f['variable'] = "max_force"
table_shapiro_size['variable'] = "size"
table_shapiro_weight['variable'] = "weight"
table_shapiro_lum['variable'] = "lum"
table_shapiro_sat['variable'] = "saturation"
table_shapiro_bright['variable'] = "mean_brightness"
table_shapiro_hue['variable'] = "hue"
table_shapiro_S1U['variable'] = "S1U"
table_shapiro_S1V['variable'] = "S1V"
table_shapiro_S1B['variable'] = "S1B"
table_shapiro_S1G['variable'] = "S1G"
table_shapiro_S1Y['variable'] = "S1Y"
table_shapiro_S1R['variable'] = "S1R"

#2.4. juntando resultados de shapiro em uma unica planilha ----
shapiro_table <- bind_rows(table_shapiro_ds,
                           table_shapiro_max_f,
                           table_shapiro_size,
                           table_shapiro_weight,
                           table_shapiro_lum,
                           table_shapiro_sat,
                           table_shapiro_bright,
                           table_shapiro_hue,
                           table_shapiro_S1U,
                           table_shapiro_S1V,
                           table_shapiro_S1B,
                           table_shapiro_S1G,
                           table_shapiro_S1Y,
                           table_shapiro_S1R) %>% 
  mutate(result = if_else(p.value > 0.05, "normal", "nao-normal"))

print(shapiro_table)

#3. estatistica descritiva ----
summary_stats_size <- data_qt %>% 
  group_by(claw_type) %>% 
  get_summary_stats(size, type = "common")

summary_stats_maxf <- data_qt %>% 
  group_by(claw_type) %>% 
  get_summary_stats(max_force, type = "common")

summary_stats_weight <- data_qt %>% 
  group_by(claw_type) %>% 
  get_summary_stats(weight_mg, type = "common")

summary_stats_ds <- data_qt %>% 
  group_by(claw_type) %>% 
  get_summary_stats(chromatic_contrast, type = "common")

summary_stats_lum <- data_qt %>% 
  group_by(claw_type) %>% 
  get_summary_stats(luminance, type = "common")

summary_stats_sat <- data_qt %>% 
  group_by(claw_type) %>% 
  get_summary_stats(saturation, type = "common")

summary_stats_bright <- data_qt %>% 
  group_by(claw_type) %>% 
  get_summary_stats(mean_brightness, type = "common")

summary_stats_S1U <- data_qt %>% 
  group_by(claw_type) %>% 
  get_summary_stats(S1U, type = "common")

summary_stats_S1V <- data_qt %>% 
  group_by(claw_type) %>% 
  get_summary_stats(S1V, type = "common")

summary_stats_S1B <- data_qt %>% 
  group_by(claw_type) %>% 
  get_summary_stats(S1B, type = "common")

summary_stats_S1G <- data_qt %>% 
  group_by(claw_type) %>% 
  get_summary_stats(S1G, type = "common")

summary_stats_S1Y <- data_qt %>% 
  group_by(claw_type) %>% 
  get_summary_stats(S1Y, type = "common")

summary_stats_S1R <- data_qt %>% 
  group_by(claw_type) %>% 
  get_summary_stats(S1R, type = "common")

#3.1. transformando em tabela tidy a estatistica descritiva ----
table_summary_statix <- bind_rows(summary_stats_size,
                                  summary_stats_maxf,
                                  summary_stats_weight,
                                  summary_stats_ds,
                                  summary_stats_lum,
                                  summary_stats_bright,
                                  summary_stats_sat,
                                  summary_stats_S1U,
                                  summary_stats_S1V,
                                  summary_stats_S1B,
                                  summary_stats_S1G,
                                  summary_stats_S1Y,
                                  summary_stats_S1R
                                  )
print(table_summary_statix)

#4. Test t - claw type ----
boxplot(data_qt$size ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Claw size")
wilcox.test(data_qt$size ~ data_qt$claw_type)

boxplot(data_qt$weight_g ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Claw weight (g)")
wilcox.test(data_qt$weight_g ~ data_qt$claw_type)

boxplot(data_qt$max_force ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Maximum force")
wilcox.test(data_qt$max_force ~ data_qt$claw_type)

boxplot(data_qt$luminance ~ data_qt$claw_type,
        xlab = "Claw_type",
        ylab = "Fiddler crab's luminance")
wilcox.test(data_qt$luminance ~ data_qt$claw_type)

boxplot(data_qt$chromatic_contrast ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Chromatic contrast (JND)")
wilcox.test(data_qt$chromatic_contrast ~ data_qt$claw_type)

boxplot(data_qt$saturation ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Saturation")
wilcox.test(data_qt$saturation ~ data_qt$claw_type)

boxplot(data_qt$mean_brightness ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Mean brightness")
wilcox.test(data_qt$mean_brightness ~ data_qt$claw_type)

boxplot(data_qt$hue ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Hue")
wilcox.test(data_qt$hue ~ data_qt$claw_type)

boxplot(data_qt$S1U ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Saturation UV")
wilcox.test(data_qt$S1U ~ data_qt$claw_type)

boxplot(data_qt$S1V ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Saturation V")
wilcox.test(data_qt$S1V ~ data_qt$claw_type)

boxplot(data_qt$S1B ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Saturation B")
wilcox.test(data_qt$S1B ~ data_qt$claw_type)

boxplot(data_qt$S1G ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Saturation G")
wilcox.test(data_qt$S1G ~ data_qt$claw_type)

boxplot(data_qt$S1Y ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Saturation Y")
wilcox.test(data_qt$S1Y ~ data_qt$claw_type)

boxplot(data_qt$S1R ~ data_qt$claw_type,
        xlab = "Claw type",
        ylab = "Saturation R")
wilcox.test(data_qt$S1R ~ data_qt$claw_type)

#ggplot ----
chr_plot <- ggplot(data_qt, aes(x = claw_type, y = chromatic_contrast)) +
  geom_boxplot(fill = "white") +
  geom_jitter(aes(color = claw_type), alpha = 0.5, size = 3, show.legend = FALSE, width = 0.1)+
  scale_color_manual(values = c("#63686a", "#63686a"))+
  theme_classic(base_size = 19) +
  labs(x = 'Tipo de quela', 
       y = 'Contraste cromático (dS)')
chr_plot

lum_plot <- ggplot(data_qt, aes(x = claw_type, y = luminance)) +
  geom_boxplot(fill = "white") +
  geom_jitter(color = "#63686a", alpha = 0.5, size = 3, show.legend = FALSE, width = 0.1)+
  theme_classic(base_size = 19) +
  labs(x = 'Tipo de quela', 
       y = 'Luminância')
lum_plot

brilho_plot <- ggplot(data_qt, aes(x = claw_type, y = mean_brightness))+
  geom_boxplot(fill = "white") +
  geom_jitter(color = "#63686a", alpha = 0.5, size = 3, show.legend = FALSE, width = 0.1)+
  theme_classic(base_size = 19) +
  labs(x = 'Tipo de quela', 
       y = 'Brilho médio')
brilho_plot

sat_plot <- ggplot(data_qt, aes(x = claw_type, y = saturation)) +
  geom_boxplot(fill = "white") +
  geom_jitter(aes(color = claw_type), alpha = 0.5, size = 3, show.legend = FALSE, width = 0.1)+
  scale_color_manual(values = c("#63686a", "#63686a"))+
  theme_classic(base_size = 19) +
  labs(x = 'Tipo de quela', 
       y = 'Saturação')
sat_plot

hue_plot <- ggplot(data_qt, aes(x = claw_type, y = hue)) +
  geom_boxplot(fill = "white") +
  geom_jitter(aes(color = claw_type), alpha = 0.5, size = 3, show.legend = FALSE, width = 0.1)+
  scale_color_manual(values = c("#63686a", "#63686a"))+
  theme_classic(base_size = 19) +
  labs(x = 'Tipo de quela', 
       y = 'Matiz')
hue_plot

size_plot <- ggplot(data_qt, aes(x = claw_type, y = size)) +
  geom_boxplot(fill = "white") +
  geom_jitter(color = "#63686a", alpha = 0.5, size = 3, show.legend = FALSE, width = 0.1)+
  theme_classic(base_size = 19) +
  labs(x = 'Tipo de quela', 
       y = 'Tamanho da quela (mm)')
size_plot

peso_plot <- ggplot(data_qt, aes(x = claw_type, y = weight_mg)) +
  geom_boxplot(fill = "white") +
  geom_jitter(aes(color = claw_type), alpha = 0.5, size = 3, show.legend = FALSE, width = 0.1)+
  scale_color_manual(values = c("#63686a", "#63686a"))+
  theme_classic(base_size = 19) +
  labs(x = 'Tipo de quela', 
       y = 'Peso seco (mg)')
peso_plot

force_plot <- ggplot(data_qt, aes(x = claw_type, y = max_force)) +
  geom_boxplot(fill = "white") +
  geom_jitter(aes(color = claw_type), alpha = 0.5, size = 3, show.legend = FALSE, width = 0.1)+
  scale_color_manual(values = c("#63686a", "#63686a"))+
  theme_classic(base_size = 19) +
  labs(x = 'Tipo de quela', 
       y = 'Força de preensão (N)')
force_plot

#save ggplot ----
fig2 <- plot_grid(chr_plot, lum_plot, sat_plot, hue_plot, ncol = 2, 
                  labels = "AUTO", 
                  align = "vh",
                  label_size = 18)
fig2

fig3 <- plot_grid(size_plot, peso_plot, force_plot, ncol = 2, 
                  labels = "AUTO", 
                  align = "vh",
                  label_size = 18)
fig3

ggsave(plot = fig2, 
       filename = "outputs/artigo_ii/figures/04_claw-types_jitter2.png",
       width = 11, 
       height = 9, 
       dpi = 300)
ggsave(plot = fig3, 
       filename = "outputs/artigo_ii/figures/04_claw-types_jitter3.png",
       width = 11, 
       height = 9, 
       dpi = 300)


#Save table ----
write.csv(shapiro_table,
          "outputs/artigo_ii/tables/shapiro_table.csv",
          row.names = F)

#test table ----
test_shapiro_table <- read.csv("outputs/artigo_ii/tables/shapiro_table.csv")
head(test_shapiro_table)

# FIM ---------------------------------------------------------------------

