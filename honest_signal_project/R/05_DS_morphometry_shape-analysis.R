# Script to analyse claw shape
# Author: Diogo Silva
# Date: Wed Dec 14 19:47:12 2022
# Last update: Fri Jun  6 14:58:57 2025

# Packages ----
library(StereoMorph)
library(geomorph)
library(psych)
library(lmPerm)
library(tidyverse)

# Import classifier data ----
# Taking only IDs from 1 to 200 to match the number of photos.
classific <- read.csv("data/processed/04_data_master.csv", sep = ",") %>% 
  dplyr::filter(body_region == "Claw") %>% 
  dplyr::filter(vismodel == "Fiddler crab")

nrow(classific)

# Importing photos ----
digitizeImages(image.file='morphometry/amostras',
               shapes.file ='morphometry/coordenadas', 
               landmarks.ref='morphometry/landmarks.txt')

# Converting coords ----
coords_sm <- readShapes('morphometry/coordenadas')
coords_gm <- readland.shapes(coords_sm)

gpa <- gpagen(coords_gm, ProcD = FALSE)
plot(gpa)

# Verifying outliers ----
outliers<- plotOutliers(gpa$coords, inspect.outliers = FALSE)
plot(outliers)

# PCA ----
alinhamento_coords<- two.d.array(gpa$coords)

pca <- prcomp(alinhamento_coords, retx=TRUE)
summary(pca)

df_forma <- data.frame(classific, pca$x[, 1:2])

# Plot ----
plot_pca <- ggplot(df_forma) + 
            aes(PC1, PC2, color=claw_type, shape=claw_type) + 
            geom_point (size=4) +
            theme_bw() + 
            labs (title="Titulo",x="PC1", y = "PC2") + 
            theme(legend.text = element_text (size = 15), legend.title = element_text (size = 15), axis.text = element_text(size = 15), axis.title = element_text(size = 15))

plot_pca

# Elipse plot ----
plot_pca_elipse <- ggplot(df_forma) + 
  aes(PC1, PC2, color = claw_type, shape = claw_type) + 
  scale_shape_manual(values = seq(0,21)) + 
  geom_point (aes(color = claw_type), size=4) +
  scale_color_manual(values = c("#ffb560","#18617b"))+
  stat_ellipse (geom = 'polygon', level = 0.90, alpha = 1/20, aes(fill=claw_type, group=claw_type)) +
  theme_classic(base_size = 18) +
  labs (title="", x="PC1", y = "PC2") + 
  theme(legend.position = "top", legend.text = element_text (size = 15), legend.title = element_text (size = 15), axis.text = element_text(size = 15), axis.title = element_text(size = 15))
plot_pca_elipse

# Plot grid ----
windows()
teste.pca <- gm.prcomp(gpa$coords)                
pleth.pca.plot <- plot(teste.pca)

picknplot.shape(pleth.pca.plot)

# Shape analysis (size-independent)  ----
anova_forma <- procD.lm(alinhamento_coords ~ df_forma$claw_type)
summary(anova_forma)

# Size analyses (shape-independent) ----
df_tamanho <- data.frame(classific, gpa$Csize)
summary(gpa$Csize)
describeBy(df_tamanho, group = df_tamanho$claw_type)

anova_tamanho <- aov(gpa$Csize ~ claw_type, data = df_tamanho, perm = 'Exact') 
summary (anova_tamanho)
TukeyHSD(anova_tamanho)

# Boxplot ----
boxplot_csize <- ggplot(data = df_tamanho, aes(x = claw_type, y = gpa.Csize)) + 
  geom_boxplot (aes(fill = claw_type)) + 
  theme_bw() + 
  labs (title="Título",x="Nome do eixo X", y = "Nome do eixo Y") + 
  theme (legend.text = element_text (size = 15), 
         legend.title = element_text (size = 15), 
         axis.text = element_text(size = 15), 
         axis.title = element_text(size = 15))
boxplot_csize

#Saving plot ----
ggsave(plot = plot_pca_elipse, 
       filename = "outputs/figures/05_claw-types_pca.png",
       width = 7, 
       height = 6, 
       dpi = 300)

# THE END -----------------------------------------------------------------

