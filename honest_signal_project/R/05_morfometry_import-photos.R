#script to analyse claw photos
#author: Diogo Silva
#Wed Dec 14 19:47:12 2022 ------------------------------
#Fonte: https://citogenetica.ufes.br/pt-br/morfometria-geometrica-no-r

#Packages ----
library(StereoMorph)
library(geomorph)
library(psych)
library(lmPerm)
library(tidyverse)

#Import data classificadores ----
#pegando apenas o ID de 1 a 200 para dar match na quantidade de fotos.
classific <- read.csv("data/processed/04_data_master.csv", sep = ",") %>% 
  dplyr::filter(body_region == "Claw") %>% 
  dplyr::filter(vismodel == "Fiddler crab")

nrow(classific)

#Import photos ----
digitizeImages(image.file='morfometria/amostras',
               shapes.file ='morfometria/coordenadas', 
               landmarks.ref='morfometria/landmarks.txt')

#convert coord ----
coords_sm <- readShapes('morfometria/coordenadas')
coords_gm <- readland.shapes(coords_sm)

gpa <- gpagen(coords_gm, ProcD = FALSE)
plot(gpa)

#verificar outliers ----
outliers<- plotOutliers(gpa$coords, inspect.outliers = FALSE)
plot(outliers)

#PCA----
alinhamento_coords<- two.d.array(gpa$coords)

pca <- prcomp(alinhamento_coords, retx=TRUE)
summary(pca)

df_forma <- data.frame(classific, pca$x[, 1:2])

#ggplot----
plot_pca <- ggplot(df_forma) + 
            aes(PC1, PC2, color=claw_type, shape=claw_type) + 
            geom_point (size=4) +
            theme_bw() + 
            labs (title="Titulo",x="PC1", y = "PC2") + 
            theme(legend.text = element_text (size = 15), legend.title = element_text (size = 15), axis.text = element_text(size = 15), axis.title = element_text(size = 15))

plot_pca

#grafico elipse ----
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

#plot grid ----
windows()
teste.pca <- gm.prcomp(gpa$coords)                
pleth.pca.plot <- plot(teste.pca)

picknplot.shape(pleth.pca.plot)

#Análises de forma isolada do tamanho ----
anova_forma <- procD.lm(alinhamento_coords ~ df_forma$claw_type)
summary(anova_forma)

#Analise par a par para mais de 2 grupos (nao precisa pois temos 2 grupos apenas - brac vs lep)
#gdf <- geomorph.data.frame(site=df_forma$grupo)
#gp <-  (gdf$site)
#pw <- pairwise(anova_forma, groups = gp, covariate = NULL)
#summary(pw, test.type = "dist", confidence = 0.95, stat.table = TRUE)

#Análises de tamanho isolado da forma ----
df_tamanho <- data.frame(classific, gpa$Csize)
summary(gpa$Csize)
describeBy(df_tamanho, group = df_tamanho$claw_type)

anova_tamanho <- aov(gpa$Csize ~ claw_type, data = df_tamanho, perm = 'Exact') 
summary (anova_tamanho)
TukeyHSD(anova_tamanho)

#Grafico
boxplot_csize <- ggplot(data = df_tamanho, aes(x = claw_type, y = gpa.Csize)) + 
  geom_boxplot (aes(fill = claw_type)) + 
  theme_bw() + 
  labs (title="Título",x="Nome do eixo X", y = "Nome do eixo Y") + 
  theme (legend.text = element_text (size = 15), 
         legend.title = element_text (size = 15), 
         axis.text = element_text(size = 15), 
         axis.title = element_text(size = 15))
boxplot_csize

#Save plots ----
#pca elipse ----
ggsave(plot = plot_pca_elipse, 
       filename = "outputs/figures/05_claw-types_pca.png",
       width = 7, 
       height = 6, 
       dpi = 300)

ggsave(plot = plot_pca_elipse, 
       filename = "outputs/figures/05_claw-types_pca.png",
       width = 8, 
       height = 6, 
       dpi = 300)
