# Packages ----------------------------------------------------------------
library(tidyverse)
library(pavo)
library(colorspec) #remotes::install_github("Diogojackson/colorspec/colorspec")

# 1.Import reflectance data -------------------------------------------------
#1.1. Import ll ----
reflet_ll <- read.csv("data/processed/00_refletancias_ll.csv")
reflet_ll <- fixspec(reflet_ll) %>% 
  select(-"X000_bkg_thayeri",
         -"X000_bkg_rapax",
         -"X000_bkg_panema",
         -"X000_bkg_maracoani"
  )


#Plot means ----
reflet_ll %>%
  rowwise() %>%
  
  mutate(media1 = mean(c_across(contains("_q_")), na.rm = TRUE),
         sd1 = sd(c_across(contains("_q_")), na.rm = TRUE)) %>%
  
  mutate(media2 = mean(c_across(contains("_c_")), na.rm = TRUE),
         sd2 = sd(c_across(contains("_c_")), na.rm = TRUE)) %>%
  
  mutate(media3 = mean(c_across(contains("_f_")), na.rm = TRUE),
         sd3 = sd(c_across(contains("_f_")), na.rm = TRUE)) %>%
  
  ungroup() %>%
  
  ggplot(aes(x = wl)) +
  
  geom_ribbon(aes(ymin = media1 - sd1, ymax = media1 + sd1, fill = "Claw"), alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = media1, color = "Claw")) +
  
  geom_ribbon(aes(ymin = media2 - sd2, ymax = media2 + sd2, fill = "Male carapace"), alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = media2, color = "Male carapace")) +
  
  geom_ribbon(aes(ymin = media3 - sd3, ymax = media3 + sd3, fill = "Female carapace"), alpha = 0.3, show.legend = FALSE) +
  geom_line(aes(y = media3, color = "Female carapace")) +
  
  scale_fill_manual(values = c("Claw" = "#FF0000", 
                               "Male carapace" = "#0000FF", 
                               "Female carapace" = "#00FF00"),name = "Groups") +
  scale_color_manual(values = c("Claw" = "#FF0000", 
                                "Male carapace" = "#0000FF", 
                                "Female carapace" = "#00FF00"), name = "Groups") +
  labs(title = expression(italic("L. leptodactyla")),
       x = "Wavelenght (nm)",
       y = "Reflectances (%)") +
  theme_minimal()


# Fim ---------------------------------------------------------------------


