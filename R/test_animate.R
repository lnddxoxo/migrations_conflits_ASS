library(gganimate)
library(gifski)

# Fusion avec le fond de carte
shp_anim <- shp_ssa %>% 
  left_join(master_panel, by = c("ISO_A3" = "iso3"))

# Création de l'animation
anim <- ggplot(shp_anim) +
  geom_sf(aes(fill = cluster), color = "white", size = 0.1) +
  scale_fill_manual(values = c("HH"="#C1440E", "LL"="#2D5016", "NS"="#F5ECD7")) +
  theme_void() +
  transition_time(as.integer(annee)) +
  labs(title = "Évolution spatiale des conflits - Année : {frame_time}")

# Sauvegarde
anim_save("outputs/figures/test_animation_LISA.gif", anim, renderer = gifski_renderer())
cat("Animation générée dans outputs/figures/ ✓\n")