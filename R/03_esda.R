setwd("D:/M1 DS 25 26/SEMESTRE 1/UE CULTURE GENERALE/ECONOMIE/econometrie/Econometric_projet/migrations_conflits_ASS")

library(tidyverse)
library(sf)
library(spdep)

panel    <- readRDS("data/processed/panel_clean.rds")
W1_listw <- readRDS("data/processed/W1_listw.rds")
shp_ssa  <- st_read("data/raw/ne_50m_admin_0_countries") %>%
  filter(ISO_A3 %in% unique(panel$iso3)) %>%
  arrange(ISO_A3)

pays_ordre <- shp_ssa$ISO_A3
panel_collecte <- list()

for(an in unique(panel$annee)) {
  data_an <- panel %>% 
    filter(annee == an) %>% 
    mutate(log_conflits = ifelse(is.na(log_conflits), 0, log_conflits)) %>%
    arrange(match(iso3, pays_ordre))
  
  set.seed(42)
  lisa_an <- localmoran_perm(data_an$log_conflits, W1_listw, nsim = 499, zero.policy = TRUE)
  
  z_an <- scale(data_an$log_conflits)[,1]
  wz_an <- lag.listw(W1_listw, z_an, zero.policy = TRUE)
  p_vals <- lisa_an[, "Pr(z != E(Ii))"]
  
  panel_collecte[[as.character(an)]] <- data.frame(
    iso3 = data_an$iso3,
    annee = an,
    conflits = data_an$log_conflits,
    migrants = data_an$log_migrants,
    z_conflits = as.numeric(z_an),
    lag_z = as.numeric(wz_an),
    cluster = case_when(
      z_an > 0 & wz_an > 0 & p_vals < 0.05 ~ "HH",
      z_an < 0 & wz_an < 0 & p_vals < 0.05 ~ "LL",
      z_an > 0 & wz_an < 0 & p_vals < 0.05 ~ "HL",
      z_an < 0 & wz_an > 0 & p_vals < 0.05 ~ "LH",
      TRUE ~ "NS"
    )
  )
}

master_panel <- bind_rows(panel_collecte)

write_csv(master_panel %>% select(iso3, annee, conflits, migrants), "data/processed/maps_panel.csv")
write_csv(master_panel %>% select(iso3, annee, z_conflits, lag_z, cluster), "data/processed/moran_panel.csv")
write_csv(master_panel %>% select(iso3, annee, cluster), "data/processed/lisa_panel.csv")

cat("Fichiers CSV générés pour Sara ✓\n")


