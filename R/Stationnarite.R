setwd("D:/M1 DS 25 26/SEMESTRE 1/UE CULTURE GENERALE/ECONOMIE/econometrie/Econometric_projet/migrations_conflits_ASS")

library(tidyverse)
library(plm)
library(spdep)
library(ggplot2)
library(ggrepel)

panel_final  <- readRDS("data/processed/panel_final_concordant.rds")
W1_fin       <- readRDS("data/processed/W1_final_concordant.rds")
pays_communs <- readRDS("data/processed/pays_communs.rds")

cat("chargement ok\n")

# ============================================================
# TEST IPS — STATIONNARITE
# ============================================================
p_panel <- pdata.frame(panel_final, index = c("iso3", "annee"))

cat("\n--- Test IPS : log_conflits ---\n")
ips_conflits <- purtest(log_conflits ~ 1,
                        data   = p_panel,
                        test   = "ips",
                        lags   = "AIC",
                        pmax   = 4)
print(ips_conflits)

cat("\n--- Test IPS : lag_migrants ---\n")
ips_migrants <- purtest(lag_migrants ~ 1,
                        data   = p_panel,
                        test   = "ips",
                        lags   = "AIC",
                        pmax   = 4)
print(ips_migrants)

cat("\n--- Test IPS : log_pib ---\n")
ips_pib <- purtest(log_pib ~ 1,
                   data   = p_panel,
                   test   = "ips",
                   lags   = "AIC",
                   pmax   = 4)
print(ips_pib)

cat("\n--- Test IPS : polity2 ---\n")
ips_polity <- purtest(polity2 ~ 1,
                      data   = p_panel,
                      test   = "ips",
                      lags   = "AIC",
                      pmax   = 4)
print(ips_polity)

cat("\n--- Test IPS : log_ressources ---\n")
ips_ressources <- purtest(log_ressources ~ 1,
                          data   = p_panel,
                          test   = "ips",
                          lags   = "AIC",
                          pmax   = 4)
print(ips_ressources)

# Tableau récapitulatif IPS
extraire_ips <- function(test, nom) {
  stat <- test$statistic$statistic
  p    <- test$statistic$p.value
  data.frame(
    Variable   = nom,
    Statistique = round(stat, 3),
    `p-value`  = round(p, 4),
    Conclusion = ifelse(p < 0.05, "Stationnaire I(0) ✓", "Non stationnaire I(1)"),
    check.names = FALSE
  )
}

t_ips <- bind_rows(
  extraire_ips(ips_conflits,   "Log conflits pc"),
  extraire_ips(ips_migrants,   "Log migrants (t-1)"),
  extraire_ips(ips_pib,        "Log PIB/hab"),
  extraire_ips(ips_polity,     "Démocratie (Polity2)"),
  extraire_ips(ips_ressources, "Log ressources")
)

print(t_ips)
write_csv(t_ips, "outputs/tables/T_IPS_stationnarite.csv")
cat("Tableau IPS exporté ✓\n")

# ============================================================
# F4 — DIAGRAMME DE MORAN PROPRE AVEC ISO3
# ============================================================
panel_cross <- panel_final |>
  filter(annee >= 1996) |>
  group_by(iso3) |>
  summarise(
    log_conflits   = mean(log_conflits,   na.rm = TRUE),
    lag_migrants   = mean(lag_migrants,   na.rm = TRUE),
    log_pib        = mean(log_pib,        na.rm = TRUE),
    polity2        = mean(polity2,        na.rm = TRUE),
    log_ressources = mean(log_ressources, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(iso3)

ols_cross <- lm(
  log_conflits ~ lag_migrants + log_pib + polity2 + log_ressources,
  data = panel_cross
)

resid_ols <- residuals(ols_cross)
lag_resid  <- lag.listw(W1_fin, resid_ols, zero.policy = TRUE)

moran_df <- data.frame(
  iso3      = panel_cross$iso3,
  resid     = as.numeric(resid_ols),
  lag_resid = as.numeric(lag_resid)
) |>
  mutate(
    quadrant = case_when(
      resid >= 0 & lag_resid >= 0 ~ "HH",
      resid <  0 & lag_resid <  0 ~ "LL",
      resid >= 0 & lag_resid <  0 ~ "HL",
      resid <  0 & lag_resid >= 0 ~ "LH"
    ),
    outlier = iso3 %in% c("SOM","CAF","BDI","SSD","COD",
                          "NGA","SDN","MLI","BFA","ETH")
  )

moran_test <- moran.test(resid_ols, W1_fin, zero.policy = TRUE)
I_val <- round(moran_test$estimate["Moran I statistic"], 3)
p_val <- round(moran_test$p.value, 4)

couleurs <- c(
  "HH" = "#C1440E",
  "LL" = "#2D5016",
  "HL" = "#F97316",
  "LH" = "#8B5CF6"
)

F4 <- ggplot(moran_df, aes(x = resid, y = lag_resid)) +
  
  geom_hline(yintercept = mean(moran_df$lag_resid, na.rm = TRUE),
             linetype = "dashed", color = "grey60", linewidth = 0.5) +
  geom_vline(xintercept = mean(moran_df$resid, na.rm = TRUE),
             linetype = "dashed", color = "grey60", linewidth = 0.5) +
  
  geom_smooth(method = "lm", se = TRUE,
              color = "#0D2137", fill = "#BFDBFE",
              linewidth = 1, alpha = 0.2) +
  
  geom_point(aes(color = quadrant), size = 3, alpha = 0.85) +
  
  geom_text_repel(
    data = filter(moran_df, outlier == TRUE),
    aes(label = iso3),
    size = 3.2, fontface = "bold",
    color = "#0D2137",
    box.padding  = 0.4,
    max.overlaps = 20
  ) +
  
  annotate("text",
           x =  max(moran_df$resid, na.rm = TRUE) * 0.7,
           y =  max(moran_df$lag_resid, na.rm = TRUE) * 0.8,
           label = "HH", size = 5,
           color = "#C1440E", fontface = "bold") +
  annotate("text",
           x = min(moran_df$resid, na.rm = TRUE) * 0.7,
           y = min(moran_df$lag_resid, na.rm = TRUE) * 0.8,
           label = "LL", size = 5,
           color = "#2D5016", fontface = "bold") +
  annotate("text",
           x =  max(moran_df$resid, na.rm = TRUE) * 0.7,
           y = min(moran_df$lag_resid, na.rm = TRUE) * 0.8,
           label = "HL", size = 5,
           color = "#F97316", fontface = "bold") +
  annotate("text",
           x = min(moran_df$resid, na.rm = TRUE) * 0.7,
           y =  max(moran_df$lag_resid, na.rm = TRUE) * 0.8,
           label = "LH", size = 5,
           color = "#8B5CF6", fontface = "bold") +
  
  scale_color_manual(values = couleurs, name = "Quadrant") +
  
  labs(
    title    = "F4 — Diagramme de Moran des résidus MCO",
    subtitle = paste0("I de Moran = ", I_val,
                      " | p-value = ", p_val,
                      " | Matrice W₁ contiguïté"),
    x        = "Résidus MCO — log(conflits pc)",
    y        = "Décalage spatial W·Résidus",
    caption  = "Source : UCDP + WDI | M1 Économétrie Spatiale 2025"
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13,
                                   color = "#0D2137"),
    plot.subtitle   = element_text(color = "grey40", size = 10),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA)
  )

print(F4)

ggsave("outputs/figures/F4_moran_propre.png",
       F4, width = 8, height = 6, dpi = 300, bg = "white")
cat("F4 sauvegardée ✓\n")

cat("\nIPS + F4 terminés ✓\n")


fichiers <- c(
  "data/processed/panel_clean.csv",
  "data/processed/W1_matrix.csv",
  "data/processed/sdm_results.csv",
  "data/processed/impacts_matrix.csv",
  "data/processed/lisa_panel.csv"
)

for (f in fichiers) {
  cat(f, "—", ifelse(file.exists(f), "✓ existe", "✗ MANQUANT"), "\n")
}