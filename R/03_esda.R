setwd("D:/M1 DS 25 26/SEMESTRE 1/UE CULTURE GENERALE/ECONOMIE/econometrie/Econometric_projet/migrations_conflits_ASS")

library(tidyverse)
library(sf)
library(spdep)
library(ggplot2)
library(ggrepel)

panel    <- readRDS("data/processed/panel_clean.rds")
W1_listw <- readRDS("data/processed/W1_listw.rds")
shp_ssa  <- st_read("data/raw/ne_50m_admin_0_countries") |>
  filter(ISO_A3 %in% unique(panel$iso3)) |>
  arrange(ISO_A3)

pays_ordre <- shp_ssa$ISO_A3

panel_moy <- panel |>
  filter(annee >= 1996) |>
  group_by(iso3) |>
  summarise(
    conflits_moy = mean(log_conflits, na.rm = TRUE),
    migrants_moy = mean(log_migrants, na.rm = TRUE)
  )

shp_data <- shp_ssa |>
  left_join(panel_moy, by = c("ISO_A3" = "iso3"))

F1 <- ggplot(shp_data) +
  geom_sf(aes(fill = conflits_moy), color = "white", size = 0.3) +
  scale_fill_gradient(low="#FEF0D9", high="#C1440E",
                      name="Log conflits\n(moy. 1996-2024)", na.value="grey80") +
  labs(title = "F1 — Intensité des conflits armés en ASS") +
  theme_void() +
  theme(plot.title=element_text(size=13, face="bold", hjust=0.5),
        legend.position="right")

ggsave("outputs/figures/F1_conflits.png", F1, width=10, height=7, dpi=300)
cat("F1 sauvegardée ✓\n")

F2 <- ggplot(shp_data) +
  geom_sf(aes(fill = migrants_moy), color = "white", size = 0.3) +
  scale_fill_gradient(low="#EFF3FF", high="#2171B5",
                      name="Log migrants\n(moy. 1996-2024)", na.value="grey80") +
  labs(title = "F2 — Stock de migrants en ASS") +
  theme_void() +
  theme(plot.title=element_text(size=13, face="bold", hjust=0.5),
        legend.position="right")

ggsave("outputs/figures/F2_migrants.png", F2, width=10, height=7, dpi=300)
cat("F2 sauvegardée ✓\n")

panel_pays <- panel |>
  filter(annee >= 1996) |>
  group_by(iso3) |>
  summarise(conflits_moy = mean(log_conflits, na.rm = TRUE))

Q1      <- quantile(panel_pays$conflits_moy, 0.25, na.rm=TRUE)
Q3      <- quantile(panel_pays$conflits_moy, 0.75, na.rm=TRUE)
IQR_val <- Q3 - Q1
outliers <- panel_pays |> filter(conflits_moy > Q3 + 1.5 * IQR_val)

F3 <- ggplot(panel_pays, aes(x="", y=conflits_moy)) +
  geom_boxplot(fill="#FEE2E2", color="#C1440E", width=0.4) +
  geom_text(data=outliers, aes(x="", y=conflits_moy, label=iso3),
            hjust=-0.3, size=3.5, color="#C1440E") +
  labs(title="F3 — Distribution des conflits par pays",
       subtitle="Outliers identifiés par règle de Tukey (Q3 + 1.5×IQR)",
       x="", y="Log conflits pc (moyenne 1996-2024)") +
  theme_minimal() +
  theme(plot.title=element_text(size=13, face="bold"),
        plot.subtitle=element_text(size=10, color="grey50"))

ggsave("outputs/figures/F3_boxplot.png", F3, width=8, height=6, dpi=300)
cat("F3 sauvegardée ✓\n")
cat("Outliers :", paste(outliers$iso3, collapse=", "), "\n")

panel_cross <- panel |>
  filter(annee >= 1996) |>
  group_by(iso3) |>
  summarise(
    log_conflits   = mean(log_conflits,   na.rm=TRUE),
    log_migrants   = mean(log_migrants,   na.rm=TRUE),
    log_pib        = mean(log_pib,        na.rm=TRUE),
    polity2        = mean(polity2,         na.rm=TRUE),
    log_ressources = mean(log_ressources, na.rm=TRUE),
    .groups = "drop"
  ) |>
  arrange(match(iso3, pays_ordre))

pays_complets <- panel_cross |>
  filter(!is.na(log_conflits) & !is.na(log_migrants) &
           !is.na(log_pib) & !is.na(polity2) & !is.na(log_ressources)) |>
  pull(iso3)

panel_cross_clean <- panel_cross |>
  filter(iso3 %in% pays_complets) |>
  arrange(match(iso3, pays_ordre))

shp_clean <- shp_ssa |>
  filter(ISO_A3 %in% pays_complets) |>
  arrange(ISO_A3)

nb_clean <- poly2nb(shp_clean, queen=TRUE)
W1_clean <- nb2listw(nb_clean, style="W", zero.policy=TRUE)

ols_base  <- lm(log_conflits ~ log_migrants + log_pib + polity2 + log_ressources,
                data=panel_cross_clean)
resid_ols <- residuals(ols_base)

moran_test <- moran.test(resid_ols, W1_clean, randomisation=TRUE, zero.policy=TRUE)
print(moran_test)

I_val <- round(moran_test$estimate["Moran I statistic"], 3)
p_val <- round(moran_test$p.value, 4)
cat("\n✓ I de Moran =", I_val, "| p-value =", p_val, "\n")

lag_resid <- lag.listw(W1_clean, resid_ols, zero.policy=TRUE)

moran_df <- data.frame(
  iso3=panel_cross_clean$iso3,
  resid=as.numeric(resid_ols),
  lag_resid=as.numeric(lag_resid)
) |>
  mutate(
    quadrant = case_when(
      resid>=0 & lag_resid>=0 ~ "HH",
      resid<0  & lag_resid<0  ~ "LL",
      resid>=0 & lag_resid<0  ~ "HL",
      resid<0  & lag_resid>=0 ~ "LH"
    ),
    label = ifelse(iso3 %in% c("SOM","CAF","BDI","SSD","COD",
                               "NGA","SDN","MLI","BFA"), iso3, "")
  )

couleurs <- c("HH"="#DC2626","LL"="#2563EB","HL"="#F97316","LH"="#8B5CF6")

F4 <- ggplot(moran_df, aes(x=resid, y=lag_resid)) +
  geom_hline(yintercept=mean(lag_resid, na.rm=TRUE),
             linetype="dashed", color="grey60", linewidth=0.5) +
  geom_vline(xintercept=mean(resid, na.rm=TRUE),
             linetype="dashed", color="grey60", linewidth=0.5) +
  geom_smooth(method="lm", se=TRUE, color="#1E3A5F",
              fill="#BFDBFE", linewidth=1, alpha=0.25) +
  geom_point(aes(color=quadrant), size=3, alpha=0.85) +
  geom_text_repel(aes(label=label), size=3, fontface="bold",
                  color="#1E3A5F", max.overlaps=20, box.padding=0.4) +
  annotate("text", x=max(moran_df$resid)*0.75,
           y=max(moran_df$lag_resid)*0.75, label="HH",
           size=5, color="#DC2626", fontface="bold") +
  annotate("text", x=min(moran_df$resid)*0.75,
           y=min(moran_df$lag_resid)*0.75, label="LL",
           size=5, color="#2563EB", fontface="bold") +
  annotate("text", x=max(moran_df$resid)*0.75,
           y=min(moran_df$lag_resid)*0.75, label="HL",
           size=5, color="#F97316", fontface="bold") +
  annotate("text", x=min(moran_df$resid)*0.75,
           y=max(moran_df$lag_resid)*0.75, label="LH",
           size=5, color="#8B5CF6", fontface="bold") +
  scale_color_manual(values=couleurs, name="Quadrant") +
  labs(title="F4 — Diagramme de Moran des résidus MCO",
       subtitle=paste0("I de Moran = ", I_val, " | p-value = ", p_val,
                       " | Matrice W₁ contiguïté (reine)"),
       x="Résidus MCO — log(conflits pc)",
       y="Décalage spatial W·Résidus",
       caption="Source : UCDP + WDI | M1 Économétrie Spatiale 2025") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(face="bold", size=14),
        plot.subtitle=element_text(color="grey40", size=10),
        legend.position="bottom",
        panel.grid.minor=element_blank())

ggsave("outputs/figures/F4_moran.png", F4, width=8, height=6, dpi=300, bg="white")
cat("F4 sauvegardée ✓\n")

panel_cross_lisa <- panel |>
  filter(annee >= 1996) |>
  group_by(iso3) |>
  summarise(log_conflits = mean(log_conflits, na.rm=TRUE), .groups="drop") |>
  arrange(match(iso3, pays_ordre)) |>
  mutate(log_conflits = ifelse(is.na(log_conflits), 0, log_conflits))

set.seed(42)
lisa <- localmoran_perm(
  x           = panel_cross_lisa$log_conflits,
  listw       = W1_listw,
  nsim        = 999,
  zero.policy = TRUE
)

nb_voisins <- card(W1_listw$neighbours)
alpha_bonf <- 0.05 / nb_voisins
p_values   <- lisa[, "Pr(z != E(Ii))"]
sig_5pct   <- p_values < 0.05
sig_bonf   <- p_values < alpha_bonf

z  <- scale(panel_cross_lisa$log_conflits)[, 1]
wz <- lag.listw(W1_listw, z, zero.policy=TRUE)

quadrant_5pct <- case_when(
  z>0 & wz>0 & sig_5pct ~ "HH",
  z<0 & wz<0 & sig_5pct ~ "LL",
  z>0 & wz<0 & sig_5pct ~ "HL",
  z<0 & wz>0 & sig_5pct ~ "LH",
  TRUE ~ "NS"
)

quadrant_bonf <- case_when(
  z>0 & wz>0 & sig_bonf ~ "HH",
  z<0 & wz<0 & sig_bonf ~ "LL",
  z>0 & wz<0 & sig_bonf ~ "HL",
  z<0 & wz>0 & sig_bonf ~ "LH",
  TRUE ~ "NS"
)

cat("Distribution quadrants (5%) :\n")
print(table(quadrant_5pct))
cat("\nDistribution quadrants (Bonferroni) :\n")
print(table(quadrant_bonf))

shp_lisa <- shp_ssa |>
  arrange(ISO_A3) |>
  mutate(quadrant_5pct=quadrant_5pct, quadrant_bonf=quadrant_bonf)

palette_lisa <- c("HH"="#C1440E","LL"="#2D5016","HL"="#F97316",
                  "LH"="#8B5CF6","NS"="#F5ECD7")

F5a <- ggplot(shp_lisa) +
  geom_sf(aes(fill=quadrant_5pct), color="white", linewidth=0.3) +
  scale_fill_manual(values=palette_lisa, name="Cluster LISA") +
  labs(title="F5 — Carte LISA — Clusters spatiaux de conflits",
       subtitle="Significatif à 5% | 999 permutations | Matrice W₁ contiguïté") +
  theme_void() +
  theme(plot.title=element_text(size=13, face="bold", hjust=0.5),
        plot.subtitle=element_text(size=10, color="grey50", hjust=0.5),
        plot.background=element_rect(fill="white", color=NA),
        legend.position="bottom")

ggsave("outputs/figures/F5_lisa_5pct.png", F5a, width=10, height=7, dpi=300, bg="white")
cat("F5a (5%) sauvegardée ✓\n")

F5b <- ggplot(shp_lisa) +
  geom_sf(aes(fill=quadrant_bonf), color="white", linewidth=0.3) +
  scale_fill_manual(values=palette_lisa, name="Cluster LISA") +
  labs(title="F5 — Carte LISA — Clusters spatiaux de conflits",
       subtitle="Correction Bonferroni (α/m) | 999 permutations | Matrice W₁ contiguïté") +
  theme_void() +
  theme(plot.title=element_text(size=13, face="bold", hjust=0.5),
        plot.subtitle=element_text(size=10, color="grey50", hjust=0.5),
        plot.background=element_rect(fill="white", color=NA),
        legend.position="bottom")

ggsave("outputs/figures/F5_lisa_bonferroni.png", F5b, width=10, height=7, dpi=300, bg="white")
cat("F5b (Bonferroni) sauvegardée ✓\n")

lisa_export <- data.frame(
  iso3          = panel_cross_lisa$iso3,
  Ii            = lisa[, "Ii"],
  p_value       = p_values,
  p_bonferroni  = alpha_bonf,
  quadrant_5pct = quadrant_5pct,
  quadrant_bonf = quadrant_bonf
)

write_csv(lisa_export, "data/processed/lisa_results.csv")
cat("lisa_results.csv exporté ✓\n")
cat("\n=== ESDA COMPLÈTE ✓ ===\n")