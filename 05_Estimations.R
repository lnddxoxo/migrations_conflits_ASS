# FICHIER 05 — ROBUSTESSE + TABLEAUX
setwd("D:/M1 DS 25 26/SEMESTRE 1/UE CULTURE GENERALE/ECONOMIE/econometrie/Econometric_projet/migrations_conflits_ASS")

library(tidyverse)
library(plm)
library(spdep)
library(splm)
library(sf)
library(Matrix)
library(gt)
library(kableExtra)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

panel_final  <- readRDS("data/processed/panel_final_concordant.rds")
W1_fin       <- readRDS("data/processed/W1_final_concordant.rds")
W2_fin       <- readRDS("data/processed/W2_final_concordant.rds")
W3_fin       <- readRDS("data/processed/W3_final_concordant.rds")
pays_communs <- readRDS("data/processed/pays_communs.rds")

etoiles <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))

creer_wx <- function(panel_df, listw_obj) {
  wx_list <- list()
  for (an in sort(unique(panel_df$annee))) {
    d <- panel_df |> filter(annee == an) |> arrange(iso3)
    wx_list[[as.character(an)]] <- data.frame(
      iso3             = d$iso3,
      annee            = an,
      W_lag_migrants   = lag.listw(listw_obj, d$lag_migrants,   zero.policy = TRUE),
      W_log_pib        = lag.listw(listw_obj, d$log_pib,        zero.policy = TRUE),
      W_polity2        = lag.listw(listw_obj, d$polity2,        zero.policy = TRUE),
      W_log_ressources = lag.listw(listw_obj, d$log_ressources, zero.policy = TRUE)
    )
  }
  panel_df |> left_join(bind_rows(wx_list), by = c("iso3", "annee"))
}

ajouter_lag_Y <- function(df, var_y = "log_conflits") {
  df |>
    arrange(iso3, annee) |>
    group_by(iso3) |>
    mutate(lag_Y = dplyr::lag(.data[[var_y]])) |>
    ungroup() |>
    filter(!is.na(lag_Y))
}

formule_dsdm <- log_conflits ~
  lag_Y +
  lag_migrants + log_pib + polity2 + log_ressources +
  W_lag_migrants + W_log_pib + W_polity2 + W_log_ressources

extraire_tableau <- function(model, label) {
  coefs <- coef(model)
  se    <- sqrt(diag(vcov(model)))
  z     <- coefs / se
  p     <- 2 * (1 - pnorm(abs(z)))
  data.frame(
    Variable    = names(coefs),
    Coef        = round(coefs, 4),
    SE          = round(se, 4),
    z           = round(z, 3),
    p           = round(p, 4),
    sig         = etoiles(p),
    modele      = label,
    check.names = FALSE
  )
}

# ============================================================
# R1 — W2
# ============================================================
panel_dsdm_W2 <- ajouter_lag_Y(creer_wx(panel_final, W2_fin))

model_dsdm_W2 <- spml(
  formule_dsdm, data = panel_dsdm_W2, index = c("iso3", "annee"),
  listw = W2_fin, model = "within", effect = "individual",
  lag = TRUE, spatial.error = "none", method = "eigen"
)
summary(model_dsdm_W2)
saveRDS(model_dsdm_W2, "data/processed/model_dsdm_W2.rds")
res_W2 <- extraire_tableau(model_dsdm_W2, "W2 — Distance inverse")

# ============================================================
# R1 — W3
# ============================================================
panel_dsdm_W3 <- ajouter_lag_Y(creer_wx(panel_final, W3_fin))

model_dsdm_W3 <- spml(
  formule_dsdm, data = panel_dsdm_W3, index = c("iso3", "annee"),
  listw = W3_fin, model = "within", effect = "individual",
  lag = TRUE, spatial.error = "none", method = "eigen"
)
summary(model_dsdm_W3)
saveRDS(model_dsdm_W3, "data/processed/model_dsdm_W3.rds")
res_W3 <- extraire_tableau(model_dsdm_W3, "W3 — k=5 voisins")

# Tableau T6 — Robustesse W alternatives
res_W1 <- extraire_tableau(readRDS("data/processed/model_dsdm_W1.rds"), "W1 — Contiguïté (principal)")

noms_propres <- c(
  "lambda"           = "ρ (spatial lag Y)",
  "lag_Y"            = "τ — Conflits (t-1)",
  "lag_migrants"     = "Log migrants (t-1)",
  "log_pib"          = "Log PIB/hab",
  "polity2"          = "Démocratie",
  "log_ressources"   = "Log ressources",
  "W_lag_migrants"   = "W × Log migrants",
  "W_log_pib"        = "W × Log PIB/hab",
  "W_polity2"        = "W × Démocratie",
  "W_log_ressources" = "W × Log ressources"
)

formater_col <- function(df) {
  df |>
    mutate(
      Variable = noms_propres[Variable],
      val      = paste0(Coef, sig, "\n(", z, ")")
    ) |>
    select(Variable, val, modele)
}

t6 <- bind_rows(
  formater_col(res_W1),
  formater_col(res_W2),
  formater_col(res_W3)
) |>
  pivot_wider(names_from = modele, values_from = val)

t6 |>
  gt() |>
  tab_header(
    title    = "T6 — Robustesse R1 : Matrices W alternatives",
    subtitle = "DSDM | Variable dépendante : Log conflits pc | z-statistics entre parenthèses"
  ) |>
  tab_source_note("*** p<0.01  ** p<0.05  * p<0.10") |>
  gtsave("outputs/tables/T6_robustesse_W.html")

write_csv(t6, "outputs/tables/T6_robustesse_W.csv")
cat("T6 exporté\n")

# ============================================================
# R2 — Y = log_deces
# ============================================================
panel_r2 <- panel_final |> filter(!is.na(log_deces))
panel_dsdm_r2 <- creer_wx(panel_r2, W1_fin) |>
  arrange(iso3, annee) |>
  group_by(iso3) |>
  mutate(lag_Y = dplyr::lag(log_deces)) |>
  ungroup() |>
  filter(!is.na(lag_Y))

model_dsdm_r2 <- spml(
  log_deces ~ lag_Y +
    lag_migrants + log_pib + polity2 + log_ressources +
    W_lag_migrants + W_log_pib + W_polity2 + W_log_ressources,
  data = panel_dsdm_r2, index = c("iso3", "annee"),
  listw = W1_fin, model = "within", effect = "individual",
  lag = TRUE, spatial.error = "none", method = "eigen"
)
summary(model_dsdm_r2)
saveRDS(model_dsdm_r2, "data/processed/model_dsdm_r2.rds")
res_r2 <- extraire_tableau(model_dsdm_r2, "R2 — Log décès")

t7 <- bind_rows(
  formater_col(res_W1 |> mutate(modele = "Modèle principal\n(Log conflits)")),
  formater_col(res_r2)
) |>
  pivot_wider(names_from = modele, values_from = val)

t7 |>
  gt() |>
  tab_header(
    title    = "T7 — Robustesse R2 : Variable dépendante alternative",
    subtitle = "DSDM W1 | Log conflits vs Log décès | z-statistics entre parenthèses"
  ) |>
  tab_source_note("*** p<0.01  ** p<0.05  * p<0.10") |>
  gtsave("outputs/tables/T7_robustesse_Y.html")

write_csv(t7, "outputs/tables/T7_robustesse_Y.csv")
cat("T7 exporté\n")

# ============================================================
# R3 — GMM Blundell-Bond
# ============================================================
p_gmm <- pdata.frame(panel_final |> arrange(iso3, annee), index = c("iso3", "annee"))

model_gmm <- pgmm(
  log_conflits ~
    lag(log_conflits, 1) + lag(lag_migrants, 1) + lag(log_pib, 1) +
    lag(polity2, 1) + lag(log_ressources, 1) |
    lag(log_conflits, 2:5),
  data = p_gmm, effect = "twoways", model = "twosteps",
  collapse = FALSE, transformation = "ld"
)
gmm_sum <- summary(model_gmm, robust = TRUE)
saveRDS(model_gmm, "data/processed/model_gmm.rds")

coefs_gmm <- gmm_sum$coefficients
t8 <- data.frame(
  Variable = c(
    "τ — Conflits (t-1)", "Log migrants (t-1)", "Log PIB/hab",
    "Démocratie", "Log ressources"
  ),
  Coefficient = round(coefs_gmm[, "Estimate"], 4),
  `Std. Error` = round(coefs_gmm[, "Std. Error"], 4),
  `z-stat`     = round(coefs_gmm[, "z-value"], 3),
  `p-value`    = round(coefs_gmm[, "Pr(>|z|)"], 4),
  check.names  = FALSE
) |>
  mutate(` ` = etoiles(`p-value`))

sargan_p <- as.numeric(gmm_sum$sargan[["p-value"]])
ar2_p    <- as.numeric(gmm_sum$m2[["p-value"]])

t8 |>
  gt() |>
  tab_header(
    title    = "T8 — Robustesse R3 : Estimateur GMM Blundell-Bond",
    subtitle = "Système GMM deux étapes | Effets fixes twoways | Erreurs robustes"
  ) |>
  tab_source_note(paste0(
    "Test de Sargan : p = ", round(sargan_p, 3),
    " (H0 : instruments valides) | ",
    "Test AR(2) : p = ", round(ar2_p, 3),
    " (H0 : pas d'autocorrélation ordre 2)"
  )) |>
  tab_source_note("*** p<0.01  ** p<0.05  * p<0.10") |>
  gtsave("outputs/tables/T8_robustesse_GMM.html")

write_csv(t8, "outputs/tables/T8_robustesse_GMM.csv")
cat("T8 exporté\n")

# Export CSV pour Sara
write_csv(panel_final, "data/processed/panel_final_concordant.csv")

W1_mat_export <- listw2mat(W1_fin)
rownames(W1_mat_export) <- pays_communs
colnames(W1_mat_export) <- pays_communs
write.csv(W1_mat_export, "data/processed/W1_matrix.csv")

cat("fichier 05 terminé\n")