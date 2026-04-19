# FICHIER 04 — ESTIMATION + TABLEAUX (CORRIGÉ)
setwd("D:/M1 DS 25 26/SEMESTRE 1/UE CULTURE GENERALE/ECONOMIE/econometrie/Econometric_projet/migrations_conflits_ASS")

library(tidyverse)
library(plm)
library(spdep)
library(splm)
library(spatialreg)
library(sf)
library(Matrix)
library(car)
library(geosphere)
library(modelsummary)
library(gt)
library(kableExtra)

dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)

panel <- readRDS("data/processed/panel_clean.rds")

shp_ssa <- st_read("data/raw/ne_50m_admin_0_countries") |>
  filter(ISO_A3 %in% unique(panel$iso3)) |>
  arrange(ISO_A3)

vars_modele <- c("lag_migrants", "log_conflits", "log_pib", "polity2", "log_ressources")

pays_solides <- panel |>
  filter(!is.na(lag_migrants), !is.na(log_conflits), !is.na(log_pib)) |>
  group_by(iso3) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n >= 15) |>
  pull(iso3)

pays_communs <- intersect(sort(pays_solides), sort(shp_ssa$ISO_A3))

panel_final <- panel |>
  filter(iso3 %in% pays_communs, annee >= 1996) |>
  arrange(iso3, annee) |>
  group_by(iso3) |>
  mutate(across(all_of(vars_modele), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))) |>
  ungroup()

pays_complets <- panel_final |>
  filter(complete.cases(pick(all_of(vars_modele)))) |>
  pull(iso3) |> unique()

panel_final  <- panel_final |> filter(iso3 %in% pays_complets)
pays_communs <- sort(unique(panel_final$iso3))
shp_final    <- shp_ssa |> filter(ISO_A3 %in% pays_communs) |> arrange(ISO_A3)

patch_listw <- function(mat, pays_tri) {
  rownames(mat) <- pays_tri; colnames(mat) <- pays_tri
  lw <- mat2listw(mat, style = "W", zero.policy = TRUE)
  attr(lw$neighbours, "region.id") <- pays_tri
  lw
}

nb_W1  <- poly2nb(shp_final, queen = TRUE)
W1_fin <- patch_listw(listw2mat(nb2listw(nb_W1, style = "W", zero.policy = TRUE)), pays_communs)

coords_w2 <- st_centroid(shp_final) |> st_coordinates()
n <- length(pays_communs)
dist_mat <- matrix(0, n, n)
for (i in 1:n) for (j in 1:n) if (i != j) dist_mat[i,j] <- distHaversine(coords_w2[i,], coords_w2[j,]) / 1000
min_d  <- min(dist_mat[dist_mat > 0])
W2_raw <- ifelse(dist_mat > 0, min_d / dist_mat, 0)
rs     <- rowSums(W2_raw); rs[rs == 0] <- 1
W2_fin <- patch_listw(sweep(W2_raw, 1, rs, "/"), pays_communs)

coords_fin <- st_centroid(shp_final) |> st_coordinates()
W3_fin <- patch_listw(listw2mat(nb2listw(knn2nb(knearneigh(coords_fin, k = 5)), style = "W", zero.policy = TRUE)), pays_communs)

pays_isoles <- pays_communs[card(W1_fin$neighbours) == 0]
if (length(pays_isoles) > 0) {
  pays_communs <- setdiff(pays_communs, pays_isoles)
  panel_final  <- panel_final |> filter(iso3 %in% pays_communs)
  shp_final    <- shp_final |> filter(ISO_A3 %in% pays_communs) |> arrange(ISO_A3)
  nb_W1        <- poly2nb(shp_final, queen = TRUE)
  W1_fin       <- patch_listw(listw2mat(nb2listw(nb_W1, style = "W", zero.policy = TRUE)), pays_communs)
  coords_w2    <- st_centroid(shp_final) |> st_coordinates()
  n            <- length(pays_communs)
  dist_mat     <- matrix(0, n, n)
  for (i in 1:n) for (j in 1:n) if (i != j) dist_mat[i,j] <- distHaversine(coords_w2[i,], coords_w2[j,]) / 1000
  min_d  <- min(dist_mat[dist_mat > 0])
  W2_raw <- ifelse(dist_mat > 0, min_d / dist_mat, 0)
  rs     <- rowSums(W2_raw); rs[rs == 0] <- 1
  W2_fin <- patch_listw(sweep(W2_raw, 1, rs, "/"), pays_communs)
  coords_fin <- st_centroid(shp_final) |> st_coordinates()
  W3_fin     <- patch_listw(listw2mat(nb2listw(knn2nb(knearneigh(coords_fin, k = 5)), style = "W", zero.policy = TRUE)), pays_communs)
}

saveRDS(panel_final,  "data/processed/panel_final_concordant.rds")
saveRDS(W1_fin,       "data/processed/W1_final_concordant.rds")
saveRDS(W2_fin,       "data/processed/W2_final_concordant.rds")
saveRDS(W3_fin,       "data/processed/W3_final_concordant.rds")
saveRDS(pays_communs, "data/processed/pays_communs.rds")

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

panel_sdm_W1  <- creer_wx(panel_final, W1_fin)
panel_dsdm_W1 <- panel_sdm_W1 |>
  arrange(iso3, annee) |>
  group_by(iso3) |>
  mutate(lag_log_conflits = dplyr::lag(log_conflits)) |>
  ungroup() |>
  filter(!is.na(lag_log_conflits))

# ============================================================
# ETAPE 1 — MCO + MORAN
# ============================================================
p_panel <- pdata.frame(panel_final, index = c("iso3", "annee"))

ols_fe <- plm(
  log_conflits ~ lag_migrants + log_pib + polity2 + log_ressources,
  data = p_panel, model = "within", effect = "twoways"
)
summary(ols_fe)

panel_cross_test <- panel_final |> filter(annee == max(annee)) |> arrange(iso3)
ols_cross <- lm(log_conflits ~ lag_migrants + log_pib + polity2 + log_ressources,
                data = panel_cross_test)
moran_res <- moran.test(residuals(ols_cross), W1_fin, zero.policy = TRUE)
print(moran_res)

# ============================================================
# ETAPE 2 — TESTS LM
# ============================================================
lm_w1_lag <- slmtest(ols_fe, listw = W1_fin, test = "lml")
lm_w1_err <- slmtest(ols_fe, listw = W1_fin, test = "lme")
lm_w2_lag <- slmtest(ols_fe, listw = W2_fin, test = "lml")
lm_w2_err <- slmtest(ols_fe, listw = W2_fin, test = "lme")
lm_w3_lag <- slmtest(ols_fe, listw = W3_fin, test = "lml")
lm_w3_err <- slmtest(ols_fe, listw = W3_fin, test = "lme")

etoiles <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))

t3 <- data.frame(
  Test = c(
    "I de Moran (résidus MCO)",
    "LM-lag (W1 contiguïté)", "LM-err (W1 contiguïté)",
    "LM-lag (W2 distance)",   "LM-err (W2 distance)",
    "LM-lag (W3 k=5)",        "LM-err (W3 k=5)"
  ),
  Statistique = round(c(
    moran_res$statistic,
    lm_w1_lag$statistic, lm_w1_err$statistic,
    lm_w2_lag$statistic, lm_w2_err$statistic,
    lm_w3_lag$statistic, lm_w3_err$statistic
  ), 4),
  `p-value` = round(c(
    moran_res$p.value,
    lm_w1_lag$p.value, lm_w1_err$p.value,
    lm_w2_lag$p.value, lm_w2_err$p.value,
    lm_w3_lag$p.value, lm_w3_err$p.value
  ), 4),
  check.names = FALSE
) |>
  mutate(` ` = etoiles(`p-value`))

write_csv(t3, "outputs/tables/T3_tests_specification.csv")
cat("T3 exporté\n")

# ============================================================
# ETAPE 3 — SDM
# ============================================================
formule_sdm <- log_conflits ~
  lag_migrants + log_pib + polity2 + log_ressources +
  W_lag_migrants + W_log_pib + W_polity2 + W_log_ressources

model_sdm_W1 <- spml(
  formule_sdm, data = panel_sdm_W1, index = c("iso3", "annee"),
  listw = W1_fin, model = "within", effect = "twoways",
  lag = TRUE, spatial.error = "none", method = "eigen"
)
summary(model_sdm_W1)
saveRDS(model_sdm_W1, "data/processed/model_sdm_W1.rds")

# ============================================================
# ETAPE 4 — TESTS WALD
# ============================================================
theta_names <- c("W_lag_migrants", "W_log_pib", "W_polity2", "W_log_ressources")
coefs_sdm   <- coef(model_sdm_W1)
vcov_sdm    <- vcov(model_sdm_W1)

wald_sar <- linearHypothesis(model_sdm_W1, paste0(theta_names, " = 0"), vcov. = vcov_sdm)
rho      <- coefs_sdm["lambda"]
hyp_sem  <- paste0(theta_names, " + ", round(rho, 6), " * ",
                   c("lag_migrants", "log_pib", "polity2", "log_ressources"), " = 0")
wald_sem <- linearHypothesis(model_sdm_W1, hyp_sem, vcov. = vcov_sdm)

print(wald_sar)
print(wald_sem)

# ============================================================
# ETAPE 5 — DSDM
# ============================================================
formule_dsdm <- log_conflits ~
  lag_log_conflits +
  lag_migrants + log_pib + polity2 + log_ressources +
  W_lag_migrants + W_log_pib + W_polity2 + W_log_ressources

model_dsdm_W1 <- spml(
  formule_dsdm, data = panel_dsdm_W1, index = c("iso3", "annee"),
  listw = W1_fin, model = "within", effect = "individual",
  lag = TRUE, spatial.error = "none", method = "eigen"
)
summary(model_dsdm_W1)
saveRDS(model_dsdm_W1, "data/processed/model_dsdm_W1.rds")
saveRDS(panel_dsdm_W1, "data/processed/panel_dsdm_W1.rds")

coefs_dsdm <- coef(model_dsdm_W1)
se_dsdm    <- sqrt(diag(vcov(model_dsdm_W1)))
z_dsdm     <- coefs_dsdm / se_dsdm
p_dsdm     <- 2 * (1 - pnorm(abs(z_dsdm)))

noms_propres <- c(
  "lambda"           = "ρ (spatial lag Y)",
  "lag_log_conflits" = "τ — Conflits (t-1)",
  "lag_migrants"     = "Log migrants (t-1)",
  "log_pib"          = "Log PIB/hab",
  "polity2"          = "Démocratie (Polity2)",
  "log_ressources"   = "Log ressources",
  "W_lag_migrants"   = "W × Log migrants",
  "W_log_pib"        = "W × Log PIB/hab",
  "W_polity2"        = "W × Démocratie",
  "W_log_ressources" = "W × Log ressources"
)

t4 <- data.frame(
  Variable     = noms_propres[names(coefs_dsdm)],
  Coefficient  = round(coefs_dsdm, 4),
  `Std. Error` = round(se_dsdm, 4),
  `z-stat`     = round(z_dsdm, 3),
  `p-value`    = round(p_dsdm, 4),
  check.names  = FALSE
) |>
  mutate(` ` = etoiles(`p-value`))

write_csv(t4, "outputs/tables/T4_DSDM_resultats.csv")
cat("T4 exporté\n")

# ============================================================
# ETAPE 6 — EFFETS DIRECTS / INDIRECTS / TOTAUX
# ============================================================
# CORRECTION : on reconstruit un SDM lagsarlm sur le panel COMPLET
# (pas sur la coupe transversale de 40 obs qui donnait des NS)
# On utilise une année par pays (dernière année disponible)
# pour avoir la structure cross-sectionnelle correcte avec N=40

# Approche correcte : utiliser les coefficients du DSDM panel
# et calculer les impacts via la matrice d'impact théorique
# S_k(W) = (I - rho*W)^{-1} * (I*beta_k + W*theta_k)

cat("\n=== CALCUL DES EFFETS DIRECTS/INDIRECTS/TOTAUX ===\n")

rho_hat   <- coef(model_dsdm_W1)["lambda"]
W_mat     <- listw2mat(W1_fin)
n_pays    <- nrow(W_mat)
I_mat     <- diag(n_pays)

# Inverser (I - rho*W)
IrW_inv   <- solve(I_mat - rho_hat * W_mat)

# Variables pour lesquelles calculer les impacts
vars_impact <- c("lag_migrants", "log_pib", "polity2", "log_ressources")
vars_wx     <- c("W_lag_migrants", "W_log_pib", "W_polity2", "W_log_ressources")

impacts_list <- list()

for (k in seq_along(vars_impact)) {
  
  beta_k  <- coef(model_dsdm_W1)[vars_impact[k]]
  theta_k <- coef(model_dsdm_W1)[vars_wx[k]]
  
  # Matrice d'impact S_k(W)
  S_k <- IrW_inv %*% (I_mat * beta_k + W_mat * theta_k)
  
  # Scalaires LeSage & Pace (2009)
  direct_k   <- mean(diag(S_k))
  total_k    <- mean(rowSums(S_k))
  indirect_k <- total_k - direct_k
  
  impacts_list[[k]] <- data.frame(
    variable = paste0(vars_impact[k], " dy/dx"),
    direct   = direct_k,
    indirect = indirect_k,
    total    = total_k
  )
}

impacts_df_mat <- bind_rows(impacts_list)

# ---- Inférence par simulation Monte Carlo ----
# On simule les coefficients depuis leur distribution asymptotique
# pour obtenir des erreurs-types sur les effets directs/indirects

set.seed(42)
n_sim     <- 1000
coefs_hat <- coef(model_dsdm_W1)
vcov_hat  <- vcov(model_dsdm_W1)

# Simulation multivariée normale
sim_coefs <- MASS::mvrnorm(n_sim, mu = coefs_hat, Sigma = vcov_hat)

sim_direct   <- matrix(NA, n_sim, length(vars_impact))
sim_indirect <- matrix(NA, n_sim, length(vars_impact))
sim_total    <- matrix(NA, n_sim, length(vars_impact))

for (s in 1:n_sim) {
  rho_s   <- sim_coefs[s, "lambda"]
  IrW_s   <- tryCatch(solve(I_mat - rho_s * W_mat), error = function(e) IrW_inv)
  
  for (k in seq_along(vars_impact)) {
    beta_s  <- sim_coefs[s, vars_impact[k]]
    theta_s <- sim_coefs[s, vars_wx[k]]
    S_s     <- IrW_s %*% (I_mat * beta_s + W_mat * theta_s)
    
    sim_direct[s, k]   <- mean(diag(S_s))
    sim_total[s, k]    <- mean(rowSums(S_s))
    sim_indirect[s, k] <- sim_total[s, k] - sim_direct[s, k]
  }
}

# Calcul z-statistics et p-values
impacts_df <- impacts_df_mat |>
  mutate(
    se_direct   = apply(sim_direct,   2, sd),
    se_indirect = apply(sim_indirect, 2, sd),
    se_total    = apply(sim_total,    2, sd),
    z_direct    = direct   / se_direct,
    z_indirect  = indirect / se_indirect,
    z_total     = total    / se_total,
    p_direct    = 2 * (1 - pnorm(abs(z_direct))),
    p_indirect  = 2 * (1 - pnorm(abs(z_indirect))),
    p_total     = 2 * (1 - pnorm(abs(z_total)))
  )

cat("\n=== EFFETS DIRECTS / INDIRECTS / TOTAUX ===\n")
print(impacts_df[, c("variable","direct","z_direct","p_direct",
                     "indirect","z_indirect","p_indirect",
                     "total","z_total","p_total")])

write_csv(impacts_df, "data/processed/impacts_sdm.csv")

noms_impacts <- c(
  "lag_migrants dy/dx"   = "Log migrants (t-1)",
  "log_pib dy/dx"        = "Log PIB/hab",
  "polity2 dy/dx"        = "Démocratie (Polity2)",
  "log_ressources dy/dx" = "Log ressources"
)

t5_propre <- data.frame(
  Variable        = noms_impacts[impacts_df$variable],
  `Direct`        = round(impacts_df$direct,    4),
  `z (direct)`    = round(impacts_df$z_direct,   3),
  `p (direct)`    = round(impacts_df$p_direct,   4),
  `Indirect`      = round(impacts_df$indirect,   4),
  `z (indirect)`  = round(impacts_df$z_indirect, 3),
  `p (indirect)`  = round(impacts_df$p_indirect, 4),
  `Total`         = round(impacts_df$total,      4),
  `z (total)`     = round(impacts_df$z_total,    3),
  `p (total)`     = round(impacts_df$p_total,    4),
  check.names     = FALSE
)

write_csv(t5_propre, "outputs/tables/T5_effets_directs_indirects.csv")
cat("T5 exporté ✓\n")
cat("fichier 04 terminé ✓\n")