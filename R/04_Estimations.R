# FICHIER 04 — ESTIMATION + TABLEAUX
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

# Tableau T3 — Tests de spécification
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

t3 |>
  gt() |>
  tab_header(title = "T3 — Tests de spécification spatiale") |>
  tab_source_note("*** p<0.01  ** p<0.05  * p<0.10") |>
  fmt_number(columns = c(Statistique, `p-value`), decimals = 4) |>
  gtsave("outputs/tables/T3_tests_specification.html")

t3 |>
  kbl(caption = "T3 — Tests de spécification spatiale", booktabs = TRUE) |>
  kable_styling(latex_options = c("striped", "hold_position")) |>
  save_kable("outputs/tables/T3_tests_specification.tex")

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

# Tableau T4 — Résultats SDM (paramètres bruts)
coefs_dsdm  <- coef(model_dsdm_W1)
se_dsdm     <- sqrt(diag(vcov(model_dsdm_W1)))
z_dsdm      <- coefs_dsdm / se_dsdm
p_dsdm      <- 2 * (1 - pnorm(abs(z_dsdm)))

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
  Variable    = noms_propres[names(coefs_dsdm)],
  Coefficient = round(coefs_dsdm, 4),
  `Std. Error` = round(se_dsdm, 4),
  `z-stat`    = round(z_dsdm, 3),
  `p-value`   = round(p_dsdm, 4),
  check.names = FALSE
) |>
  mutate(` ` = etoiles(`p-value`))

t4 |>
  gt() |>
  tab_header(
    title    = "T4 — Résultats DSDM",
    subtitle = "Variable dépendante : Log conflits pc | W1 contiguïté | Effets fixes individuels"
  ) |>
  tab_source_note("z-statistics (normalité asymptotique ML) | *** p<0.01  ** p<0.05  * p<0.10") |>
  tab_source_note(paste0("N = ", n_distinct(panel_dsdm_W1$iso3), " pays | T = ",
                         n_distinct(panel_dsdm_W1$annee), " années | Obs = ", nrow(panel_dsdm_W1))) |>
  fmt_number(columns = c(Coefficient, `Std. Error`, `z-stat`, `p-value`), decimals = 4) |>
  gtsave("outputs/tables/T4_DSDM_resultats.html")

t4 |>
  kbl(caption = "T4 — Résultats DSDM", booktabs = TRUE) |>
  kable_styling(latex_options = c("striped", "hold_position")) |>
  save_kable("outputs/tables/T4_DSDM_resultats.tex")

cat("T4 exporté\n")

# ============================================================
# ETAPE 6 — EFFETS DIRECTS / INDIRECTS / TOTAUX
# ============================================================
panel_cross_sdm <- panel_sdm_W1 |>
  group_by(iso3) |>
  summarise(across(c(log_conflits, lag_migrants, log_pib, polity2,
                     log_ressources, W_lag_migrants, W_log_pib,
                     W_polity2, W_log_ressources), ~mean(., na.rm = TRUE)),
            .groups = "drop") |>
  arrange(iso3)

sdm_cross <- lagsarlm(
  log_conflits ~ lag_migrants + log_pib + polity2 + log_ressources,
  data = panel_cross_sdm, listw = W1_fin,
  Durbin = ~ lag_migrants + log_pib + polity2 + log_ressources,
  method = "eigen", zero.policy = TRUE
)

set.seed(42)
imp     <- impacts(sdm_cross, listw = W1_fin, R = 1000, zstats = TRUE)
imp_sum <- summary(imp, zstats = TRUE, short = TRUE)
print(imp_sum)

impacts_df <- data.frame(
  variable   = rownames(imp_sum$zmat),
  direct     = imp$res$direct,
  indirect   = imp$res$indirect,
  total      = imp$res$total,
  z_direct   = imp_sum$zmat[, "Direct"],
  z_indirect = imp_sum$zmat[, "Indirect"],
  z_total    = imp_sum$zmat[, "Total"],
  p_direct   = imp_sum$pzmat[, "Direct"],
  p_indirect = imp_sum$pzmat[, "Indirect"],
  p_total    = imp_sum$pzmat[, "Total"]
)
write_csv(impacts_df, "data/processed/impacts_sdm.csv")
saveRDS(sdm_cross, "data/processed/sdm_cross.rds")

# Tableau T5 — Effets directs / indirects / totaux
noms_impacts <- c(
  "lag_migrants dy/dx"   = "Log migrants (t-1)",
  "log_pib dy/dx"        = "Log PIB/hab",
  "polity2 dy/dx"        = "Démocratie (Polity2)",
  "log_ressources dy/dx" = "Log ressources"
)

t5 <- data.frame(
  Variable        = noms_impacts[impacts_df$variable],
  `Effet direct`  = paste0(round(impacts_df$direct, 4),
                           etoiles(impacts_df$p_direct),
                           "\n(", round(impacts_df$z_direct, 3), ")"),
  `Effet indirect` = paste0(round(impacts_df$indirect, 4),
                            etoiles(impacts_df$p_indirect),
                            "\n(", round(impacts_df$z_indirect, 3), ")"),
  `Effet total`   = paste0(round(impacts_df$total, 4),
                           etoiles(impacts_df$p_total),
                           "\n(", round(impacts_df$z_total, 3), ")"),
  check.names = FALSE
)

t5 |>
  gt() |>
  tab_header(
    title    = "T5 — Décomposition des effets directs, indirects et totaux",
    subtitle = "SDM — Coupe transversale moyenne | 1 000 simulations MCMC | z-statistics entre parenthèses"
  ) |>
  tab_source_note("*** p<0.01  ** p<0.05  * p<0.10 | Effet indirect = spillover géographique") |>
  gtsave("outputs/tables/T5_effets_directs_indirects.html")

t5_propre <- data.frame(
  Variable         = noms_impacts[impacts_df$variable],
  `Direct`         = round(impacts_df$direct,   4),
  `z (direct)`     = round(impacts_df$z_direct,  3),
  `p (direct)`     = round(impacts_df$p_direct,  4),
  `Indirect`       = round(impacts_df$indirect,  4),
  `z (indirect)`   = round(impacts_df$z_indirect, 3),
  `p (indirect)`   = round(impacts_df$p_indirect, 4),
  `Total`          = round(impacts_df$total,     4),
  `z (total)`      = round(impacts_df$z_total,   3),
  `p (total)`      = round(impacts_df$p_total,   4),
  check.names = FALSE
)
t5_propre |>
  kbl(caption = "T5 — Effets directs, indirects et totaux", booktabs = TRUE) |>
  kable_styling(latex_options = c("striped", "hold_position")) |>
  save_kable("outputs/tables/T5_effets_directs_indirects.tex")

write_csv(t5_propre, "outputs/tables/T5_effets_directs_indirects.csv")
cat("T5 exporté\n")
cat("fichier 04 terminé\n")