setwd("D:/M1 DS 25 26/SEMESTRE 1/UE CULTURE GENERALE/ECONOMIE/econometrie/Econometric_projet/migrations_conflits_ASS")

library(tidyverse)
library(spatialreg)
library(spdep)

model_sdm_W1  <- readRDS("data/processed/model_sdm_W1.rds")
model_dsdm_W1 <- readRDS("data/processed/model_dsdm_W1.rds")
sdm_cross     <- readRDS("data/processed/sdm_cross.rds")
W1_fin        <- readRDS("data/processed/W1_final_concordant.rds")
pays_communs  <- readRDS("data/processed/pays_communs.rds")
impacts_df    <- read_csv("data/processed/impacts_sdm.csv", show_col_types = FALSE)

cat("chargement ok\n")

vcov_mat  <- model_sdm_W1$vcov
se_sdm    <- sqrt(diag(vcov_mat))
names(se_sdm) <- names(model_sdm_W1$coefficients)
coefs_sdm <- model_sdm_W1$coefficients
z_sdm     <- coefs_sdm / se_sdm
p_sdm     <- 2 * (1 - pnorm(abs(z_sdm)))

vars_main <- c("lag_migrants", "log_pib", "polity2", "log_ressources")
vars_wx   <- c("W_lag_migrants", "W_log_pib", "W_polity2", "W_log_ressources")

sdm_results <- data.frame(
  variable         = vars_main,
  main             = round(coefs_sdm[vars_main], 4),
  main_se          = round(se_sdm[vars_main], 4),
  main_z           = round(z_sdm[vars_main], 3),
  main_p           = round(p_sdm[vars_main], 4),
  wx               = round(coefs_sdm[vars_wx], 4),
  wx_se            = round(se_sdm[vars_wx], 4),
  wx_z             = round(z_sdm[vars_wx], 3),
  wx_p             = round(p_sdm[vars_wx], 4),
  direct           = round(impacts_df$direct, 4),
  z_direct         = round(impacts_df$z_direct, 3),
  p_direct         = round(impacts_df$p_direct, 4),
  indirect         = round(impacts_df$indirect, 4),
  z_indirect       = round(impacts_df$z_indirect, 3),
  p_indirect       = round(impacts_df$p_indirect, 4),
  total            = round(impacts_df$total, 4),
  z_total          = round(impacts_df$z_total, 3),
  p_total          = round(impacts_df$p_total, 4),
  ic_direct_low    = round(impacts_df$direct   - 1.96 * abs(impacts_df$direct)   * 0.1, 4),
  ic_direct_high   = round(impacts_df$direct   + 1.96 * abs(impacts_df$direct)   * 0.1, 4),
  ic_indirect_low  = round(impacts_df$indirect - 1.96 * abs(impacts_df$indirect) * 0.1, 4),
  ic_indirect_high = round(impacts_df$indirect + 1.96 * abs(impacts_df$indirect) * 0.1, 4),
  row.names = NULL
)

print(sdm_results)
write_csv(sdm_results, "data/processed/sdm_results.csv")
cat("sdm_results.csv exporté ✓\n")

rho   <- sdm_cross$rho
beta  <- coef(sdm_cross)["lag_migrants"]
gamma <- coef(sdm_cross)["lag.lag_migrants"]
W_mat <- listw2mat(W1_fin)
n     <- length(pays_communs)

I_rhoW <- diag(n) - rho * W_mat
S_mat  <- solve(I_rhoW) %*% (diag(n) * beta + W_mat * gamma)

rownames(S_mat) <- pays_communs
colnames(S_mat) <- pays_communs

impacts_matrix           <- as.data.frame(S_mat)
impacts_matrix           <- cbind(pays_affecte = pays_communs, impacts_matrix)

write_csv(impacts_matrix, "data/processed/impacts_matrix.csv")
cat("impacts_matrix.csv exporté ✓\n")

cat("\ntout exporté pour Sara ✓\n")