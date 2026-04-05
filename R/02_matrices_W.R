setwd("D:/M1 DS 25 26/SEMESTRE 1/UE CULTURE GENERALE/ECONOMIE/econometrie/Econometric_projet/migrations_conflits_ASS")

library(tidyverse)
library(sf)
library(spdep)
library(haven)

panel <- readRDS("data/processed/panel_clean.rds")

ssa_countries <- unique(panel$iso3)

shp_ssa <- st_read("data/raw/ne_50m_admin_0_countries") |>
  filter(ISO_A3 %in% ssa_countries) |>
  arrange(ISO_A3)

cepii_clean <- read_dta("data/raw/dist_cepii.dta") |>
  filter(iso_o %in% ssa_countries & iso_d %in% ssa_countries) |>
  select(iso_o, iso_d, distcap)

cat("Pays dans le panel :", length(ssa_countries), "\n")
cat("Pays dans le shapefile :", nrow(shp_ssa), "\n")


nb_queen <- poly2nb(shp_ssa, queen = TRUE)

W1_listw <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)

W1_matrix <- listw2mat(W1_listw)

cat("W1 construite ✓\n")
cat("Dimensions :", nrow(W1_matrix), "×", ncol(W1_matrix), "\n")
summary(nb_queen)


rownames(W1_matrix) <- shp_ssa$ISO_A3
colnames(W1_matrix) <- shp_ssa$ISO_A3

saveRDS(W1_listw, "data/processed/W1_listw.rds")
write.csv(W1_matrix, "data/processed/W1_matrix.csv")
cat("W1 exportée ✓\n")


##W2
pays <- shp_ssa$ISO_A3

dist_mat <- matrix(0, nrow = length(pays), ncol = length(pays))
rownames(dist_mat) <- pays
colnames(dist_mat) <- pays

for (i in pays) {
  for (j in pays) {
    if (i != j) {
      d <- cepii_clean |>
        filter(iso_o == i & iso_d == j) |>
        pull(distcap)
      if (length(d) > 0) dist_mat[i, j] <- d
    }
  }
}

min_dist <- min(dist_mat[dist_mat > 0])
W2_raw <- ifelse(dist_mat > 0, min_dist / dist_mat, 0)
W2_raw <- W2_raw / rowSums(W2_raw)

W2_listw <- mat2listw(W2_raw, style = "W")

saveRDS(W2_listw, "data/processed/W2_listw.rds")
cat("W2 construite et exportée ✓\n")
cat("Dimensions :", nrow(W2_raw), "×", ncol(W2_raw), "\n")

cepii_clean <- read_dta("data/raw/dist_cepii.dta") |>
  mutate(
    iso_o = case_when(iso_o == "ZAR" ~ "COD", TRUE ~ iso_o),
    iso_d = case_when(iso_d == "ZAR" ~ "COD", TRUE ~ iso_d)
  ) |>
  filter(iso_o %in% ssa_countries & iso_d %in% ssa_countries) |>
  select(iso_o, iso_d, distcap)

cepii_clean |> filter(iso_o == "COD") |> head(3)


#distance Soudans du sud
library(geosphere)

juba_lon <- 31.6
juba_lat <- 4.85

coords <- st_centroid(shp_ssa) |>
  st_coordinates() |>
  as.data.frame() |>
  mutate(iso3 = shp_ssa$ISO_A3) |>
  rename(lon = X, lat = Y)

ssd_distances <- coords |>
  mutate(
    dist_ssd = distHaversine(
      cbind(lon, lat),
      c(juba_lon, juba_lat)
    ) / 1000
  ) |>
  select(iso3, dist_ssd)

ssd_distances