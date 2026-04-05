#Chargement de librairies
setwd("D:/M1 DS 25 26/SEMESTRE 1/UE CULTURE GENERALE/ECONOMIE/econometrie/Econometric_projet/migrations_conflits_ASS")

library(tidyverse)
library(haven)

library(sf)

#Vecteur de code
ssa_countries <- c(
  "AGO","BEN","BWA","BFA","BDI","CMR","CAF","TCD",
  "COD","COG","CIV","DJI","ERI","ETH","GAB","GMB",
  "GHA","GIN","GNB","GNQ","KEN","LSO","LBR","MDG",
  "MWI","MLI","MRT","MOZ","NAM","NER","NGA","RWA",
  "SEN","SLE","SOM","ZAF","SSD","SDN","SWZ","TZA",
  "TGO","UGA","ZMB","ZWE"
)


wdi_raw <- read_csv("data/raw/wdi_data.csv")

wdi <- wdi_raw |>
  filter(`Country Code` %in% ssa_countries) |>
  select(-`Country Name`, -`Series Name`) |>
  rename(iso3 = `Country Code`, indicateur = `Series Code`) |>
  pivot_longer(
    cols = starts_with("19") | starts_with("20"),
    names_to = "annee",
    values_to = "valeur"
  ) |>
  mutate(annee = as.integer(str_extract(annee, "\\d{4}"))) |>
  filter(annee >= 1995, annee <= 2024) |>
  mutate(valeur = na_if(valeur, "..")) |>
  mutate(valeur = as.numeric(valeur)) |>
  pivot_wider(names_from = indicateur, values_from = valeur) |>
  rename(
    migrants   = SM.POP.TOTL,
    pib_pc     = NY.GDP.PCAP.KD,
    ressources = NY.GDP.TOTL.RT.ZS,
    population = SP.POP.TOTL
  )

glimpse(wdi)
nrow(wdi)
n_distinct(wdi$iso3)


ucdp_raw <- read_csv("data/raw/ucdp_ged.csv")

glimpse(ucdp_raw)
names(ucdp_raw)

ucdp_afrique <- ucdp_raw |>
  filter(region == "Africa") |>
  filter(year >= 1996, year <= 2024)

unique(ucdp_afrique$country)

#Correspondance ucdp
ucdp_iso <- c(
  "Algeria" = "DZA",
  "Angola" = "AGO",
  "Benin" = "BEN",
  "Burkina Faso" = "BFA",
  "Burundi" = "BDI",
  "Cameroon" = "CMR",
  "Central African Republic" = "CAF",
  "Chad" = "TCD",
  "Congo" = "COG",
  "Djibouti" = "DJI",
  "DR Congo (Zaire)" = "COD",
  "Eritrea" = "ERI",
  "Ethiopia" = "ETH",
  "Gambia" = "GMB",
  "Ghana" = "GHA",
  "Guinea" = "GIN",
  "Guinea-Bissau" = "GNB",
  "Ivory Coast" = "CIV",
  "Kenya" = "KEN",
  "Kingdom of eSwatini (Swaziland)" = "SWZ",
  "Lesotho" = "LSO",
  "Liberia" = "LBR",
  "Madagascar (Malagasy)" = "MDG",
  "Mali" = "MLI",
  "Mauritania" = "MRT",
  "Mozambique" = "MOZ",
  "Namibia" = "NAM",
  "Niger" = "NER",
  "Nigeria" = "NGA",
  "Rwanda" = "RWA",
  "Senegal" = "SEN",
  "Sierra Leone" = "SLE",
  "Somalia" = "SOM",
  "South Africa" = "ZAF",
  "South Sudan" = "SSD",
  "Sudan" = "SDN",
  "Tanzania" = "TZA",
  "Togo" = "TGO",
  "Uganda" = "UGA",
  "Zambia" = "ZMB",
  "Zimbabwe (Rhodesia)" = "ZWE"
)

ucdp_clean <- ucdp_afrique |>
  mutate(iso3 = ucdp_iso[country]) |>
  filter(!is.na(iso3)) |>
  filter(iso3 %in% ssa_countries) |>
  group_by(iso3, year) |>
  summarise(
    n_events = n(),
    n_deaths = sum(best, na.rm = TRUE),
    .groups = "drop"
  ) |>
  rename(annee = year)

glimpse(ucdp_clean)
n_distinct(ucdp_clean$iso3)

polity_raw2 <- read_csv2("data/raw/polity5_annual.csv")
glimpse(polity_raw2)
"year" %in% names(polity_raw2)


polity_iso <- c(
  "ALG" = "DZA",
  "ANG" = "AGO",
  "BEN" = "BEN",
  "BOT" = "BWA",
  "BFO" = "BFA",
  "BUI" = "BDI",
  "CAO" = "CMR",
  "CEN" = "CAF",
  "CHA" = "TCD",
  "ZAI" = "COD",
  "CON" = "COG",
  "IVO" = "CIV",
  "DJI" = "DJI",
  "EQG" = "GNQ",
  "ERI" = "ERI",
  "ETI" = "ETH",
  "GAB" = "GAB",
  "GAM" = "GMB",
  "GHA" = "GHA",
  "GUI" = "GIN",
  "GNB" = "GNB",
  "KEN" = "KEN",
  "LES" = "LSO",
  "LBR" = "LBR",
  "MAG" = "MDG",
  "MAW" = "MWI",
  "MLI" = "MLI",
  "MAA" = "MRT",
  "MZM" = "MOZ",
  "NAM" = "NAM",
  "NIR" = "NER",
  "NIG" = "NGA",
  "RWA" = "RWA",
  "SEN" = "SEN",
  "SIE" = "SLE",
  "SOM" = "SOM",
  "SAF" = "ZAF",
  "SSD" = "SSD",
  "SUD" = "SDN",
  "SWA" = "SWZ",
  "TAZ" = "TZA",
  "TOG" = "TGO",
  "UGA" = "UGA",
  "ZAM" = "ZMB",
  "ZIM" = "ZWE"
)

polity_clean <- polity_raw2 |>
  mutate(iso3 = polity_iso[scode]) |>
  filter(!is.na(iso3)) |>
  filter(iso3 %in% ssa_countries) |>
  filter(year >= 1996 & year <= 2024) |>
  select(iso3, year, polity2) |>
  rename(annee = year)

glimpse(polity_clean)
n_distinct(polity_clean$iso3)


shp_raw <- st_read("data/raw/ne_50m_admin_0_countries")

shp_ssa <- shp_raw |>
  filter(ISO_A3 %in% ssa_countries)

nrow(shp_ssa)

shp_ssa$ISO_A3

setdiff(shp_ssa$ISO_A3, ssa_countries)


panel_base <- expand.grid(
  iso3  = ssa_countries,
  annee = 1995:2024
)

panel <- panel_base |>
  left_join(wdi, by = c("iso3", "annee")) |>
  left_join(ucdp_clean, by = c("iso3", "annee")) |>
  left_join(polity_clean, by = c("iso3", "annee")) |>
  mutate(
    n_events = replace_na(n_events, 0),
    n_deaths = replace_na(n_deaths, 0)
  )

glimpse(panel)
nrow(panel)




panel <- panel |>
  mutate(
    conflits_pc    = ifelse(population > 0, n_events / population * 100000, NA),
    deces_pc       = ifelse(population > 0, n_deaths / population * 100000, NA),
    log_conflits   = log(conflits_pc + 1),
    log_deces      = log(deces_pc + 1),
    log_migrants   = log(migrants + 1),
    log_pib        = log(pib_pc),
    log_ressources = log(ressources + 1),
    periode        = ifelse(annee <= 2010, "1996-2010", "2011-2024")
  ) |>
  arrange(iso3, annee) |>
  group_by(iso3) |>
  mutate(
    lag_conflits   = lag(log_conflits),
    lag_migrants   = lag(log_migrants),
    lag_pib        = lag(log_pib),
    lag_polity     = lag(polity2),
    lag_ressources = lag(log_ressources)
  ) |>
  ungroup()

glimpse(panel)




panel |>
  summarise(across(everything(), ~sum(is.na(.)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") |>
  mutate(pct_na = round(n_na / nrow(panel) * 100, 1)) |>
  arrange(desc(pct_na))

panel_final <- panel |> filter(annee >= 1996)

saveRDS(panel_final, "data/processed/panel_clean.rds")
write_csv(panel_final, "data/processed/panel_clean.csv")

cat("Panel exporté :", nrow(panel_final), "lignes\n")
cat("Pays :", n_distinct(panel_final$iso3), "\n")
cat("Années :", min(panel_final$annee), "→", max(panel_final$annee), "\n")

panel_final <- panel_final |>
  group_by(iso3) |>
  mutate(
    migrants       = zoo::na.approx(migrants, na.rm = FALSE),
    log_migrants   = log(migrants + 1),
    lag_migrants   = lag(log_migrants)
  ) |>
  ungroup()

panel_final |>
  summarise(pct_na_migrants = sum(is.na(migrants)) / n() * 100)


saveRDS(panel_final, "data/processed/panel_clean.rds")
write_csv(panel_final, "data/processed/panel_clean.csv")
cat("Panel final exporté ✓\n")
cat("Lignes :", nrow(panel_final), "\n")
cat("NA migrants :", sum(is.na(panel_final$migrants)), "\n")