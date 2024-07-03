
# Packages ----
library(tidyverse)
library(sf)
library(leaflet)
library(plotly)
library(gt)
library(shiny)
library(bslib)
library(arrow)

# Paramètres ----

# ils demandent une palette de trois couleurs, j'aime pas le simple green/blue/red tho 
# pour les marqueurs d'aéroport 
palette <- c("green", "blue", "red")


# 1) on importe les fonctions créées dans les scripts ----
source('R/create_data_list.R')
source('R/import_data.R')
source('R/figures.R')
source('R/fonctions_diverses.R')
source('R/tables.R')
source('R/format_chiffres.R')


# 2) on importe les données ----

## les sources ont changé depuis le Funathon, les tables
## passagers, compagnies et liaisons sont maintenant au format
## parquet et aggrègrent toutes les années 

## liste de tous les liens, à partir de sources.yml
urls <- yaml::read_yaml("sources.yml")

## on importe les données airports, compagnies et liaisons 
pax_apt_all <- import_data(urls$airports)
pax_cie_all <- import_data(urls$compagnies)
pax_lsn_all <- import_data(urls$liaisons)

## on importe les localisations des aéroports
airports_location <- st_read(urls$geojson$airport)

# elle a déjà toutes les infos des autres df...ça simplifie beaucoup trop l'exercice...
# je la réduis pour mettre du piment dans la vie
airports_location_rec <- airports_location %>%
  select(!starts_with(c("apt_pax", "apt_frp", "apt_nmvt")),
         -c(anmois:color)) %>%
  janitor::clean_names()

coord <- derivePoints(airports_location_rec)

airports_location_rec <- bind_cols(airports_location_rec, coord)


# 3) nettoyage de la table de trafic, pour avoir toutes les données dans un seul dataframe ----

trafic_aeroports <- pax_apt_all %>% 
  mutate(trafic = rowSums(pick(starts_with("apt_pax_"))),
         date = ym(paste(an, mois))) %>% 
  # nettoyage nom aéroport 
  mutate(apt_nom_clean = paste0(str_to_title(apt_nom), " _(", apt, ")_"), .before = apt_nom) %>% 
  # jointure sur coordonnées des aéroports 
  inner_join(airports_location_rec, by = join_by(apt == code_oaci)) %>%
  # nouveau apt_nom_clean, mais avec nom aéroport qu'on connait tous 
  mutate(apt_nom_clean2 = paste0(str_to_title(apt_nom), " _(", coalesce(code_iata, apt), ")_")) %>%
  # classer chaque observation dans un tertile et lui donner une couleur
  mutate(volume = ntile(trafic, n = 3),
         couleur = palette[volume], .by = c(an, mois)) 


# min et max des années et mois
dt_deb <- min(ym(trafic_aeroports$anmois))
dt_fin <- max(ym(trafic_aeroports$anmois))

YEARS_LIST <- sort(unique(trafic_aeroports$an))
MONTHS_LIST <- 1:12

# 4) les inputs du cadre "Fréquentation d'un aéroport" ----
liste_aeroports <- unique(trafic_aeroports$code_iata)
default_airport <- liste_aeroports[1] 

