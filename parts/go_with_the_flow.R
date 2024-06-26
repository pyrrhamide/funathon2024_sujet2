
#::::::::::::::::::::::::::::::::::::::
# Funathon 2024 - sujet 2
# Un tableau de bord du trafic aérien 
#::::::::::::::::::::::::::::::::::::::

# packages ----
library(dplyr)
library(lubridate)
library(stringr)
library(purrr)

# 1) se placer dans l'environnement du projet ----

## pour récupérer l'environnement de travail (à partir de renv.lock)
# renv::restore() # télécharge les packages à la bonne version - déjà fait tourner

## si nouveau package utilisé, pour faire une capture 
# renv::snapshot()

# 2) récupérer les données ----

## déb exo 1 ----
# sources.yaml : liste de tous les liens vers les données du TDB
# transformer ce fichier YAML en liste imbriquée 
yaml::read_yaml("sources.yml")

# transformer ce bout de code en une fonction create_data_list prenant un argument source_file et renvoyant cette liste 
create_data_list <- function(source_file) yaml::read_yaml(source_file)

# créer à la racine du projet un dossier R/
if (!dir.exists("R")) dir.create("R")

# dans le dossier, créer script create_data_list.R avec la fonction créée plus haut (je fais dans la console)

# fin exo 1

# importer les bases
## lister tous les URL 
urls <- create_data_list("sources.yml")
urls

## déb exo 2 ----

# données aéroports : csv, sep ";"
# je suis les recos de l'énoncé puis je fais à ma sauce
read_csv2(unlist(urls$airports)[1],
          col_types = cols(
            ANMOIS = col_character(),
            APT = col_character(),
            APT_NOM = col_character(),
            APT_ZON = col_character(),
            .default = col_double()
          )) %>% 
  # à partir de ANMOIS, créer an et mois
  mutate(an = str_sub(ANMOIS, 1, 4),
         mois = str_remove(str_sub(ANMOIS, -2), "^0")) %>% 
  rename_with(tolower)

# créer une fonction clean_dataframe qui prend en entrée un df, crée les variables an et mois, ajoute une étape de passage des noms de colonne en minuscule et renvoie le df en sortie 
clean_dataframe <- function(df){
  read_csv2(df,
            col_types = cols(
              ANMOIS = col_character(),
              APT = col_character(),
              APT_NOM = col_character(),
              APT_ZON = col_character(),
              .default = col_double()
            )) %>% 
    # à partir de ANMOIS, créer an et mois
    mutate(an = str_sub(ANMOIS, 1, 4),
           mois = str_remove(str_sub(ANMOIS, -2), "^0")) %>% 
    rename_with(tolower)
}

# créer une fonction import_airport_data qui prend en input list_files et intègre les deux étapes précédentes : la lecture des données, le nettoyage avec clean_dataframe 
import_airport_data <- function(list_files) clean_dataframe(list_files)

airports <- import_airport_data(unlist(urls$airports)) # ce qui est cool : les données sont immédiatement empilées 

# comment j'aurais fait, en pure tidyverse
# on prend aussi clean_dataframe, mais je fais pas de unlist
map(urls$airports, clean_dataframe) %>% list_rbind()

# données compagnies
# freestyle
# 1 - je regarde à quoi ça ressemble, et je détermine le type des colonnes directos 
read_csv2(urls$compagnies[[1]]) %>% glimpse()
# seulement ANMOIS à changer le type (et peut-être CIE_VOL ? pas sûre)
read_csv2(urls$compagnies[[1]]) %>% count(CIE_VOL) # nope 

# reco col_types veut vraiment rien laisser au hasard, je vais faire comme ça alors
import_compagnies_data <- function(df) {
  read_csv2(df,
            col_types = cols(
              ANMOIS = col_character(),
              CIE = col_character(),
              CIE_NOM = col_character(),
              CIE_NAT = col_character(),
              CIE_PAYS = col_character(),
              .default = col_double()
            ))%>% 
    # à partir de ANMOIS, créer an et mois
    mutate(an = str_sub(ANMOIS, 1, 4),
           mois = str_remove(str_sub(ANMOIS, -2), "^0")) %>% 
    rename_with(tolower)
}

compagnies <- map(urls$compagnies, import_compagnies_data) %>% list_rbind()

# données liaisons
# même chanson
read_csv2(urls$liaisons[[1]]) %>% glimpse()
# flemme de faire les col_types. je fais les trois (airports, compagnies, liaisons) en one-shot, parce que read_csv2 est assez intelligent pour détecter les strings + le seul truc à tout le temps surveiller est ANMOIS 
import_data <- function(df){
  read_csv2(df,
            col_types = cols(ANMOIS = col_character())) %>% 
    # à partir de ANMOIS, créer an et mois
    mutate(an = str_sub(ANMOIS, 1, 4),
           mois = str_remove(str_sub(ANMOIS, -2), "^0")) %>% 
    rename_with(tolower)
}

liaisons <- map(urls$liaisons, import_data) %>% list_rbind()
# peut s'appliquer aux deux autres 

# données localisations des aéroports 
# jeu de données spatial ! 
library(sf)

airports_location <- st_read(dsn = unlist(urls$geojson))
head(airports_location)

# faire une carte pour vérifier que tout est bien placé
leaflet::addMarkers(airports_location) # oh j'ai besoin des latitudes et longitudes 
st_coordinates(airports_location)
# après vérif sur le WWW, X=longitude, Y=latitude 
airports_location %>% 
  mutate(x = st_coordinates(airports_location))
# ah mais c'est pas nécessaire, leaflet fait tout !
# ils ont pas méga bien instruits sur le coup, la correction est affichée plus bas + ils mentionnent leaflet::addMarkers comme ça, sans plus de précisions 

leaflet::leaflet(airports_location) %>% # lance la carte
  leaflet::addTiles() %>% # fond de carte
  leaflet::addMarkers(popup = ~Nom) # pour avoir un "message" quand on clique sur la pointe 

# ok, maintenant ils veulent tout centraliser dans un script main.R