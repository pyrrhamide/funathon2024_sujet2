
# Packages ----
library(tidyverse)
library(sf)
library(leaflet)


# Paramètres ----


# 1) on importe les fonctions créées dans les scripts ----
source('R/create_data_list.R')
source('R/import_data.R')
source('R/plot_airport_line.R')


# 2) on importe les données ----

## liste de tous les liens, à partir de sources.yml
urls <- yaml::read_yaml("sources.yml")

## on importe les données airports, compagnies et liaisons 
pax_apt_all <- map(urls$airports, import_data) %>% list_rbind()
pax_cie_all <- map(urls$compagnies, import_data) %>% list_rbind()
pax_lsn_all <- map(urls$liaisons, import_data) %>% list_rbind()

## on importe les localisations des aéroports
airports_location <- st_read(urls$geojson)

# elle a déjà toutes les infos des autres df...ça simplifie beaucoup trop l'exercice...
# je la réduis pour mettre du piment dans la vie
airports_location_rec <- airports_location %>% 
  select(!starts_with(c("apt_pax", "apt_frp", "apt_nmvt")),
         -c(anmois:color)) %>% 
  janitor::clean_names()


# 3) visualisation des données ----

# BROUILLON (on suit de nouveau les instructions)
liste_aeroports <- unique(pax_apt_all$apt)
default_airport <- liste_aeroports[1] # MAYOTTE

## Valorisation 1: le trafic par aéroport ----
pax_apt_all <- pax_apt_all %>% 
  mutate(trafic = rowSums(pick(starts_with("apt_pax_"))),
         date = ym(paste(an, mois))) 

# figure STATIQUE
pax_apt_all %>% 
  filter(apt == default_airport) %>% 
  ggplot(aes(x = date, y = trafic)) +
  geom_line()

# figure DYNAMIQUE 
library(plotly)

## moi
pax_apt_all %>% 
  filter(apt == default_airport) %>% 
  plot_ly(x = ~date, y = ~trafic, type = "scatter", mode = "lines") %>% 
  add_trace(mode = "markers")

## la correction (c'est pas au bon endroit, jipeps)
pax_apt_all %>% 
  filter(apt == default_airport) %>% 
  plot_ly(x = ~date, y = ~trafic, 
          text = ~apt_nom, 
          hovertemplate = paste("<i>Aéroport:</i> %{text}<br>Trafic: %{y}") ,
          type = "scatter", mode = "lines+markers")
# ils en font une fonction, mais à chaque fois ils recréent le df avec l'aéroport, recalculent le trafic et la date...stupide 
# j'ai fait la fonction pour le graphique, mais à partir du df déjà complété 
plot_airport_line(pax_apt_all, "FMEE") # FMEE = RUN 

## Valorisation 2 : tableau HTML pour afficher des données ----
library(gt)

years_list <- 2018:2022
months_list <- 1:12

pax_apt_all %>% 
  filter(an == 2021, mois == 11) %>% 
  select(date, apt, apt_nom, starts_with("apt_pax_"), trafic) %>% 
  arrange(-trafic)
# fonctions sur fonctions, incroyable 
summary_stat_airport(pax_apt_all, 2019, 6)

# nettoyage nom aéroport
pax_apt_all <- pax_apt_all %>% 
  mutate(apt_nom_clean = paste0(str_to_title(apt_nom), " _(", apt, ")_"), .before = apt_nom)
# les underscores pour que ça devienne italique dans le TDB 

# les beaux tableaux 
summary_stat_airport(pax_apt_all, 2019, 6) %>% 
  select(-apt) %>% 
  gt() %>% 
  # nombres concis : mille-K, millions-M
  fmt_number(suffixing = T) %>% 
  # mise en forme adaptée sur le nom des aéroports 
  fmt_markdown(columns = apt_nom_clean) %>% 
  cols_label(apt_nom_clean = "Aéroport",
             apt_pax_dep = "Départs",
             apt_pax_arr = "Arrivées",
             apt_pax_tr = "Transit",
             trafic = "Trafic") %>% 
  tab_header(title = "Statistiques de fréquentation",
             subtitle = "Classement des aéroports") %>% 
  tab_source_note("Source : DGAC, à partir des données sur data.gouv.fr") %>% 
  tab_options(table.font.names = "Marianne",
              column_labels.font.weight = "bold") %>% 
  opt_interactive()

# => devenu une fonction
summary_stat_airport(pax_apt_all, 2021, 9) %>% 
  select(-apt) %>% 
  tableau_interactif()

## Valorisation 3 : carte des aéroports ----
# carte intéractive du trafic des aéroports

# ils demandent une palette de trois couleurs, j'aime pas le simple green/blue/red tho 
# pour les marqueurs d'aéroport 
palette <- c("green", "blue", "red")

pax_apt_all %>% 
  filter(an == 2022, mois == 11) %>% 
  # jointure sur coordonnées des aéroports 
  left_join(airports_location_rec, by = join_by(apt == code_oaci))
