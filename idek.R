glimpse(pax_lsn_all)

pax_lsn_all %>% 
  filter(anmois == "202311") %>% view()

pax_lsn_all %>% 
  select(where(is.character)) %>% 
  map(table)

# lsn = aeroport à aeroport
# lsn_dep_nom = aeroport de départ (je vais enlever les AUTRES)
# lsn_arr_nom = aeroport d'arrivée (bye les AUTRES aussi)

# lsn_sct (?)
# CC LC MC
# ah. long courrier, moyen courrier, court courrier
# ! CC ne veut pas dire que ça va pas à l'international, eg RUN <> MRU (tmtc)

# lsn_fsc = type de liaison DANS LES DEUX SENS 
# eg : MET_INTL = France métro -> International MAIS AUSSI International -> France métro
# MET_OM = outre-mer, MET_RAD = vol interne métro, MET_TRA = vol interne hors Paris ?
# MET_INTL MET_OM MET_RAD MET_TRA OM_INTL OM_OM

# j'enlève les autres, j'ai pas envie de les comprendre
liaisons <- pax_lsn_all %>% 
  filter(!str_sub(lsn, 1, 2) == "ZZ")

