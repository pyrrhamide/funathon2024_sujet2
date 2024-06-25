tableau_interactif <- function(tableau, ...) {
  tableau %>% 
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
}
