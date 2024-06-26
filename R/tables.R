tableau_interactif <- function(tableau, ...) {
  tableau %>% 
    gt(id = "pax-freq") %>% 
    # nombres concis : mille-K, millions-M
    fmt_number(suffixing = T) %>% 
    # mise en forme adaptée sur le nom des aéroports 
    fmt_markdown(columns = apt_nom_clean2) %>% 
    cols_label(apt_nom_clean2 = md("**Aéroport**"),
               apt_pax_dep = md("**Départs**"),
               apt_pax_arr = md("**Arrivées**"),
               apt_pax_tr = md("**Transit**"),
               trafic = md("**Trafic**")) %>% 
    tab_header(title = "Statistiques de fréquentation",
               subtitle = md("_Classement des aéroports_")) %>% 
    # tab_source_note(md("_Source_ : DGAC, à partir des données sur data.gouv.fr")) %>% 
    tab_options(table.font.names = "Marianne",
                column_labels.font.weight = "bold") %>% 
    opt_interactive(
      use_search = TRUE,
      use_filters = TRUE,
      use_compact_mode = TRUE,
      use_page_size_select = TRUE,
      page_size_default = 15,
      pagination_type = "jump",
      use_highlight = TRUE
      )
}
