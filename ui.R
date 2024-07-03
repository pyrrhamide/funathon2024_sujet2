# couleur de la barre en haut
main_color <- "purple"

# définir le thème (pour l'instant que la police)
theme_plane <- bs_theme(
  base_font = font_google("Work Sans"),
  heading_font = font_google("Work Sans")
)

# définir le widget de date 
input_date <- shinyWidgets::airDatepickerInput(
  "date",
  label = "Choisir un mois",
  value = "2019-01-01",
  view = "months",
  minView = "months",
  minDate = dt_deb,
  maxDate = dt_fin,
  dateFormat = "MMMM yyyy",
  language = "fr"
)

# définir le widget pour sélectionner le ou les aéroports
input_airport <- selectInput(
  "select",
  "Choisir un ou plusieurs aéroports",
  choices = liste_aeroports,
  selected = default_airport,
  multiple = T,
  selectize = T # si FALSE, le menu déroulant est pas très fashion 
)

# définir le pied de page (j'arrive pas à en faire un lien)
footer <- tags$footer(
  shiny::icon("github"), "Projet issu du funathon 2024, organisé par l'Insee et la DGAC",
  href = "https://github.com/pyrrhamide/funathon2024_sujet2.git",
  target = "_blank"
  , tags$style(HTML('footer {text-align : center; padding-bottom : 15px};'))
)

# construire le dashbord :)
ui <- page_navbar(
  title = "Tableau de bord des aéroports français",
  theme = theme_plane,
  bg = main_color,
  inverse = TRUE,
  footer = footer,
  
  # onglet "Trafic"
  nav_panel(
    "Trafic", 
    layout_columns(
      # Gauche : tableau des fréquentations
      card(
        card_header("Flux de fréquentation des aéroports français",
                    class = c("bg-pink", "text-center", "fs-6")),
        input_date,
        gt_output(outputId = "table"),
        card_footer(markdown("_Source_ : DGAC, à partir des données sur data.gouv.fr"))
      ),
      layout_columns(
        # Droite haut : carte
        card(
          card_header("Localisation des aéroports français",
                      class = c("bg-pink", "text-center", "fs-6")), 
          card(leafletOutput("carte")) 
        ),
        # Droite bas : graphique 
        card(card_header("Fréquentation par aéroport (trafic mensuel)", 
                         class = c("bg-pink", "text-center", "fs-6")),
             input_airport,
             plotlyOutput("lineplot") # faut définir précisément chaque objet
        ),
        col_widths = c(12,12)
      ),
      cols_widths = c(12,12,12)
    )
  ),
  
  # onglet "Liaisons" -- vide
  nav_panel("Liaisons"),
  
  # onglet "Compagnies" -- vide
  nav_panel("Compagnies")
)
