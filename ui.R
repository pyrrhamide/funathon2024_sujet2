main_color <- "purple"

input_date <- shinyWidgets::airDatepickerInput(
  "date",
  label = "Mois choisi",
  value = "2019-01-01",
  view = "months",
  minView = "months",
  minDate = dt_deb,
  maxDate = dt_fin,
  dateFormat = "MMMM yyyy",
  language = "fr"
)

input_airport <- selectInput(
  "select",
  "Choisir un ou plusieurs aéroports",
  choices = liste_aeroports,
  selected = default_airport,
  multiple = T,
  selectize = T # si FALSE, le menu déroulant est pas très fashion 
)

ui <- page_navbar(
  title = "Tableau de bord des aéroports français",
  id = "titre-tdb",
  bg = main_color,
  inverse = TRUE,
  header = em("Projet issu du funathon 2024, organisé par l'Insee et la DGAC"),
  nav_panel("Trafic", layout_columns(
    card(
      card_header("Flux de fréquentation des aéroports français",
                  class = c("bg-pink", "text-center", "fs-6")),
      input_date,
      gt_output(outputId = "table"),
      card_footer(markdown("_Source_ : DGAC, à partir des données sur data.gouv.fr"))
      # textOutput("date1")
      # table viendra ici
    ),
    layout_columns(
      card(
        # carte viendra ici
        # textOutput("date2")
        card_header("Localisation des aéroports français",
                    class = c("bg-pink", "text-center", "fs-6")), # pour ajouter un encadré titre :)
        card(leafletOutput("carte")) # indique qu'une carte sera ici ?
      ),
      card(card_header("Fréquentation par aéroport (trafic mensuel)", 
                       class = c("bg-pink", "text-center", "fs-6")),
           input_airport,
           plotlyOutput("lineplot") # faut définir précisément chaque objet
           # textOutput("airport")
           # figure viendra ici
      ),
      col_widths = c(12,12)
    ),
    cols_widths = c(12,12,12)
  )),
  nav_panel("Liaisons"),
  nav_panel("Compagnies")
)
