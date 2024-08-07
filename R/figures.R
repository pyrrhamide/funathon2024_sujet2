# Graphique trafic
plot_airport_line <- function(df, selected_airport) {
  df %>% 
    filter(code_iata %in% selected_airport) %>% 
    plot_ly(x = ~date, y = ~trafic, color = ~code_iata,
            text = ~apt_nom, 
            hovertemplate = paste("<i>Aéroport:</i> %{text}<br>Trafic: %{y}") ,
            type = "scatter", mode = "lines+markers",
            colors = rcartocolor::carto_pal(n = 7, name = "Vivid")) 
}

# Carte trafic + localisation aéroport 


map_leaflet_airport <- function(df, in_an, in_mois){
  ## icones
  icons <- awesomeIcons(
    icon = 'plane',
    iconColor = 'black',
    library = 'fa',
    markerColor = df$couleur
  )
  
  df %>%
    filter(an == in_an, mois == in_mois) %>%
    leaflet() %>%
    addTiles() %>%
    # colorer les pics en fonction de leur tertile
    addAwesomeMarkers(icon = icons[],
                      popup = ~str_glue("{str_to_title(apt_nom)} - {format_chiffres(trafic)} voyageurs ({str_sub(anmois, -2)}/{an})"),
                      label = ~str_glue("{str_to_title(apt_nom)} ({code_iata})"),
                      labelOptions = labelOptions(
                        style = list(
                          # "font-family" = "Lato",
                          "font-size" = "12px"
                        )
                      ))
}
