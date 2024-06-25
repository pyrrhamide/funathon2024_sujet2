# Graphique trafic
plot_airport_line <- function(df, selected_airport) {
  df %>% 
    filter(apt == selected_airport) %>% 
    plot_ly(x = ~date, y = ~trafic, 
            text = ~apt_nom, 
            hovertemplate = paste("<i>Aéroport:</i> %{text}<br>Trafic: %{y}") ,
            type = "scatter", mode = "lines+markers")
}

# Carte trafic + localisation aéroport 

## icones
icons <- awesomeIcons(
  icon = 'plane',
  iconColor = 'black',
  library = 'fa',
  markerColor = trafic_aeroports$couleur
)

map_leaflet_airport <- function(df, in_an, in_mois){
  df %>% 
    filter(an == in_an, mois == in_mois) %>% 
    leaflet() %>% 
    addTiles() %>% 
    # colorer les pics en fonction de leur tertile
    addAwesomeMarkers(icon = icons,
                      popup = ~str_glue("{str_to_title(apt_nom)} - {trafic} passagers"), 
                      popupOptions = popupOptions(
                        style = list(
                          "font-family" = "Marianne"
                        )
                      ),
                      label = ~str_to_title(apt_nom),
                      labelOptions = labelOptions(
                        style = list(
                          "font-family" = "Marianne",
                          "font-size" = "12px"
                        )
                      ))
}
