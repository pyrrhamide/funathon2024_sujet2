plot_airport_line <- function(df, selected_airport) {
  df %>% 
    filter(apt == selected_airport) %>% 
    plot_ly(x = ~date, y = ~trafic, 
            text = ~apt_nom, 
            hovertemplate = paste("<i>Aéroport:</i> %{text}<br>Trafic: %{y}") ,
            type = "scatter", mode = "lines+markers")
}