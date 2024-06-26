function(input, output) {
  
  output$table <- render_gt(
    summary_stat_airport(trafic_aeroports, year(input$date), month(input$date)) %>% 
      select(-apt) %>% 
      tableau_interactif() %>% 
      opt_css(
        css = "#pax-freq {font-size:small};"
      )
  )
  
  output$carte <- renderLeaflet(
    map_leaflet_airport(
      trafic_aeroports,
      year(input$date), month(input$date)
    )
  )
  
  output$lineplot <- renderPlotly(
    plot_airport_line(trafic_aeroports, input$select)
  )
  
  # output$date1 <- renderText(input$date)
  # output$date2 <- renderText(input$date)
  # output$airport <- renderText(input$select)
  
}