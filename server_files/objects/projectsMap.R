# projectsMap ------------------------------
output$projectsMap <- renderLeaflet({
  
  .projectsMap(input$inCouSel)
  
})

# When map is clicked, show a popup with city info
observe({
  leafletProxy("projectsMap") %>% clearPopups()
  event <- input$map_shape_click
  if (is.null(event))
    return()
  
  isolate({
    .showPopup(event$id, event$lat, event$lng)
  })
})
