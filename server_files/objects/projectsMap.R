# # projectsMap ------------------------------
output$projectsMap <- renderLeaflet({ # render map initially
  
  #.projectsMap(input$mapStatus,input$mapDateRange,input$mapProdLine)
  leaflet() %>%
    addTiles() %>%
    setView(lng = 0, lat = 0, zoom = 2)
  
})

# Observe user inputs on the map
observe({
  # Observe user inputs
  status <- input$mapStatus
  dateRange <- input$mapDateRange
  prodLine <- input$mapProdLine
  
  leafletProxy("projectsMap", data = .projectsMapData(status,dateRange,prodLine)) %>%
    clearMarkers() %>%
    addMarkers(lng = ~Long, lat = ~Lat, layerId=~countryCode) 
    # layerId identifies where to put the markers
    
})

# When map is clicked, show a popup with country info
observe({
  
  leafletProxy("projectsMap") %>% clearPopups()
  event <- input$projectsMap_marker_click
  if (is.null(event))
    return()
  
  isolate({
    .showPopup(input$mapStatus,input$mapDateRange,input$mapProdLine,event$id, event$lat, event$lng)
  })
})

