# Plot tSNE ---------------------

output$plotTSNE <- renderPlot({
  plotTSNE <- .tSNE_plot_All(input$colRegion,input$colPeriod,input$colCountry,input$colIndicator)
                             #,input$centralMeasure)
  return(plotTSNE)
})

# tooltip hover over scatterplot points: see https://gitlab.com/snippets/16220
output$hover_info <- renderUI({
  hover <- input$plot_hover
  point <- nearPoints(.tSNE_plot_filter_hover(input$colRegion,input$colPeriod,input$colCountry,
                                        input$colIndicator), hover, threshold = 3, maxpoints = 1, addDist = TRUE)
  
  if (nrow(point) == 0) return(NULL)
  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
  
  # calculate distance from left and bottom side of the picture in pixels
  # avoid overlapping with other objects by keeping the tooltip inside the frame
  if (left_pct > .75){
    if (top_pct >.75){
      left_px <- -25*hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- -15*hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    } else {
      left_px <- -25*hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    }
  } else {
    
    if (top_pct >.75){
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- -15*hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    } else{
      left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
      top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    }
  }
  
  
  # create style property fot tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure are tooltip will be on top
  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                  "left:", left_px + 2, "px; top:", top_px + 2, "px;")
  
  # actual tooltip created as wellPanel
  wellPanel(
    style = style,
    p(HTML(paste0(point$CountryShort, "<br/>",point$RegionShort," - ",point$Period,"<br/>",
                  input$colIndicator,": ",eval(parse(text=paste0("point$",input$colIndicator))))))
  )
})

output$plotTSNEdensities <- renderPlot({
  
  click <- input$plot_click
  point <- nearPoints(.tSNE_plot_filter(input$colRegion,input$colPeriod,input$colCountry,
                                        input$colIndicator), click, threshold = 3, maxpoints = 1, addDist = TRUE)
  
  if (nrow(point) == 0){
    plotTSNEdensities <- .densityPlots(input$colRegion,input$colPeriod,input$colCountry,
                                       input$colIndicator,NULL,NULL)
  } else {
    plotTSNEdensities <- .densityPlots(input$colRegion,input$colPeriod,input$colCountry,
                                       input$colIndicator,point$CountryShort,point$Period)
  }
  return(plotTSNEdensities)
})

output$plotRadarBrushed <- renderPlot({
  
  brush <- input$plot_brush
  pointsBrushed <- brushedPoints(.tSNE_plot_filter(input$colRegion,input$colPeriod,input$colCountry,
                                                   input$colIndicator), brush)
  
  plotRadarBrushed <- .radarPlot_base(pointsBrushed)
  
  return(plotRadarBrushed)
  
})

output$plotBarchartBrushed <- renderPlot({
  
  brush <- input$plot_brush
  pointsBrushed <- brushedPoints(.tSNE_plot_filter(input$colRegion,input$colPeriod,input$colCountry,
                                                   input$colIndicator,input$explore_variables), brush)
  
  plotRadarBrushed <- .bar_chart(pointsBrushed,input$explore_variables)
  
  return(plotRadarBrushed)
  
})

output$tableBrushed <- DT::renderDataTable({
  brush <- input$plot_brush
  pointsBrushed <- brushedPoints(.tSNE_plot_filter(input$colRegion,input$colPeriod,input$colCountry,
                                                   input$colIndicator,input$explore_variables), brush)
  tableBrushed <- .brushTable(pointsBrushed,input$explore_variables)
  return(tableBrushed)
},options = list(dom = 't', pageLength = 25, paging = TRUE),rownames= FALSE)
