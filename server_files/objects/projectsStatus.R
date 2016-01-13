# Projects portfolio Status  ----------------------------------------------------
projectsStatus <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".projectsStatus", args = list(
    couName = input$inCouSel, count_type = input$inProjStatusChart, dateRange = input$projectDateRange
  ))
})

output$projectsStatus <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .projectsStatus(input$inCouSel,input$inProjStatusChart,input$projectDateRange)
  #})
}, bg = "transparent")