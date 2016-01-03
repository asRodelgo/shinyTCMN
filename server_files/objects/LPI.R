# Logistics Performance  ----------------------------------------------------
LPI_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".LPIindicators", args = list(
    couName = input$inCouSel, couName2 = input$inCouSelLPI, period = input$inPeriodLPI
  ))
})

output$LPI <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .LPIindicators(input$inCouSel,input$inCouSelLPI,input$inPeriodLPI)
  #})
}, bg = "transparent")