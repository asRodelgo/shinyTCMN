# Logistics Performance  ----------------------------------------------------
LPI_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".LPIindicators", args = list(
    couName = input$inCouSel, couName2 = input$inCouSel2
  ))
})

output$LPI <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .LPIindicators(input$inCouSel,input$inCouSel2)
  #})
}, bg = "transparent")