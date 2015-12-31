# Competitiveness Indicators  ----------------------------------------------------
Compet_Indic_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".WEFradar", args = list(
    couName = input$inCouSel, couName2 = input$inCouSel2
  ))
})

output$compet_Indic <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .WEFradar(input$inCouSel,input$inCouSel2)
  #})
}, bg = "transparent")
