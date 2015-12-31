# Logistics Performance  ----------------------------------------------------
WGI_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".WGIindicators", args = list(
    couName = input$inCouSel, neighbor = input$inNeighbor
  ))
})

output$WGI <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .WGIindicators(input$inCouSel,input$inNeighbor)
  #})
}, bg = "transparent")