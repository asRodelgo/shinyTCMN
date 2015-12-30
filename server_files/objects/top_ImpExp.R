# imports -----------------------------------------------
topImport_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".topImport", args = list(
    couName = input$inCouSel, type = "m"
  ))
})

output$topImport <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .ImpExp_Treemap(input$inCouSel, "m")
  #})
}, bg = "transparent")

# exports -----------------------------------------------
topExport_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".topExport", args = list(
    couName = input$inCouSel, type="x"
  ))
})

output$topExport <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .ImpExp_Treemap(input$inCouSel,"x")
  #})
}, bg = "transparent")