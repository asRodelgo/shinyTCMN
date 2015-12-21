# treempa chart ----------------------------------------------------
GVA_Treemap_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".GVA_Treemap", args = list(
    couName = input$inCouSel
  ))
})

output$GVA_Treemap <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .GVA_Treemap(input$inCouSel)
  #})
}, bg = "transparent")
