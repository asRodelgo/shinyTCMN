# line chart ----------------------------------------------------
ExpImp_HF_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".ExpImp_HF", args = list(
    couName = input$inCouSel
  ))
})

output$ExpImp_HF <- renderPlot({
   #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
    .ExpImp_HF(input$inCouSel)
  #})
}, bg = "transparent")
