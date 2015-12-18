# line chart ----------------------------------------------------
ExpImp_HF_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".ExpImp_HF", args = list(
    couName = input$inCouSel
  ))
})

output$ExpImp_HF <- renderPlot({
  .ExpImp_HF(couName)
}, bg = "transparent")
