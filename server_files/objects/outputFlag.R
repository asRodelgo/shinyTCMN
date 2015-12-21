# country flags ----------------------------------------------------
outputFlag <- reactive({
  
  do.call(".outFlag", args = list(
    couName = input$inCouSel
  ))
})

output$outFlag <- renderUI({
  
  .outFlag(input$inCouSel)
})
