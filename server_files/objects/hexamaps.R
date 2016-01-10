# tsne chart ----------------------------------------------------
hexamaps <- eventReactive(input$hexamaps_go,{
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".hexamaps", args = list(
    couName = input$inCouSel, num_iter = input$inNumIter, 
    max_num_neighbors = input$maxNumNeigh, period = input$inPeriod,
    fact = input$inFactor
  ))
})

output$hexamaps <- renderPlot({
  
  input$hexamaps_go # button reactive 
  
  isolate({ # Use isolate() to avoid dependency on input values
    
    .hexamaps(input$inCouSel,input$inNumIter,input$maxNumNeigh,input$inPeriod,input$inFactor)
  })
  
}, bg = "transparent")
