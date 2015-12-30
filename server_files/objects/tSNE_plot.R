# tsne chart ----------------------------------------------------
tSNE_plot <- eventReactive(input$tsne_go,{
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".tSNE_plot", args = list(
    num_iter = input$inNumIter, max_num_neighbors = input$maxNumNeigh, period = input$inPeriod 
  ))
})

output$tSNE_plot <- renderPlot({
  
  input$tsne_go # button reactive 
  
  isolate({ # Use isolate() to avoid dependency on input values
    
    .tSNE_plot(input$inNumIter,input$maxNumNeigh,input$inPeriod)
  })
  
}, bg = "transparent")
