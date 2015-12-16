# tsne chart ----------------------------------------------------
tSNE_plot <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".tSNE_plot", args = list(
    num_iter = input$inNumIter, max_num_neighbors = input$maxNumNeigh 
  ))
})

output$tSNE_plot <- renderPlot({
  .tSNE_plot(input$inNumIter, input$maxNumNeigh)
}, bg = "transparent")
