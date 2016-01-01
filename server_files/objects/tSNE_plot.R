# tsne chart ----------------------------------------------------
tSNE_plot <- eventReactive(input$tsne_go,{
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".tSNE_plot", args = list(
    couName = input$inCouSel, num_iter = input$inNumIter, max_num_neighbors = input$maxNumNeigh, period = input$inPeriod 
  ))
})

output$tSNE_plot <- renderPlot({
  
  input$tsne_go # button reactive 
  
  isolate({ # Use isolate() to avoid dependency on input values
    
    .tSNE_plot(input$inCouSel,input$inNumIter,input$maxNumNeigh,input$inPeriod)
  })
  
}, bg = "transparent")

output$tSNE_table <- renderDataTable({
  
  input$tsne_go # button reactive 
  
  isolate({ # Use isolate() to avoid dependency on input values
    
    tsneTable <- .tSNE_plot(input$inCouSel,input$inNumIter,input$maxNumNeigh,input$inPeriod)
    return(tsneTable)
    
  })
  
})#,options = list(dom = 't')) # disable all the table fancy options  



