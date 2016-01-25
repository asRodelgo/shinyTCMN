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
    
    tsneTable <- .tSNE_dist(input$inCouSel,input$inNumIter,input$maxNumNeigh,input$inPeriod)
    return(tsneTable)
    
  })
  
})#,options = list(dom = 't')) # disable all the table fancy options  

# download chart
output$downTSNE <- downloadHandler(
  filename = function(){
    paste0("TSNE_",.getCountryCode(input$inCouSel),".png")
  },
  content = function(file){
    png(file,width=900,height=900)
    .tSNE_plot(input$inCouSel,input$inNumIter,input$maxNumNeigh,input$inPeriod)
    dev.off()
  }
)
# download data ----------------------------
output$dataTSNE <- downloadHandler(
  filename = function() { 
    paste0("TSNE_",.getCountryCode(input$inCouSel),"_",input$inNumIter,"_",input$maxNumNeigh,"_",input$inPeriod,"_",input$inFactor,".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.tSNE_compute(input$inCouSel,input$inNumIter,input$maxNumNeigh,input$inPeriod), file, row.names = FALSE)
  }
)


