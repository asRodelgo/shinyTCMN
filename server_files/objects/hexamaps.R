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


# download chart
output$downHexamap <- downloadHandler(
  filename = function(){
    paste0("Hexamap_",.getCountryCode(input$inCouSel),".png")
  },
  content = function(file){
    png(file,width=900,height=900)
    print(.hexamaps(input$inCouSel,input$inNumIter,input$maxNumNeigh,input$inPeriod,input$inFactor))
    dev.off()
  }
)
# download data ----------------------------
output$dataHexamap <- downloadHandler(
  filename = function() { 
    paste0("Hexamap_",.getCountryCode(input$inCouSel),"_",input$inNumIter,"_",input$maxNumNeigh,"_",input$inPeriod,"_",input$inFactor,".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.tSNE_compute(input$inCouSel,input$inNumIter,input$maxNumNeigh,input$inPeriod), file, row.names = FALSE)
  }
)

