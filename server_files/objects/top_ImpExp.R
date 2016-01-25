# imports -----------------------------------------------
topImport_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".topImport", args = list(
    couName = input$inCouSel, type = "m"
  ))
})

output$topImport <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .ImpExp_Treemap(input$inCouSel, "m")
  #})
}, bg = "transparent")

# download chart
output$downTopImp <- downloadHandler(
  filename = function(){
    paste0("Top_Imports_",.getCountryCode(input$inCouSel),".png")
  },
  content = function(file){
    png(file)
    .ImpExp_Treemap(input$inCouSel,"m")
    dev.off()
  }
)
# download data ----------------------------
output$dataTopImp <- downloadHandler(
  filename = function() { 
    paste0("Top_Imports_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(mWits[mWits$CountryCode==.getCountryCode(input$inCouSel),], file, row.names = FALSE)
  }
)


# exports -----------------------------------------------
topExport_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".topExport", args = list(
    couName = input$inCouSel, type="x"
  ))
})

output$topExport <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .ImpExp_Treemap(input$inCouSel,"x")
  #})
}, bg = "transparent")

# download chart
output$downTopExp <- downloadHandler(
  filename = function(){
    paste0("Top_Exports_",.getCountryCode(input$inCouSel),".png")
  },
  content = function(file){
    png(file)
    .ImpExp_Treemap(input$inCouSel,"x")
    dev.off()
  }
)
# download data ----------------------------
output$dataTopExp <- downloadHandler(
  filename = function() { 
    paste0("Top_Exports_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(xWits[xWits$CountryCode==.getCountryCode(input$inCouSel),], file, row.names = FALSE)
  }
)


