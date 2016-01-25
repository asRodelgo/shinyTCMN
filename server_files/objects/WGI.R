# Logistics Performance  ----------------------------------------------------
WGI_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".WGIindicators", args = list(
    couName = input$inCouSel, neighbor = input$inNeighbor
  ))
})

output$WGI <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .WGIindicators(input$inCouSel,input$inNeighbor)
  #})
}, bg = "transparent")

# download chart
output$downWGI <- downloadHandler(
  filename = function(){
    paste0("World_Governance_Indicators_",.getCountryCode(input$inCouSel),"_",input$inNeighbor,".png")
  },
  content = function(file){
    png(file,width=800,height=600)
    print(.WGIindicators(input$inCouSel,input$inNeighbor))
    dev.off()
  }
)
# download data ----------------------------
output$dataWGI <- downloadHandler(
  filename = function() { 
    paste0("World_Governance_Indicators_",.getCountryCode(input$inCouSel),"_",input$inNeighbor,".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(TCMN_data[(TCMN_data$CountryCode==.getCountryCode(input$inCouSel)) & 
                          (TCMN_data$Subsection=="chart6"),], file, row.names = FALSE)
  }
)
