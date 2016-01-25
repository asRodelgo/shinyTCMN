# Competitiveness Indicators  ----------------------------------------------------
Compet_Indic_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".WEFradar", args = list(
    couName = input$inCouSel, couName2 = input$inCouSel2
  ))
})

output$compet_Indic <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .WEFradar(input$inCouSel,input$inCouSel2)
  #})
}, bg = "transparent")

# download chart
output$downGCI <- downloadHandler(
  filename = function(){
    paste0("CompetitivenessIndicators_",.getCountryCode(input$inCouSel),".png")
  },
  content = function(file){
    png(file,width=700,height=700)
    .WEFradar(input$inCouSel,input$inCouSel2)
    dev.off()
  }
)
# download data ----------------------------
output$dataGCI <- downloadHandler(
  filename = function() { 
    paste0("CompetitivenessIndicators_",.getCountryCode(input$inCouSel),"_",.getCountryCode(input$inCouSel2),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(TCMN_data[(TCMN_data$CountryCode %in% c(.getCountryCode(input$inCouSel),
                                                      .getCountryCode(input$inCouSel2))) & 
                          (TCMN_data$Subsection=="chart7"),], file, row.names = FALSE)
  }
)
