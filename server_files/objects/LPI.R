# Logistics Performance  ----------------------------------------------------
LPI_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".LPIindicators", args = list(
    couName = input$inCouSel, couName2 = input$inCouSelLPI, period = input$inPeriodLPI
  ))
})

output$LPI <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .LPIindicators(input$inCouSel,input$inCouSelLPI,input$inPeriodLPI)
  #})
}, bg = "transparent")

# download chart
output$downLPI <- downloadHandler(
  filename = function(){
    paste0("LogisticsPerformanceIndicators_",.getCountryCode(input$inCouSel),"_",.getCountryCode(input$inCouSelLPI),"_",input$inPeriodLPI,".png")
  },
  content = function(file){
    png(file,width=700,height=600)
    print(.LPIindicators(input$inCouSel,input$inCouSelLPI,input$inPeriodLPI))
    dev.off()
  }
)
# download data ----------------------------
output$dataGVA <- downloadHandler(
  filename = function() { 
    paste0("LogisticsPerformanceIndicators_",.getCountryCode(input$inCouSel),"_",.getCountryCode(input$inCouSelLPI),"_",input$inPeriodLPI,".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(TCMN_data[(TCMN_data$CountryCode %in% c(.getCountryCode(input$inCouSel),.getCountryCode(input$inCouSelLPI))) & 
                          (TCMN_data$Subsection=="chart8"),], file, row.names = FALSE)
  }
)
