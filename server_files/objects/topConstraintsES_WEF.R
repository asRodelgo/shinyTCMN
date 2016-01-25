# ES constraints -----------------------------------------------
esConstraints <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".top5constraintsES", args = list(
    couName = input$inCouSel
  ))
})

output$top5constraintsES <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .top5constraintsES(input$inCouSel)
  #})
}, bg = "transparent",height = 200)

# download chart
output$downTop5ES <- downloadHandler(
  filename = function(){
    paste0("Top5constraints_ES_",.getCountryCode(input$inCouSel),".png")
  },
  content = function(file){
    png(file,width=600)
    print(.top5constraintsES(input$inCouSel))
    dev.off()
  }
)
# download data ----------------------------
output$dataTop5ES <- downloadHandler(
  filename = function() { 
    paste0("Top5constraints_ES_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(TCMN_data[(TCMN_data$CountryCode==.getCountryCode(input$inCouSel)) & 
                          (TCMN_data$Subsection=="chart3"),], file, row.names = FALSE)
  }
)


# -------------------------------------
# WEF constraints -----------------------------------------------
wefConstraints <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".top5constraintsWEF", args = list(
    couName = input$inCouSel
  ))
})

output$top5constraintsWEF <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .top5constraintsWEF(input$inCouSel)
  #})
}, bg = "transparent",height = 200)

# download chart
output$downTop5WEF <- downloadHandler(
  filename = function(){
    paste0("Top5constraints_WEF_",.getCountryCode(input$inCouSel),".png")
  },
  content = function(file){
    png(file,width=600)
    print(.top5constraintsWEF(input$inCouSel))
    dev.off()
  }
)
# download data ----------------------------
output$dataTop5WEF <- downloadHandler(
  filename = function() { 
    paste0("Top5constraints_WEF_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(TCMN_data[(TCMN_data$CountryCode==.getCountryCode(input$inCouSel)) & 
                          (TCMN_data$Subsection=="chart4"),], file, row.names = FALSE)
  }
)

