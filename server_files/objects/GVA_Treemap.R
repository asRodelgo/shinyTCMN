# treemap chart ----------------------------------------------------
GVA_Treemap_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".GVA_Treemap", args = list(
    couName = input$inCouSel
  ))
})

output$GVA_Treemap <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .GVA_Treemap(input$inCouSel)
  #})
}, bg = "transparent")

# download chart
output$downGVA <- downloadHandler(
  filename = function(){
    paste0("GrossValueAdded_",.getCountryCode(input$inCouSel),".png")
  },
  content = function(file){
    png(file)
    .GVA_Treemap(input$inCouSel)
    dev.off()
  }
)
# download data ----------------------------
output$dataGVA <- downloadHandler(
  filename = function() { 
    paste0("GrossValueAdded_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(TCMN_data[(TCMN_data$CountryCode==.getCountryCode(input$inCouSel)) & 
                          (TCMN_data$Subsection=="chart2"),], file, row.names = FALSE)
  }
)
