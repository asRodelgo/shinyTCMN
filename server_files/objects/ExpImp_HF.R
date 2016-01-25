# line chart ----------------------------------------------------
ExpImp_HF_input <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".ExpImp_HF", args = list(
    couName = input$inCouSel
  ))
})

output$ExpImp_HF <- renderPlot({
   #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
    .ExpImp_HF(input$inCouSel)
  #})
}, bg = "transparent")

# download chart
output$downExpImp <- downloadHandler(
  filename = function(){
    paste0("Goods_Exp_Imp_HF_",.getCountryCode(input$inCouSel),".png")
  },
  content = function(file){
    png(file,width=900)
    print(.ExpImp_HF(input$inCouSel))
    dev.off()
  }
)
# download data ----------------------------
output$dataExpImp <- downloadHandler(
  filename = function() { 
    paste0("Goods_Exp_Imp_HF_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(TCMN_data[(TCMN_data$CountryCode==.getCountryCode(input$inCouSel)) & 
                          (TCMN_data$Subsection=="chart1"),], file, row.names = FALSE)
  }
)
