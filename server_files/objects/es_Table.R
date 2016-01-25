# ES table output -----------------------------
output$es_Table <- DT::renderDataTable({
  esTable <- .ESTable(input$inCouSel)
  return(esTable)
},options = list(dom = 't')) # disable all the table fancy options  

# download data ----------------------------
output$dataES <- downloadHandler(
  filename = function() { 
    paste0("EnterpriseSurvey_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(TCMN_data[(TCMN_data$CountryCode==.getCountryCode(input$inCouSel)) & 
                          (TCMN_data$Subsection=="table3"),], file, row.names = FALSE)
  }
)
