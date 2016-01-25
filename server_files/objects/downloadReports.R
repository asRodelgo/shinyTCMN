# download PDF full-----------------------------
output$downloadReportFull <- downloadHandler(
  filename = paste0("pdf/TCMN_Full_",.getCountryCode(input$inCouSel),".pdf"),
  content = function(file) file.copy(paste0("pdf/TCMN_Full_",.getCountryCode(input$inCouSel),".pdf"), file),
  contentType = 'application/pdf'
)
# download PDF data-----------------------------
output$downloadReportData <- downloadHandler(
  filename = paste0("pdf/TCMN_",.getCountryCode(input$inCouSel),".pdf"),
  content = function(file) file.copy(paste0("pdf/TCMN_",.getCountryCode(input$inCouSel),".pdf"), file),
  contentType = 'application/pdf'
)
# download PDF operations-----------------------------
output$downloadReportOperations <- downloadHandler(
  filename = paste0("pdf/TCMN_Operations_",.getCountryCode(input$inCouSel),".pdf"),
  content = function(file) file.copy(paste0("pdf/TCMN_Operations_",.getCountryCode(input$inCouSel),".pdf"), file),
  contentType = 'application/pdf'
)
# download Operations PDF home -----------------------------
output$downloadReportFullHome <- downloadHandler(
  filename = paste0("pdf/TCMN_Full_",.getCountryCode(input$inCouSel),".pdf"),
  content = function(file) file.copy(paste0("pdf/TCMN_Full_",.getCountryCode(input$inCouSel),".pdf"), file),
  contentType = 'application/pdf'
)
# download PDF home -----------------------------
output$downloadReportHome <- downloadHandler(
  filename = paste0("pdf/TCMN_",.getCountryCode(input$inCouSel),".pdf"),
  content = function(file) file.copy(paste0("pdf/TCMN_",.getCountryCode(input$inCouSel),".pdf"), file),
  contentType = 'application/pdf'
)
# download Operations PDF home -----------------------------
output$downloadReportOperHome <- downloadHandler(
  filename = paste0("pdf/TCMN_Operations_",.getCountryCode(input$inCouSel),".pdf"),
  content = function(file) file.copy(paste0("pdf/TCMN_Operations_",.getCountryCode(input$inCouSel),".pdf"), file),
  contentType = 'application/pdf'
)
