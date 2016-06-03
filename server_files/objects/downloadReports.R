# download PDF full-----------------------------
output$downloadReportFull <- downloadHandler(
  filename = paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_Full_",.getCountryCode(input$inCouSel),".pdf"),
  content = function(file) file.copy(paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_Full_",.getCountryCode(input$inCouSel),".pdf"), file),
  contentType = 'application/pdf'
)
# download PDF data-----------------------------
output$downloadReportData <- downloadHandler(
  filename = paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_",.getCountryCode(input$inCouSel),".pdf"),
  content = function(file) file.copy(paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_",.getCountryCode(input$inCouSel),".pdf"), file),
  contentType = 'application/pdf'
)
# download PDF operations-----------------------------
output$downloadReportOperations <- downloadHandler(
  filename = paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_Operations_",.getCountryCode(input$inCouSel),".pdf"),
  content = function(file) file.copy(paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_Operations_",.getCountryCode(input$inCouSel),".pdf"), file),
  contentType = 'application/pdf'
)
# download Operations PDF home -----------------------------
output$downloadReportFullHome <- downloadHandler(
  filename = paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_Full_",.getCountryCode(input$inCouSel),".pdf"),
  content = function(file) file.copy(paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_Full_",.getCountryCode(input$inCouSel),".pdf"), file),
  contentType = 'application/pdf'
)
# download PDF home -----------------------------
output$downloadReportHome <- downloadHandler(
  filename = paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_",.getCountryCode(input$inCouSel),".pdf"),
  content = function(file) file.copy(paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_",.getCountryCode(input$inCouSel),".pdf"), file),
  contentType = 'application/pdf'
)
# download Operations PDF home -----------------------------
output$downloadReportOperHome <- downloadHandler(
  filename = paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_Operations_",.getCountryCode(input$inCouSel),".pdf"),
  content = function(file) file.copy(paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_Operations_",.getCountryCode(input$inCouSel),".pdf"), file),
  contentType = 'application/pdf'
)
# download PDF Reports in a .zip file for Country Departments -----------------------------
# output$downloadRepsDeps <- downloadHandler(
#   filename = paste0("pdf/TCMN_CountryDepartment_",input$inCouDepHome,".zip"),
#   content = function(file) {
#     fs <- c()
#     #tmpdir <- tempdir()
#     #setwd(tempdir())
#     for (cou in (filter(countryDeps, CMU == input$inCouDepHome)$CountryCodeISO3)){
#       path <- paste0("pdf/TCMN_Operations_",cou,".pdf")
#       fs <- c(fs,path)
#     }
#     zip(zipfile=file, files=fs)
#   },
#   contentType = 'application/zip'
# )

# download PDF Country Department Reports -----------------------------
output$downloadRepsDeps <- downloadHandler(
  filename = paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_RegionDeps_",input$inCouDepHome,".pdf"),
  content = function(file) file.copy(paste0("/srv/shiny-server/shinyTCMN-data/pdf/TCMN_RegionDeps_",input$inCouDepHome,".pdf"), file),
  contentType = 'application/pdf'
)

