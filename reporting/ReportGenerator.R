# PDF Offline Report generator --------------------------
.reportGenerator <- function(x){
  for (c in countryNames$Country) {
    iso3 <- .getCountryCode(c)
    knit2pdf('reporting/TCMN_PDF_Local.Rnw', clean = TRUE,
             encoding = "UTF-8",
             output = paste0("TCMN_",iso3,".tex"))
    #print(paste("Report generated successfully for",c))
  }
}
