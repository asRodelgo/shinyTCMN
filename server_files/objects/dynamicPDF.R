# dynamic PDF report generation ----------------------------
output$downloadDynamicReport <- downloadHandler(
  filename = "TCMN_PDF.pdf",
  content = function(file) {
    out = knit2pdf('reporting/TCMN_PDF.Rnw', clean = TRUE)
    file.rename(out, file) # move pdf to file for downloading
  },
  contentType = 'application/pdf'
)