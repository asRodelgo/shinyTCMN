# dynamic PDF report generation for all countries ----------------------------
downloadAllReports <- eventReactive(input$downloadMultipleReports,{
    
  do.call(.reportGenerator, args = list(couName = input$inCouSel))
})

# output$generatePDF_log <- renderText({
#   
# #  input$generatePDF_go # button reactive
#   
# #  isolate({ # Use isolate() to avoid dependency on input values
#     
#     .generatePDFReports(input$inCouMult)
# #  })  
# 
# })
#   
output$downloadMultipleReports <- renderUI({
  
  input$downloadMultipleReports
  isolate({ # Use isolate() to avoid dependency on input values
  
    .reportGenerator(input$inCouSel)
  })
})

#     for (c in input$inCouMult){  
#       downloadHandler(
#         filename = function() {
#           paste0("TCMN_",c,".pdf")
#         },
#         content = function(file) {
#           out <- knit2pdf('reporting/TCMN_PDF.Rnw', clean = TRUE)
#           file.rename(out, file) # move pdf to file for downloading
#         },
#         contentType = 'application/pdf'
#       )
#     }
