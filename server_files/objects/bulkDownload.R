# data downloads ----------------------------
output$bulkDownload <- downloadHandler(
  filename = function() { 
    paste("TCMN_data", '.csv', sep='') 
  },
  content = function(file) {
    write.csv(TCMN_data, file)
  }
)
# -----
output$mWitsDownload <- downloadHandler(
  filename = function() { 
    paste("importsWits", '.csv', sep='') 
  },
  content = function(file) {
    write.csv(mWits, file)
  }
)
# -----
output$xWitsDownload <- downloadHandler(
  filename = function() { 
    paste("exportsWits", '.csv', sep='') 
  },
  content = function(file) {
    write.csv(xWits, file)
  }
)
# -----
output$indicatorsDownload <- downloadHandler(
  filename = function() { 
    paste("TCMN_indicators", '.csv', sep='') 
  },
  content = function(file) {
    write.csv(TCMN_indic, file)
  }
)
# -----
output$projectsTCDownload <- downloadHandler(
  filename = function() { 
    paste("TCprojects", '.csv', sep='') 
  },
  content = function(file) {
    write.csv(TCprojects, file)
  }
)
# -----
output$projectsIFCDownload <- downloadHandler(
  filename = function() { 
    paste("IFCprojects", '.csv', sep='') 
  },
  content = function(file) {
    write.csv(IFCprojects, file)
  }
)


