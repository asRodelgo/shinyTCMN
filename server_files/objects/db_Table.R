# table with sparklines ----------------------------------
# Reference: http://omnipotent.net/jquery.sparkline/#s-docs
# generate output
output$db_Table <- renderDataTable({
  
  # define types of sparkline charts
  line_string <- "type: 'line', lineColor: 'blue', fillColor: 'white' ,minSpotColor: 'red', maxSpotColor: 'green', highlightLineColor: 'red', highlightSpotColor: 'red'"
  bar_string <- "type: 'bar', barColor: 'green', zeroColor: 'white', barWidth: '10px', negBarColor: 'red', highlightColor: 'black'"
  box_string <- "type: 'box', lineColor: 'black', whiskerColor: 'black', outlierFillColor: 'black', outlierLineColor: 'black', medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black'"
  
  # here's the data
  dataSpark <- .PolicyTable(input$inCouSel)
  
  # column definitions: column #20, javascript starts arrays on 0
  cd <- list(list(targets = c(ncol(dataSpark)), 
                  render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }"))) 
  
  # callback functions to create sparklines
  cb = JS(paste0("function (oSettings, json) {
                 $('.spark:not(:has(canvas))').sparkline('html', { ",
                 bar_string,
                 " });
                 }"))
  #cb = JS(paste0("function (oSettings, json) {$('.spark:not(:has(canvas))').sparkline('html', { ", 
  #               line_string," })"), collapse = "")
  
  # call datatable function
  macroSpark <- datatable(dataSpark,options = list(
    columnDefs = cd,
    fnDrawCallback = cb,
    destroy = TRUE,
    dom = 't',
    pageLength = 20
  ))
  
  # add sparkline dependencies                        
  macroSpark$dependencies <- append(macroSpark$dependencies, htmlwidgets:::getDependency("sparkline"))
  
  return(macroSpark)
  #macroSpark
  }) # disable all the table fancy options  

#output$tableSpark <- datatable(data.table(dat.t2), rownames = FALSE, options = list(columnDefs = cd, fnDrawCallback = cb))

# download data ----------------------------
output$dataDB <- downloadHandler(
  filename = function() { 
    paste0("DoingBusiness_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(TCMN_data[(TCMN_data$CountryCode ==.getCountryCode(input$inCouSel)) & 
                          (TCMN_data$Subsection=="table4"),], file, row.names = FALSE)
  }
)


