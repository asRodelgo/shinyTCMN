# table with sparklines ----------------------------------

# generate output
output$tableSpark <- renderDataTable({
  
  # define types of sparkline charts
  line_string <- "type: 'line', lineColor: 'blue', fillColor: '', minSpotColor: 'red', 
                  maxSpotColor: 'green', highlightLineColor: 'red', highlightSpotColor: 'red',
                  spotRadius: '3', lineWidth: '2', spotColor: ''"
  bar_string <- "type: 'bar', barColor: 'orange', negBarColor: 'purple', highlightColor: 'black'"
  box_string <- "type: 'box', lineColor: 'black', whiskerColor: 'black', outlierFillColor: 'black', outlierLineColor: 'black', medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black'"
  
  # here's the data
  dataSpark <- .macroInd_Big_Spark(input$inCouSel)
  
  # column definitions: column #20, javascript starts arrays on 0
  cd <- list(list(targets = c(ncol(dataSpark)), 
                  render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }"))) 
  
  # callback functions to create sparklines
  cb = JS(paste0("function (oSettings, json) {
                      $('.spark:not(:has(canvas))').sparkline('html', { ",
              line_string,
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

# macro table header indicators --------------------
output$tableSpark_Split_head <- renderDataTable({
  # define types of sparkline charts
  line_string <- "type: 'line', lineColor: 'blue', fillColor: '', minSpotColor: 'red', 
  maxSpotColor: 'green', highlightLineColor: 'red', highlightSpotColor: 'red',
  spotRadius: '3', lineWidth: '2', spotColor: ''"
  
  dataSpark <- .macroInd_Spark_Split(input$inCouSel, "table2head")
  # column definitions: column #20, javascript starts arrays on 0
  cd <- list(list(className = 'dt-right', targets = c(1:(ncol(dataSpark)-2))),list(targets = c(ncol(dataSpark)-1), 
                  render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }"))) 
  # callback functions to create sparklines
  cb = JS(paste0("function (oSettings, json) {
                 $('.spark:not(:has(canvas))').sparkline('html', { ",
                 line_string," });}"))
  # call datatable function
  macroSpark <- datatable(dataSpark,options = list(
    columnDefs = cd,
    fnDrawCallback = cb,
    destroy = TRUE,
    dom = 't',
    pageLength = 20
  ),rownames = FALSE)
  # add sparkline dependencies                        
  macroSpark$dependencies <- append(macroSpark$dependencies, htmlwidgets:::getDependency("sparkline"))
  
  return(macroSpark)

})

# macro table macro indicators --------------------
output$tableSpark_Split_macro <- renderDataTable({
  
  # define types of sparkline charts
  line_string <- "type: 'line', lineColor: 'blue', fillColor: '', minSpotColor: 'red', 
  maxSpotColor: 'green', highlightLineColor: 'red', highlightSpotColor: 'red',
  spotRadius: '3', lineWidth: '2', spotColor: ''"
  
  dataSpark <- .macroInd_Spark_Split(input$inCouSel, "table2macro")
  # column definitions: column #20, javascript starts arrays on 0
  cd <- list(list(className = 'dt-right', targets = c(1:(ncol(dataSpark)-2))),list(targets = c(ncol(dataSpark)-1), 
                                                                                   render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }"))) 
  # callback functions to create sparklines
  cb = JS(paste0("function (oSettings, json) {
                 $('.spark:not(:has(canvas))').sparkline('html', { ",
                 line_string," });}"))
  # call datatable function
  macroSpark <- datatable(dataSpark,options = list(
    columnDefs = cd,
    fnDrawCallback = cb,
    destroy = TRUE,
    dom = 't',
    pageLength = 20
  ),rownames = FALSE)
  # add sparkline dependencies                        
  macroSpark$dependencies <- append(macroSpark$dependencies, htmlwidgets:::getDependency("sparkline"))
  
  return(macroSpark)
  
}) 

# macro table investment indicators --------------------
output$tableSpark_Split_invest <- renderDataTable({
  # define types of sparkline charts
  line_string <- "type: 'line', lineColor: 'blue', fillColor: '', minSpotColor: 'red', 
  maxSpotColor: 'green', highlightLineColor: 'red', highlightSpotColor: 'red',
  spotRadius: '3', lineWidth: '2', spotColor: ''"
  
  dataSpark <- .macroInd_Spark_Split(input$inCouSel, "table2invest")
  # column definitions: column #20, javascript starts arrays on 0
  cd <- list(list(className = 'dt-right', targets = c(1:(ncol(dataSpark)-2))),list(targets = c(ncol(dataSpark)-1), 
                                                                                   render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }"))) 
  # callback functions to create sparklines
  cb = JS(paste0("function (oSettings, json) {
                 $('.spark:not(:has(canvas))').sparkline('html', { ",
                 line_string," });}"))
  # call datatable function
  macroSpark <- datatable(dataSpark,options = list(
    columnDefs = cd,
    fnDrawCallback = cb,
    destroy = TRUE,
    dom = 't',
    pageLength = 20
  ),rownames = FALSE)
  # add sparkline dependencies                        
  macroSpark$dependencies <- append(macroSpark$dependencies, htmlwidgets:::getDependency("sparkline"))
  
  return(macroSpark)
  
}) 

# macro table trade indicators --------------------
output$tableSpark_Split_trade <- renderDataTable({
  # define types of sparkline charts
  line_string <- "type: 'line', lineColor: 'blue', fillColor: '', minSpotColor: 'red', 
  maxSpotColor: 'green', highlightLineColor: 'red', highlightSpotColor: 'red',
  spotRadius: '3', lineWidth: '2', spotColor: ''"
  
  dataSpark <- .macroInd_Spark_Split(input$inCouSel, "table2trade")
  # column definitions: column #20, javascript starts arrays on 0
  cd <- list(list(className = 'dt-right', targets = c(1:(ncol(dataSpark)-2))),list(targets = c(ncol(dataSpark)-1), 
                                                                                   render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }"))) 
  # callback functions to create sparklines
  cb = JS(paste0("function (oSettings, json) {
                 $('.spark:not(:has(canvas))').sparkline('html', { ",
                 line_string," });}"))
  # call datatable function
  macroSpark <- datatable(dataSpark,options = list(
    columnDefs = cd,
    fnDrawCallback = cb,
    destroy = TRUE,
    dom = 't',
    pageLength = 20
  ),rownames = FALSE)
  # add sparkline dependencies                        
  macroSpark$dependencies <- append(macroSpark$dependencies, htmlwidgets:::getDependency("sparkline"))
  
  return(macroSpark)
  
}) 

# download data ----------------------------
output$dataMacro <- downloadHandler(
  filename = function() { 
    paste0("Macro_Indicators_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(TCMN_data[(TCMN_data$CountryCode==.getCountryCode(input$inCouSel)) & 
                          (substr(TCMN_data$Subsection,1,6)=="table2"),], file, row.names = FALSE)
  }
)



