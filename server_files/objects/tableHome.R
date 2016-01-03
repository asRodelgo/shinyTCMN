# table with sparklines ----------------------------------

# generate output
output$tableHome <- renderDataTable({
  
  # define types of sparkline charts
  line_string <- "type: 'line', lineColor: 'blue', fillColor: '', minSpotColor: 'red', 
  maxSpotColor: 'green', highlightLineColor: 'red', highlightSpotColor: 'red',
  spotRadius: '3', lineWidth: '2', spotColor: ''"
  bar_string <- "type: 'bar', barColor: 'orange', negBarColor: 'purple', highlightColor: 'black'"
  box_string <- "type: 'box', lineColor: 'black', whiskerColor: 'black', outlierFillColor: 'black', outlierLineColor: 'black', medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black'"
  
  # here's the data
  dataSpark <- .macroInd(input$inCouSel)
  
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
    pageLength = 10
  ))
  
  # add sparkline dependencies                        
  macroSpark$dependencies <- append(macroSpark$dependencies, htmlwidgets:::getDependency("sparkline"))
  
  return(macroSpark)
  #macroSpark
  }) # disable all the table fancy options  

#output$tableSpark <- datatable(data.table(dat.t2), rownames = FALSE, options = list(columnDefs = cd, fnDrawCallback = cb))
