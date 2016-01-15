# table with sparklines ----------------------------------
# Reference: http://omnipotent.net/jquery.sparkline/#s-docs
# generate output
output$tradePolicy_Table <- renderDataTable({
  
  # define types of sparkline charts
  line_string <- "type: 'line', lineColor: 'blue', fillColor: '', minSpotColor: 'red', 
                  maxSpotColor: 'green', highlightLineColor: 'red', highlightSpotColor: 'red',
                  spotRadius: '3', lineWidth: '2', spotColor: ''"
  bar_string <- "type: 'bar', barColor: 'green', zeroColor: 'white', barWidth: '10px', negBarColor: 'red', highlightColor: 'black'"
  box_string <- "type: 'box', lineColor: 'black', whiskerColor: 'black', outlierFillColor: 'black', outlierLineColor: 'black', medianColor: 'black', boxFillColor: 'orange', boxLineColor: 'black'"
  
  # here's the data
  dataSpark <- .PolicyFacilTable(input$inCouSel)
  
  # column definitions: column #20, javascript starts arrays on 0
  cd <- list(list(className = 'dt-right', targets = c(1:(ncol(dataSpark)-2))),
             list(targets = c(ncol(dataSpark)-1), render = JS("function(data, type, full){ return '<span class=spark>' + data + '</span>' }"))) 
  
  # callback functions to create sparklines
  cb = JS(paste0("function (oSettings, json) {
                 $('.spark:not(:has(canvas))').sparkline('html', { ",
                 line_string,
                 " });
                 }"))
  
  # call datatable function
  macroSpark <- datatable(dataSpark,options = list(
    columnDefs = cd,
    fnDrawCallback = cb,
    destroy = TRUE,
    dom = 't',
    pageLength = 10
  ), rownames = FALSE)
  
  # add sparkline dependencies                        
  macroSpark$dependencies <- append(macroSpark$dependencies, htmlwidgets:::getDependency("sparkline"))
  
  return(macroSpark)
}) 

