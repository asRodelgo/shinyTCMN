# Projects portfolio Status  ----------------------------------------------------
projectsStatus <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".projectsStatus", args = list(
    couName = input$inCouSel, count_type = input$inProjStatusChart, dateRange = input$projectDateRange
  ))
})

output$projectsStatus <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .projectsStatus(input$inCouSel,input$inProjStatusChart,input$projectDateRange)
  #})
}, bg = "transparent")

# download chart
output$downOperStatusChart <- downloadHandler(
  filename = function(){
    paste0("OperStatusChart_",.getCountryCode(input$inCouSel),"_",input$inProjStatusChart,"_",input$projectDateRange,".png")
  },
  content = function(file){
    png(file,width=600)
    print(.projectsStatus(input$inCouSel,input$inProjStatusChart,input$projectDateRange))
    dev.off()
  }
)
# download data ----------------------------
output$dataOperStatusChart <- downloadHandler(
  filename = function() { 
    paste0("OperStatusChart_",.getCountryCode(input$inCouSel),"_",input$inProjStatusChart,"_",input$projectDateRange,".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.projectsStatusData(input$inCouSel,input$inProjStatusChart,input$projectDateRange), file, row.names = FALSE)
  }
)
