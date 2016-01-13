# Projects Portfolio table output -----------------------------
output$projectsTable <- DT::renderDataTable({
  projectsTable <- .projectsTable(input$inCouSel, input$projectDateRange)
  return(projectsTable)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = 8)),scrollX = TRUE))

# Projects Portfolio People table output -----------------------------
output$projectsPeople <- DT::renderDataTable({
  projectsPeople <- .projectsPeople(input$inCouSel)
  return(projectsPeople)
},options = list(dom = 't'),rownames= FALSE)

# project grades chart ----------------------------------------------------
output$projectsGrades <- renderPlot({
  
  .projectsStaffStats(input$inCouSel)
  
}, bg = "transparent")




