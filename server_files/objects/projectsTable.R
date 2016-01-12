# Projects Portfolio table output -----------------------------
output$projectsTable <- DT::renderDataTable({
  projectsTable <- .projectsTable(input$inCouSel)
  return(projectsTable)
},options = list(dom = 't'))

# Projects Portfolio People table output -----------------------------
output$projectsPeople <- DT::renderDataTable({
  projectsPeople <- .projectsPeople(input$inCouSel)
  return(projectsPeople)
},options = list(dom = 't'))

# project grades chart ----------------------------------------------------
output$projectsGrades <- renderPlot({
  
  .projectsStaffStats(input$inCouSel)
  
}, bg = "transparent")




