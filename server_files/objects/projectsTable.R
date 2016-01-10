# Projects Portfolio table output -----------------------------
output$projectsTable <- DT::renderDataTable({
  projectsTable <- .projectsTable(input$inCouSel)
  return(projectsTable)
},options = list(dom = 't'))
