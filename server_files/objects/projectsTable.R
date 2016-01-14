# Projects Portfolio table output -----------------------------
output$projectsTable <- DT::renderDataTable({
  projectsTable <- .projectsTable(input$inCouSel, input$projectDateRange)
  return(projectsTable)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = 8)),
                 scrollX = FALSE),rownames = FALSE)

# Projects Portfolio table Financing output -----------------------------
output$projectsTableFinancing <- DT::renderDataTable({
  projectsTableFinancing <- .projectsTableFinancing(input$inCouSel, input$projectDateRange)
  return(projectsTableFinancing)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = 6)),
                 scrollX = FALSE),rownames = FALSE)

# Projects Portfolio table ASA output -----------------------------
output$projectsTableASA <- DT::renderDataTable({
  projectsTableASA <- .projectsTableASA(input$inCouSel, input$projectDateRange)
  return(projectsTableASA)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = 6)),
                 scrollX = FALSE),rownames = FALSE)

# Projects Portfolio table ASA IFC output -----------------------------
output$projectsTableASA_IFC <- DT::renderDataTable({
  projectsTableASA_IFC <- .projectsTableASA_IFC(input$inCouSel, input$projectDateRange)
  return(projectsTableASA_IFC)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = 4)),
                 scrollX = TRUE),rownames = FALSE)


# Projects Portfolio People table output -----------------------------
output$projectsPeople <- DT::renderDataTable({
  projectsPeople <- .projectsPeople(input$inCouSel)
  return(projectsPeople)
},options = list(dom = 't'),rownames= FALSE)

# project grades chart ----------------------------------------------------
output$projectsGrades <- renderPlot({
  
  .projectsStaffStats(input$inCouSel)
  
}, bg = "transparent")




