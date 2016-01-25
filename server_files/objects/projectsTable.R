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

# download data ---------------------
output$dataOperStatus <- downloadHandler(
  filename = function() { 
    paste0("Operations_Overview_",.getCountryCode(input$inCouSel),"_",input$projectDateRange,".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.projectsTable(input$inCouSel, input$projectDateRange), file, row.names = FALSE)
  }
)


# Projects Portfolio People table output -----------------------------
output$projectsPeople <- DT::renderDataTable({
  projectsPeople <- .projectsPeople(input$inCouSel)
  return(projectsPeople)
},options = list(dom = 't'),rownames= FALSE)

# download data ---------------------
output$dataOperStaff <- downloadHandler(
  filename = function() { 
    paste0("Operations_Staff_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.projectsPeople(input$inCouSel), file, row.names = FALSE)
  }
)

# project grades chart ----------------------------------------------------
output$projectsGrades <- renderPlot({
  
  .projectsStaffStats(input$inCouSel)
  
}, bg = "transparent")

# download chart
output$downOperStaff <- downloadHandler(
  filename = function(){
    paste0("Operations_Staff_",.getCountryCode(input$inCouSel),".png")
  },
  content = function(file){
    png(file)
    print(.projectsStaffStats(input$inCouSel))
    dev.off()
  }
)




