# Projects Portfolio table output -----------------------------
output$projectsTable <- DT::renderDataTable({
  projectsTable <- .projectsTable(input$inCouSel, input$projectDateRange)
  return(projectsTable)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = 8)),
                 scrollX = FALSE),rownames = FALSE)

# Projects Lending Pipeline -----------------------------
output$lendingPipeline <- DT::renderDataTable({
  lendingPipeline <- .projectsTableLendingPipeline(input$inCouSel)#, input$projectDateRange)
  return(lendingPipeline)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = c(6,8,9))),
                 scrollX = TRUE),rownames = FALSE,escape=FALSE)
# Projects Portfolio Active -----------------------------
output$portfolioActive <- DT::renderDataTable({
  portfolioActive <- .projectsTablePortfolioActive(input$inCouSel)#, input$projectDateRange)
  return(portfolioActive)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = c(6,7,11))),
                 scrollX = TRUE),rownames = FALSE,escape=FALSE)
# Projects Portfolio Closed last 2 years -----------------------------
output$portfolioClosed <- DT::renderDataTable({
  portfolioClosed <- .projectsTablePortfolioClosed(input$inCouSel)#, input$projectDateRange)
  return(portfolioClosed)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = 6)),
                 scrollX = TRUE),rownames = FALSE,escape=FALSE)
# Projects Portfolio table ASA Active -----------------------------
output$asaActive <- DT::renderDataTable({
  asaActive <- .projectsTableASAActive(input$inCouSel)#, input$projectDateRange)
  return(asaActive)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = c(6,7,8,9))),
                 scrollX = FALSE),rownames = FALSE,escape=FALSE)
# Projects Portfolio table ASA Closed -----------------------------
output$asaClosed <- DT::renderDataTable({
  asaClosed <- .projectsTableASAClosed(input$inCouSel)#, input$projectDateRange)
  return(asaClosed)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = c(6,7,8,9))),
                 scrollX = FALSE),rownames = FALSE,escape=FALSE)

# Projects Portfolio table ASA IFC Active -----------------------------
output$ifcActive <- DT::renderDataTable({
  ifcActive <- .projectsTableASA_IFC(input$inCouSel, "Active")
  return(ifcActive)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = c(5,6,7))),
                 scrollX = TRUE),rownames = FALSE,escape=FALSE)
# Projects Portfolio table ASA IFC Active -----------------------------
output$ifcPipeline <- DT::renderDataTable({
  ifcPipeline <- .projectsTableASA_IFC(input$inCouSel, "Pipeline")
  return(ifcPipeline)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = c(5,6,7))),
                 scrollX = TRUE),rownames = FALSE,escape=FALSE)
# Projects Portfolio table ASA IFC Closed -----------------------------
output$ifcClosed <- DT::renderDataTable({
  ifcClosed <- .projectsTableASA_IFC(input$inCouSel, "Closed")
  return(ifcClosed)
},options = list(dom = 't', columnDefs = list(list(className = 'dt-right', targets = c(5,6,7))),
                 scrollX = TRUE),rownames = FALSE,escape=FALSE)
# SCD/CPF most recent -----------------------------
output$mostRecentDocs <- DT::renderDataTable({
  mostRecentDocs <- .mostRecentDocs(input$inCouSel)
  return(mostRecentDocs)
},options = list(dom = 't', scrollX = TRUE),rownames = FALSE,escape=FALSE)
# SCD/CPF planned -----------------------------
output$plannedDocs <- DT::renderDataTable({
  plannedDocs <- .plannedDocs(input$inCouSel)
  return(plannedDocs)
},options = list(dom = 't', scrollX = TRUE),rownames = FALSE,escape=FALSE)

# download data ---------------------
output$dataLendingPipeline <- downloadHandler(
  filename = function() { 
    paste0("Operations_LendingPipeline_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.projectsTableLendingPipeline(input$inCouSel), file, row.names = FALSE)
  }
)
output$dataPortfolioActive <- downloadHandler(
  filename = function() { 
    paste0("Operations_PortfolioActive_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.projectsTablePortfolioActive(input$inCouSel), file, row.names = FALSE)
  }
)
output$dataPortfolioClosed <- downloadHandler(
  filename = function() { 
    paste0("Operations_PortfolioClosed_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.projectsTablePortfolioClosed(input$inCouSel), file, row.names = FALSE)
  }
)
output$dataASAActive <- downloadHandler(
  filename = function() { 
    paste0("Operations_ASAActive_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.projectsTableASAActive(input$inCouSel), file, row.names = FALSE)
  }
)
output$dataASAClosed <- downloadHandler(
  filename = function() { 
    paste0("Operations_ASAClosed_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.projectsTableASAClosed(input$inCouSel), file, row.names = FALSE)
  }
)
output$dataASA_IFCActive <- downloadHandler(
  filename = function() { 
    paste0("Operations_ASA_IFCActive_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.projectsTableASA_IFC(input$inCouSel,"Active"), file, row.names = FALSE)
  }
)
output$dataASA_IFCPipeline <- downloadHandler(
  filename = function() { 
    paste0("Operations_ASA_IFCPipeline_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.projectsTableASA_IFC(input$inCouSel,"Pipeline"), file, row.names = FALSE)
  }
)
output$dataASA_IFCClosed <- downloadHandler(
  filename = function() { 
    paste0("Operations_ASA_IFCClosed_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.projectsTableASA_IFC(input$inCouSel,"Closed"), file, row.names = FALSE)
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




