# Import table output -----------------------------
output$impTable <- DT::renderDataTable({
  impTable <- .topImportsTable(input$inCouSel)
  return(impTable)
},options = list(dom = 't')) # disable all the table fancy options  

# Export table output -----------------------------
output$expTable <- DT::renderDataTable({
  expTable <- .topExportsTable(input$inCouSel)
  return(expTable)
},options = list(dom = 't')) # disable all the table fancy options  