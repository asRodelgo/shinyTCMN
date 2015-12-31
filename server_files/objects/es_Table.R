# ES table output -----------------------------
output$es_Table <- DT::renderDataTable({
  esTable <- .ESTable(input$inCouSel)
  return(esTable)
},options = list(dom = 't')) # disable all the table fancy options  
