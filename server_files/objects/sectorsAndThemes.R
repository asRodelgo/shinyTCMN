# sectors treemap ----------------------------------------------------
sectorsTreemap <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".projectsTreemap", args = list(
    couName = input$inCouSel, sectTheme = "sector"
  ))
})

output$sectorsTreemap <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .projectsTreemap(input$inCouSel,"sector")
  #})
}, bg = "transparent")

# themes treemap ----------------------------------------------------
themesTreemap <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".projectsTreemap", args = list(
    couName = input$inCouSel, sectTheme = "theme"
  ))
})

output$themesTreemap <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .projectsTreemap(input$inCouSel,"theme")
  #})
}, bg = "transparent")

# sectors table ----------------------------
output$sectorsTable <- DT::renderDataTable({
  sectorsTable <- .sectThemesTable(input$inCouSel,"sector")
  return(sectorsTable)
},options = list(dom = 't')) # disable all the table fancy options 

# themes table ----------------------------
output$themesTable <- DT::renderDataTable({
  themesTable <- .sectThemesTable(input$inCouSel,"theme")
  return(themesTable)
},options = list(dom = 't')) # disable all the table fancy options 

