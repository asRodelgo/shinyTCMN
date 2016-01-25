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

# download chart
output$downOperSec <- downloadHandler(
  filename = function(){
    paste0("Operations_Sectors_",.getCountryCode(input$inCouSel),".png")
  },
  content = function(file){
    png(file)
    .projectsTreemap(input$inCouSel,"sector")
    dev.off()
  }
)
# download data ----------------------------
output$dataOperSec <- downloadHandler(
  filename = function() { 
    paste0("Operations_Sectors_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.projectsSectors(input$inCouSel), file, row.names = FALSE)
  }
)

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

# download chart
output$downOperThem <- downloadHandler(
  filename = function(){
    paste0("Operations_Themes_",.getCountryCode(input$inCouSel),".png")
  },
  content = function(file){
    png(file)
    .projectsTreemap(input$inCouSel,"theme")
    dev.off()
  }
)
# download data ----------------------------
output$dataOperThem <- downloadHandler(
  filename = function() { 
    paste0("Operations_Themes_",.getCountryCode(input$inCouSel),".csv")
  },
  content = function(file) {
    #write.csv(.GVA_Table(input$inCouSel), file)
    write.csv(.projectsThemes(input$inCouSel), file, row.names = FALSE)
  }
)

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

