# # leaflet world map based on ----------------------------------------------------
# # https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/ui.R
sidebarLayout(
  
  sidebarPanel(
    
    selectInput('mapStatus',"Project Status:",c("Active","Closed","Pipeline"),selected="Active"),
    dateRangeInput('mapDateRange',
                   label = 'Approval Date Range (yyyy-mm-dd)',
                   start = Sys.Date() - 4000, end = Sys.Date() + 1500),
    selectInput('mapProdLine',"Product Line:",c("Lending","Advisory Services and Analytics (ASA) IBRD","Advisory Services and Analytics (ASA) IFC"),
                selected="Advisory Services and Analytics (ASA) IFC")
  ),
  
  mainPanel(
  
    leafletOutput("projectsMap",height=500)
  )
)
# absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
#               draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
#               width = 330, height = "auto",
#               
#               h2("ZIP explorer"),
#               
#               selectInput("color", "Color", vars),
#               selectInput("size", "Size", vars, selected = "adultpop"),
#               conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
#                                # Only prompt for threshold when coloring or sizing by superzip
#                                numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
#               ),
#               
#               plotOutput("histCentile", height = 200),
#               plotOutput("scatterCollegeIncome", height = 250)
# )