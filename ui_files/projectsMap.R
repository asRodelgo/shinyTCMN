# leaflet world map based on ----------------------------------------------------
# https://github.com/rstudio/shiny-examples/blob/master/063-superzip-example/ui.R
leafletOutput("projectsMap", width="100%", height="100%")
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