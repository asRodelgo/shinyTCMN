fluidPage(
  fluidRow(
    column(6,h4("Projects Portfolio by Sectors"),br(),
           plotOutput('sectorsTreemap', height=300)),
    column(6,h4("Projects Portfolio by Themes"),br(),
           plotOutput('themesTreemap', height=300))
  )
)