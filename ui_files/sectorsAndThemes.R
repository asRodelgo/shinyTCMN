fluidPage(
  fluidRow(
    column(6,h4("Projects Portfolio by Sectors, in percent"),
           h5("(Includes only T&C IBRD projects for the last 3 fiscal years)"),br(),
           plotOutput('sectorsTreemap', height=300),br(),
           tableOutput('sectorsTable')),
    column(6,h4("Projects Portfolio by Themes, in percent"),
           h5("(Includes only T&C IBRD projects for the last 3 fiscal years)"),br(),
           plotOutput('themesTreemap', height=300),br(),
           tableOutput('themesTable'))
  )
)