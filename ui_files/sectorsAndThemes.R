fluidPage(
  fluidRow(
    column(6,h4("Projects Portfolio by Sectors, in percent"),
           h5("(Includes only T&C IBRD projects for the last 3 fiscal years)"),
           h6("Download:",downloadLink("downOperSec","Chart ")," ",downloadLink("dataOperSec","Data")),
           br(),
           plotOutput('sectorsTreemap', height=300),br(),
           dataTableOutput('sectorsTable')),
    column(6,h4("Projects Portfolio by Themes, in percent"),
           h5("(Includes only T&C IBRD projects for the last 3 fiscal years)"),
           h6("Download:",downloadLink("downOperThem","Chart ")," ",downloadLink("dataOperThem","Data")),
           br(),
           plotOutput('themesTreemap', height=300),br(),
           dataTableOutput('themesTable'))
  )
)