fluidRow(
  column(3,radioButtons('inNeighbor', "Select Country Group to compare:",
                        c("Top performers in region" = "topRegion",
                          "Highest income countries" = "topIncome"
                          ), selected = "topRegion")
  ),
  column(12, plotOutput('WGI'))
)