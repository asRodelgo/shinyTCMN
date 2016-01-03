fluidRow(
  column(12,h5("World Governance indicators (Std. score, High=best)"),
         h6("Source: ",
            a(TCMN_sources[TCMN_sources$Source=="WDI",]$SourceDescription, 
              href = TCMN_sources[TCMN_sources$Source=="WDI",]$url)),
         radioButtons('inNeighbor', "Select Country Group to compare:",
                        c("Top performers in region" = "topRegion",
                          "Highest income countries" = "topIncome"
                          ), selected = "topRegion", inline = TRUE)
  ),
  column(12, plotOutput('WGI'))
)