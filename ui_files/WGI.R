fluidRow(
  column(12,h4("World Governance indicators (Std. score, High=best)"),
         h6("Source: ",
            a(TCMN_sources[TCMN_sources$Source=="WDI",]$SourceDescription, 
              href = TCMN_sources[TCMN_sources$Source=="WDI",]$url)),
         h6("Download:",downloadLink("downWGI","Chart ")," ",downloadLink("dataWGI","Data")),
         radioButtons('inNeighbor', "Select Country Group to compare:",
                        c("Top performers in region" = "topRegion",
                          "Top performers in income region" = "topIncome"
                          ), selected = "topIncome", inline = TRUE)
  ),
  column(12, plotOutput('WGI'))
)