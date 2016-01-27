# Private Sector's Views ------------------------------------
fluidRow(
  column(12,h4("Enterprise Survey Indicators"),
         h6("Source: ",
            a(TCMN_sources[TCMN_sources$Source=="ES",]$SourceDescription, 
              href = TCMN_sources[TCMN_sources$Source=="ES",]$url)),
         h6("Download: ",downloadLink("dataES","data",class = "plot-download")),
         tags$style(HTML("
                         .jqstooltip{
                         box-sizing: content-box;
                         }")), # adjust tooltips in datatables
         dataTableOutput('es_Table'),
         br()
  ),
  column(6, h5("Top 5 constraints according to ES"),
         h6("Source: ",
            a(TCMN_sources[TCMN_sources$Source=="ES",]$SourceDescription, 
              href = TCMN_sources[TCMN_sources$Source=="ES",]$url)),
         h6("Download:",downloadLink("downTop5ES","plot",class = "plot-download")," ",downloadLink("dataTop5ES","data",class = "plot-download")),
         plotOutput('top5constraintsES')),
  column(6,h5("Top 5 problematic factors according to WEF"),
         h6("Source: ",
            a(TCMN_sources[TCMN_sources$Source=="WEF_PROFACT",]$SourceDescription, 
              href = TCMN_sources[TCMN_sources$Source=="WEF_PROFACT",]$url)), 
         h6("Download:",downloadLink("downTop5WEF","plot",class = "plot-download")," ",downloadLink("dataTop5WEF","data",class = "plot-download")),
         plotOutput('top5constraintsWEF'))
)