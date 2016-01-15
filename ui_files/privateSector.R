# Private Sector's Views ------------------------------------
fluidRow(
  column(12,h4("Enterprise Survey Indicators"),
         h6("Source: ",
            a(TCMN_sources[TCMN_sources$Source=="ES",]$SourceDescription, 
              href = TCMN_sources[TCMN_sources$Source=="ES",]$url)),
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
         plotOutput('top5constraintsES')),
  column(6,h5("Top 5 problematic factors according to WEF"),
         h6("Source: ",
            a(TCMN_sources[TCMN_sources$Source=="WEF_PROFACT",]$SourceDescription, 
              href = TCMN_sources[TCMN_sources$Source=="WEF_PROFACT",]$url)), 
         plotOutput('top5constraintsWEF'))
)