# Macro tables -----------------------------------------------------
fluidPage(
  column(12, h4("Macroeconomic Indicators"),
            h5("Data for the current and the following year correspond to projections. Here is the list of data sources:"),
            h6(
              a(TCMN_sources[TCMN_sources$Source=="MFM",]$SourceDescription, 
              href = TCMN_sources[TCMN_sources$Source=="MFM",]$url),"; ",
              a(TCMN_sources[TCMN_sources$Source=="WDI",]$SourceDescription, 
              href = TCMN_sources[TCMN_sources$Source=="WDI",]$url),"; ",
              a(TCMN_sources[TCMN_sources$Source=="UNCTADSTAT",]$SourceDescription, 
              href = TCMN_sources[TCMN_sources$Source=="UNCTADSTAT",]$url),"; ",
              a(TCMN_sources[TCMN_sources$Source=="WEO",]$SourceDescription, 
              href = TCMN_sources[TCMN_sources$Source=="WEO",]$url)),
         h6("Download: ",downloadLink("dataMacro","data",class = "plot-download")),
            tags$style(HTML("
                .jqstooltip{
                box-sizing: content-box;
                }")), # adjust tooltips in datatables
         h4("Overview indicators", style="color:#3399ff"),   
         dataTableOutput('tableSpark_Split_head'),br(),
         h4("Macro specific", style="color:#3399ff"),   
         dataTableOutput('tableSpark_Split_macro'),br(),
         h4("Investment", style="color:#3399ff"),   
         dataTableOutput('tableSpark_Split_invest'),br(),
         h4("Trade", style="color:#3399ff"),   
         dataTableOutput('tableSpark_Split_trade')
  )
)