column(12,h4("Trade Policy Indicators"),
h6("Sources: ",
   a(TCMN_sources[TCMN_sources$Source=="WITS_TARIFF",]$SourceDescription, 
     href = TCMN_sources[TCMN_sources$Source=="WITS_TARIFF",]$url),"; ",
   a(TCMN_sources[TCMN_sources$Source=="WTO",]$SourceDescription, 
     href = TCMN_sources[TCMN_sources$Source=="WTO",]$url)),
h6("Download:",downloadLink("dataTradePolicy","data",class = "plot-download")),
tags$style(HTML("
                 .jqstooltip{
                 box-sizing: content-box;
                 }")), # adjust tooltips in datatables
dataTableOutput('tradePolicy_Table'))