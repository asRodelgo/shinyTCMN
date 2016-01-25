column(12,h4("Doing Business Ranks"),
h6("Source: ",
   a(TCMN_sources[TCMN_sources$Source=="DB",]$SourceDescription, 
     href = TCMN_sources[TCMN_sources$Source=="DB",]$url)),
h6("Download:",downloadLink("dataDB","Data")),
tags$style(HTML("
                 .jqstooltip{
                 box-sizing: content-box;
                 }")), # adjust tooltips in datatables
dataTableOutput('db_Table'))