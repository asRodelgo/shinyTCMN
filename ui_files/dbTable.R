#column(12,h4("Doing Business Ranks"),
column(12,h4("Doing Business Ranks"),
       radioButtons('inDBtype', "",
                    c("Distance to Frontier" = "dbDTF","Ranks" = "dbRanks"), 
                    selected = "dbDTF", inline = TRUE),
h6("Source: ",
   a(TCMN_sources[TCMN_sources$Source=="DB",]$SourceDescription, 
     href = TCMN_sources[TCMN_sources$Source=="DB",]$url)),
h6("Download:",downloadLink("dataDB","data",class = "plot-download")),
tags$style(HTML("
                 .jqstooltip{
                 box-sizing: content-box;
                 }")), # adjust tooltips in datatables
dataTableOutput('db_Table'))