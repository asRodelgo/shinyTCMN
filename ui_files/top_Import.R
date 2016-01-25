# ExpImp_HF ------------------------------------
column(12, h5("Imports Categories by % of Total Value"),
       h6("Source: ",
          a(TCMN_sources[TCMN_sources$Source=="WITS_TRADE",]$SourceDescription, 
            href = TCMN_sources[TCMN_sources$Source=="WITS_TRADE",]$url)),
       h6("Download:",downloadLink("downTopImp","Chart ")," ",downloadLink("dataTopImp","Data")),
       plotOutput('topImport', height=200))
