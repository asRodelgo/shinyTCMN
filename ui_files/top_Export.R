# ExpImp_HF ------------------------------------
column(12, h5("Top 5 Exports by % of Total Value"),
       h6("Source: ",
          a(TCMN_sources[TCMN_sources$Source=="WITS_TRADE",]$SourceDescription, 
            href = TCMN_sources[TCMN_sources$Source=="WITS_TRADE",]$url)),
       plotOutput('topExport', height=200))
