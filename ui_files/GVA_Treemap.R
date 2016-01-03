# GVA Treemap ------------------------------------
column(12, h5("Gross Value Added by Economic Activity (% GDP)"),
       h6("Source: ",
          a(TCMN_sources[TCMN_sources$Source=="WDI",]$SourceDescription, 
            href = TCMN_sources[TCMN_sources$Source=="WDI",]$url)),
plotOutput('GVA_Treemap', height=200))
