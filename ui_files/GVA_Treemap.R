# GVA Treemap ------------------------------------
column(12, h4("Gross Value Added by Economic Activity (% GDP)"),
      h6("Source: ",
          a(TCMN_sources[TCMN_sources$Source=="WDI",]$SourceDescription, 
            href = TCMN_sources[TCMN_sources$Source=="WDI",]$url)),
      h6("Download:",downloadLink("downGVA","plot",class = "plot-download")," ",downloadLink("dataGVA","data",class = "plot-download")),
      plotOutput('GVA_Treemap', height=200))
