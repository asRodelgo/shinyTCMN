# ExpImp_HF ------------------------------------
column(12, h5("Goods Export and Import, volume index (2000 = 100)"),
       h6("Source: ",
          a(TCMN_sources[TCMN_sources$Source=="WDI",]$SourceDescription, 
            href = TCMN_sources[TCMN_sources$Source=="WDI",]$url)),
       h6("Download:",downloadLink("downExpImp","plot",class = "plot-download")," ",downloadLink("dataExpImp","data",class = "plot-download")),
       plotOutput('ExpImp_HF', height=200)
       #bsTooltip('ExpImp_HF', "This chart shows 3-month moving averages of monthly imports and exports of goods",
      #           "bottom", options = list(container = "body"))
      )