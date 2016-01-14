# ExpImp_HF ------------------------------------
column(12, h5("Goods Export and Import, volume growth"),
       h6("Source: ",
          a(TCMN_sources[TCMN_sources$Source=="DECPG",]$SourceDescription, 
            href = TCMN_sources[TCMN_sources$Source=="DECPG",]$url)),
       plotOutput('ExpImp_HF', height=200),
       bsTooltip('ExpImp_HF', "This chart shows 3-month moving averages of monthly imports and exports of goods",
                 "bottom", options = list(container = "body")))