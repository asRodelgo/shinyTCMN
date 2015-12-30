# ExpImp_HF ------------------------------------
column(12, h5("Top Exports by product"),
       h6("Source: ",
          a("World Development Indicators (WDI), World Bank", 
            href = "http://data.worldbank.org/data-catalog/world-development-indicators")),
       plotOutput('topExport', height=200))
