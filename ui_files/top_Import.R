# ExpImp_HF ------------------------------------
column(12, h5("Top Imports by category"),
       h6("Source: ",
          a("World Development Indicators (WDI), World Bank", 
            href = "http://data.worldbank.org/data-catalog/world-development-indicators")),
       plotOutput('topImport', height=200))
