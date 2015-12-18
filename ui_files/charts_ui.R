# ExpImp_HF ------------------------------------
column(6, h5("Net Exports/Imports (billions of current USD)"),
       h6("Source: ",
       a("World Development Indicators (WDI), World Bank", 
       href = "http://data.worldbank.org/data-catalog/world-development-indicators")),
       plotOutput('ExpImp_HF', height=200))
