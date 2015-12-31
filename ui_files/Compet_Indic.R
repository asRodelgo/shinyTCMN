# Compet_Indic ------------------------------------
# column(12, h5("Net Exports/Imports (billions of current USD)"),
#        h6("Source: ",
#           a("World Development Indicators (WDI), World Bank", 
#             href = "http://data.worldbank.org/data-catalog/world-development-indicators")),
#        plotOutput('compet_Indic'))
fluidRow(
  column(12,
    column(3,selectInput('inCouSel2', 'Compare with:', choices=c("Select a country",countryNames$Country),selectize=FALSE)),
    column(3, br(), img(src = "darkbluedot.png", width="20") ,h4(textOutput('outCouSel'))),
    column(3, br(),img(src = "reddot.png", width="20") ,h4(textOutput('outRegSel'))),
    column(3, br(), img(src = "greendot.png", width="20") ,h4(textOutput('outCouSel2')))
  ),
  column(12, plotOutput('compet_Indic'))
)