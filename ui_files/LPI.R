# LPI indicators ----------------------------------------
fluidRow(
  column(12,h4("Logistic Performance Indicators"),
         h6("Source: ",
            a(TCMN_sources[TCMN_sources$Source=="WDI",]$SourceDescription, 
              href = TCMN_sources[TCMN_sources$Source=="WDI",]$url)),
         column(3,selectInput('inCouSelLPI', 'Compare with:', choices=c("Select a country",countryNames$Country),selectize=FALSE)),
         column(3,sliderInput('inPeriodLPI', 'Select a time period:', 
                     min = as.numeric(min(TCMN_data[TCMN_data$Subsection=="chart8",]$Period)),
                     max = as.numeric(thisYear)-1,
                     value = as.numeric(thisYear)-2))
         #column(3, br(),img(src = "darkbluedot.png", width="20") ,h4(textOutput('outCouSel'))),
         #column(3, br(),img(src = "greendot.png", width="20") ,h4(textOutput('outCouSelLPI')))
  ),
  column(12, #h5("Net Exports/Imports (billions of current USD)"),
       #h6("Source: ",
       #    a("World Development Indicators (WDI), World Bank", 
       #    href = "http://data.worldbank.org/data-catalog/world-development-indicators")),
       plotOutput('LPI'))
)