# input selectors panel ------------------------------------------------------

div(id = "country_selector_home",
      fluidRow(     
        selectInput('inCouSel', NULL, countryNames$Country, selectize=FALSE),
        align="center")
)