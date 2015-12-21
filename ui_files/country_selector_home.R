# input selectors panel ------------------------------------------------------

div(id = "country_selector_home",
    div(id = "shinytcmn-title", "Select a country and start exploring", align="left"),  
    div(id = "country_selector",     
        selectInput('inCouSel', NULL, countryNames$Country, selectize=FALSE),
        #actionButton('country_go', "GO"),
        align="center"
      )
)