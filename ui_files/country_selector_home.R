# input selectors panel ------------------------------------------------------

div(id = "country_selector_home",
      fluidRow(     
        selectInput('inCouSel', NULL, c("Spain","Tunisia","Morocco"), selectize=FALSE),
        align="center")
)