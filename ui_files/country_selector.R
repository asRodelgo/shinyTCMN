# input selectors panel ------------------------------------------------------

div(id = "country_selector",
    wellPanel(
      fluidRow(     
        column(4, selectInput('inCouSel', 'Select a country', c("Spain","Tunisia","Morocco"), selectize=FALSE))
      )
#       fluidRow(
#         column(4, selectizeInput(
#           inputId = 'inCouSel', label = 'Select a country', multiple = FALSE, 
#           choices = .param_list, 
#           selected = .param_list[1]))
#       )
    )
)