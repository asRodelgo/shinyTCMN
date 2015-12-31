# Private Sector's Views ------------------------------------
fluidRow(
  column(12,
         h5("Enterprise Survey Indicators"),
         tags$style(HTML("
                         .jqstooltip{
                         box-sizing: content-box;
                         }")), # adjust tooltips in datatables
         dataTableOutput('es_Table')
  ),
  column(6, plotOutput('top5constraintsES')),
  column(6, plotOutput('top5constraintsWEF'))
)