br()
tags$style(HTML("
                .jqstooltip{
                box-sizing: content-box;
                }")) # adjust tooltips in datatables
dataTableOutput('GVA_Table')