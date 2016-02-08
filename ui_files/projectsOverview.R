# One table for all -------------------------------
# fluidRow(
#   column(3,source(file.path("ui_files", "projectsStatus.R"), local = TRUE)$value),
#   column(9,  dateRangeInput('projectDateRange',
#                             label = 'Select date range (yyyy-mm-dd)',
#                             start = Sys.Date() - 1000, end = Sys.Date()),
#          dataTableOutput('projectsTable')))

# One table per product ---------------------------
#sidebarLayout(
  
  #sidebarPanel(),
  
#  mainPanel(
    tabsetPanel(selected = "WB Portfolio",
  #### tables ####
      tabPanel("WB Lending Pipeline",
                h4("WB Lending Pipeline", style="color:#3399ff"),
                h6("Download:",downloadLink("dataLendingPipeline","data",class = "plot-download")
               ),
               dataTableOutput('lendingPipeline')
      ),
      tabPanel("WB Portfolio",
               h4("WB Portfolio", style="color:#3399ff"),
               h4("Active",style="color:#3399ff"),
               h6("Download:",downloadLink("dataPortfolioActive","data",class = "plot-download")),
               dataTableOutput('portfolioActive'),
               br(),
               h4("Closed (in the last 2 years)",style="color:#3399ff"),
               h6("Download:",downloadLink("dataPortfolioClosed","data",class = "plot-download")),
               dataTableOutput('portfolioClosed'),
               br()),
      tabPanel("WB ASA",
               h4("WB ASA", style="color:#3399ff"),           
               h4("Active",style="color:#3399ff"),
               h6("Download:",downloadLink("dataASAActive","data",class = "plot-download")),
               dataTableOutput('asaActive'),
               br(),
               h4("Closed",style="color:#3399ff"),
               h6("Download:",downloadLink("dataASAClosed","data",class = "plot-download")),
               dataTableOutput('asaClosed'),
               br()),
      tabPanel("IFC ASA",
               h4("IFC ASA", style="color:#3399ff"),
               h4("Active",style="color:#3399ff"),
               h6("Download:",downloadLink("dataASA_IFCActive","data",class = "plot-download")),
               dataTableOutput('ifcActive'),
               br(),
               h4("Closed",style="color:#3399ff"),
               h6("Download:",downloadLink("dataASA_IFCClosed","data",class = "plot-download")),
               dataTableOutput('ifcClosed'),
               br()
               )
    )
  
#)
#)           
#fluidRow(
  #column(3,source(file.path("ui_files", "projectsStatus.R"), local = TRUE)$value),
#   column(12,  dateRangeInput('projectDateRange',
#                             label = 'Select Approval Date Range (yyyy-mm-dd)',
#                             start = Sys.Date() - 4000, end = Sys.Date() + 1500),
#          h6("Download:",downloadLink("dataOperStatus","data",class = "plot-download")),
#          h4("WB Lending Pipeline", style="color:#3399ff"),
#          dataTableOutput('lendingPipeline'),
#          br(),
#          h4("WB Portfolio", style="color:#3399ff"),
#          h4("Active",style="color:#3399ff"),
#          dataTableOutput('portfolioActive'),
#          br(),
#          h4("Closed (in the last 2 years)",style="color:#3399ff"),
#          dataTableOutput('portfolioClosed'),
#          br(),
#          h4("WB ASA", style="color:#3399ff"),
#          h4("Active",style="color:#3399ff"),
#          dataTableOutput('asaActive'),
#          br(),
#          h4("Closed",style="color:#3399ff"),
#          dataTableOutput('asaClosed'),
#          br(),
#          h4("IFC ASA", style="color:#3399ff"),
#          h4("Active",style="color:#3399ff"),
#          dataTableOutput('ifcActive'),
#          br(),
#          h4("Closed",style="color:#3399ff"),
#          dataTableOutput('ifcClosed'),
#          br()
#   )
# )