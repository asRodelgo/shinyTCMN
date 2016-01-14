# One table for all -------------------------------
# fluidRow(
#   column(3,source(file.path("ui_files", "projectsStatus.R"), local = TRUE)$value),
#   column(9,  dateRangeInput('projectDateRange',
#                             label = 'Select date range (yyyy-mm-dd)',
#                             start = Sys.Date() - 1000, end = Sys.Date()),
#          dataTableOutput('projectsTable')))

# One table per product ---------------------------
fluidRow(
  column(3,source(file.path("ui_files", "projectsStatus.R"), local = TRUE)$value),
  column(9,  dateRangeInput('projectDateRange',
                            label = 'Select date range (yyyy-mm-dd)',
                            start = Sys.Date() - 1000, end = Sys.Date()),
         h4("Financing", style="color:#3399ff"),
         dataTableOutput('projectsTableFinancing'),
         br(),
         h4("Advisory Services and Analytics (ASA) IBRD",style="color:#3399ff"),
         dataTableOutput('projectsTableASA'),
         br(),
         h4("Advisory Services and Analytics (ASA) IFC", style="color:#3399ff"),
         dataTableOutput('projectsTableASA_IFC')))