fluidRow(
  column(3,source(file.path("ui_files", "projectsStatus.R"), local = TRUE)$value),
  column(9,  dateRangeInput('projectDateRange',
                            label = 'Select date range (yyyy-mm-dd)',
                            start = Sys.Date() - 1000, end = Sys.Date()),
         dataTableOutput('projectsTable')))