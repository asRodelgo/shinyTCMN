fluidRow(
  column(3,h5("Distribution of staff by grade"),
         h6("Download:",downloadLink("downOperStaff","plot",class = "plot-download")),br(), 
         plotOutput('projectsGrades')),
  column(9, h5("T&C and IFC Staff assigned to projects in Active, Pipeline or Closed status"),
         h6("Download:",downloadLink("dataOperStaff","data",class = "plot-download")),br(),
         dataTableOutput('projectsPeople'))
  )