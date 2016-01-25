fluidRow(
  column(3,h5("Distribution of staff by grade"),
         h6("Download:",downloadLink("downOperStaff","Chart ")),br(), 
         plotOutput('projectsGrades')),
  column(9, h5("T&C and IFC Staff assigned to projects in Active, Pipeline or Closed status that were approved after FY2013"),
         h6("Download:",downloadLink("dataOperStaff","Data")),br(),
         dataTableOutput('projectsPeople'))
  )