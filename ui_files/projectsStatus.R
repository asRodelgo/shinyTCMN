# Projects Status chart ------------------------------------
fluidRow(
  column(12, h5("Project Portfolio Status"),
              radioButtons('inProjStatusChart',"",
                      c("By number of projects" = "count",
                        "By project amount (in $K)" = "amount"
                      ), selected = "count", inline = FALSE)
  ),
  column(12, plotOutput('projectsStatus'))
)