# Projects Status chart ------------------------------------
fluidRow(
  column(12, h5("Project Portfolio Status"),
              radioButtons('inProjStatusChart',"",
                      c("By number of projects" = "count",
                        "By project amount (in $K)" = "amount"
                      ), selected = "count", inline = FALSE),
         h6("Download:",downloadLink("downOperStatusChart","plot",class = "plot-download")," ",downloadLink("dataOperStatusChart","data",class = "plot-download"))
  ),
  column(12, plotOutput('projectsStatus'))
)