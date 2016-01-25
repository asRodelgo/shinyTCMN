# Projects Status chart ------------------------------------
fluidRow(
  column(12, h5("Project Portfolio Status"),
              radioButtons('inProjStatusChart',"",
                      c("By number of projects" = "count",
                        "By project amount (in $K)" = "amount"
                      ), selected = "count", inline = FALSE),
         h6("Download:",downloadLink("downOperStatusChart","Chart ")," ",downloadLink("dataOperStatusChart","Data"))
  ),
  column(12, plotOutput('projectsStatus'))
)