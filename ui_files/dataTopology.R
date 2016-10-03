# plot TSNE UI ----------

fluidPage(
  fluidRow(splitLayout(cellWidths = rep("22%", 4),
                       selectInput('colPeriod', 'Period:', choices=c("All",sort(unique(data_tsne_sample$Period))),selectize=FALSE),
                       selectInput('colRegion', 'Region:', choices=c("All",sort(unique(data_tsne_sample$RegionShort))),selectize=FALSE),
                       selectInput('colCountry', 'Country:', choices=c("All",sort(unique(data_tsne_sample$CountryShort))),selectize=FALSE),
                       selectInput('colIndicator', 'Indicator:', choices=c("All",sort(names(data_tsne_sample)[c(6:ncol(data_tsne_sample))])),selectize=FALSE)
  )
  ),        
  br(),
  div(style = "position:relative",plotOutput('plotTSNEdensities',height="150px")),
  br(),
  fluidRow(
    splitLayout(cellWidths = c("80%","20%"),
                div(
                  style = "position:relative",
                  plotOutput('plotTSNE', 
                             hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                             click = clickOpts("plot_click"),
                             brush = brushOpts("plot_brush", delay = 100, delayType = "debounce")),
                  uiOutput("hover_info")
                ),
                div(style = "position:relative",
                    plotOutput('plotRadarBrushed')
                )#,
                #div(style = "position:relative",
                #    plotOutput('plotRadarBrushed_Def')
                #)
    )
  ),
  div(style = "position:relative",
      DT::dataTableOutput('tableBrushed')
  )
  
)