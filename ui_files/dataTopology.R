# plot TSNE UI ----------

fluidPage(
  fluidRow(
    column(9,
           fluidRow(
             column(9,h4("Filter by"),
                splitLayout(cellWidths = rep("33%", 3),
                                 selectInput('colPeriod', 'Period:', choices=c("All",sort(unique(data_tsne_sample$Period))),selectize=FALSE),
                                 selectInput('colRegion', 'Region:', choices=c("All",sort(unique(data_tsne_sample$RegionShort))),selectize=FALSE),
                                 selectInput('colCountry', 'Country:', choices=c("All",sort(unique(data_tsne_sample$CountryShort))),selectize=FALSE)
                )
             ),
             column(3,h4("Color by"),
                    #splitLayout(cellWidths = c("62%", "36%"),
                    selectInput('colIndicator', 'Indicator:', choices=c("All",sort(names(data_tsne_sample)[c(6:ncol(data_tsne_sample))])),selectize=FALSE)
                    #radioButtons('centralMeasure', 'Centrality:', c("mean","median"))
                    #)
             )
           ),
           fluidRow(
             div(
               style = "position:relative",
               plotOutput('plotTSNE', 
                          hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                          click = clickOpts("plot_click"),
                          brush = brushOpts("plot_brush", delay = 100, delayType = "debounce"),
                          dblclick = "plot_dblclick"),
               uiOutput("hover_info")
             )
                         #,
                         #div(style = "position:relative",
                         #    plotOutput('plotRadarBrushed_Def')
                         #)
           ),
           fluidRow(
             div(style = "position:relative",
               DT::dataTableOutput('tableBrushed')
             )
           )
    ),
    column(3,h4("Select variables to explore"),
           selectizeInput(
             'explore_variables', 'Select up to 10 indicators:', choices = sort(names(data_tsne_sample)[c(6:ncol(data_tsne_sample))]),
             multiple = TRUE, selected = indicator_selection_plots, options = list(maxItems = 10)
           ),
           br(),
           div(style = "position:relative",
               plotOutput('plotBarchartBrushed')
           )
    )
  )        
  #br(),
  #div(style = "position:relative",plotOutput('plotTSNEdensities',height="150px")),
  #br(),
  
)