# plot TSNE UI ----------

fluidPage(
  fluidRow(
    column(9,
           fluidRow(
             column(9,h4("Filter by"),
                splitLayout(cellWidths = rep("33%", 3),
                                 selectizeInput('colPeriod', 'Period:', choices=c("All",sort(unique(data_tsne_sample$Period))),
                                                selected=NULL,multiple=TRUE,options = list(maxItems = 2,dropdownParent = 'body')),
                                 selectizeInput('colRegion', 'Region:', choices=c("All",sort(unique(data_tsne_sample$RegionShort))),
                                                selected=NULL,multiple=TRUE,options = list(maxItems = 2,dropdownParent = 'body')),
                                 selectizeInput('colCountry', 'Country:', choices=c("All",sort(unique(data_tsne_sample$CountryShort))),
                                               selected=NULL,multiple=TRUE,options = list(maxItems = 2,dropdownParent = 'body'))
                )
             ),
             column(3,h4("Color by"),
                    #splitLayout(cellWidths = c("62%", "36%"),
                    selectizeInput('colIndicator', 'Indicator:', choices=c("All",sort(names(data_tsne_sample)[c(6:ncol(data_tsne_sample))])),
                                   options = list(dropdownParent = 'body'))
                    #radioButtons('centralMeasure', 'Centrality:', c("mean","median"))
                    #)
             )
           ),
           fluidRow(
             #div(
               #style = "position:relative",
             actionLink('help_click',"Help"),
             #shinyBS::bsModal('popupHelp', "How does it work?", 'help_click', size = "large",p("This is a test window")),
             bsTooltip('help_click', "This message is so cool","bottom", "click",options = list(container = "body")),
             plotOutput('plotTSNE', height = "600px",
                        hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                        click = clickOpts("plot_click"),
                        brush = brushOpts("plot_brush", delay = 100, delayType = "debounce"),
                        dblclick = "plot_dblclick"),
             uiOutput("hover_info")
             #)
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
           plotOutput('plotBoxplotBrushed')
           #radioButtons('pickChart', 'Select chart:', c("box plot","bar chart"),
          #              selected = "box plot", inline = TRUE),
           #conditionalPanel(
          #   condition = "input.pickChart == 'box plot'", plotOutput('plotBoxplotBrushed')),
          # conditionalPanel(
          #   condition = "input.pickChart == 'bar chart'", plotOutput('plotBarchartBrushed'))
#            ),
#            div(style = "position:relative",
#                #plotOutput('plotBarchartBrushed')
#                plotOutput('plotBoxplotBrushed')
#            )
    )
  )        
  #br(),
  #div(style = "position:relative",plotOutput('plotTSNEdensities',height="150px")),
  #br(),
  
)