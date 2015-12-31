# tSNE ------------------------------------------------------

div(id = "tsne",
    sidebarLayout(
      sidebarPanel(
        #fluidRow(
        #  column(2,
            numericInput('inNumIter', label = "Number of iterations:", value = 10, 
                         min = 10, max = 1000),
            numericInput('maxNumNeigh', label = "Max number of neighbors per cluster:", value = 10, 
                         min = 2, max = 100),
            sliderInput('inPeriod', 'Select a time period:', 
                                  min = as.numeric(min(TCMN_data$Period)),
                                  max = as.numeric(thisYear),
                                  value = as.numeric(thisYear)),
            actionButton('tsne_go', "Generate t-SNE")#, class = "tsne-go"),
          ),
        mainPanel(
          #column(10, 
                 plotOutput('tSNE_plot',height=600),
                 dataTableOutput('tSNE_table')
        )
    )
)