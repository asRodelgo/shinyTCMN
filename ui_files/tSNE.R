# tSNE ------------------------------------------------------

div(id = "tsne",
    sidebarLayout(
      sidebarPanel(
        #fluidRow(
        #  column(2,
            numericInput('inNumIter', label = "Number of iterations:", value = 100, 
                         min = 10, max = 1000),
            numericInput('maxNumNeigh', label = "Max number of neighbors per cluster:", value = 10, 
                         min = 2, max = 100),
            sliderInput('inPeriod', 'Select a time period:', 
                                  min = as.numeric(min(TCMN_data$Period)),
                                  max = as.numeric(thisYear)-1,
                                  value = as.numeric(thisYear)-2),
            selectInput('inDataset',"Select a subset of the data:",c("All datasets",unique(TCMN_indic$Dataset)),selected = "DB",selectize = FALSE),
            actionButton('tsne_go', "Generate t-SNE")#, class = "tsne-go"),
          ),
        mainPanel(
          #column(10, 
            h5("t-Distributed Stochastic Neighbor Embedding (t-SNE) is a technique for dimensionality reduction that is particularly well suited for the visualization of high-dimensional datasets."),
            h5("The example below shows this technique using all the TCMN data available (more than 140 variables displayed in 2D). It can help identify countries with similiar Trade and Competitiveness policies"),
            h6("More info: ",
               a("t-SNE by Laurens van der Maaten", 
                 href = "https://lvdmaaten.github.io/tsne/")),
            h6("Download:",downloadLink("downTSNE","plot",class = "plot-download")," ",downloadLink("dataTSNE","data",class = "plot-download")),
            column(8,plotOutput('tSNE_plot',height=600)),
            column(4,dataTableOutput('tSNE_table'))
        )
    )
)