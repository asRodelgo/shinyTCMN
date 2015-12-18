# tSNE ------------------------------------------------------

div(id = "tsne",
    fluidRow(
      column(2,
        numericInput('inNumIter', label = "Number of iterations:", value = 10, 
                     min = 10, max = 1000),
        numericInput('maxNumNeigh', label = "Max number of neighbors per cluster:", value = 10, 
                     min = 2, max = 100),
        actionButton('tsne_go', "Generate t-SNE")#, class = "tsne-go"),
      ),
      column(10, plotOutput('tSNE_plot',height=600))
    )
)