# tSNE ------------------------------------------------------

div(id = "tsne",
    fluidRow(  
      selectInput('inNumIter', "Number of Iterations:", c(100,200,300,400,500,600)),
      selectInput('maxNumNeigh', "Max Number of Neighbors:", seq(2,15,1)),
      plotOutput('tSNE_plot',height=500)
      )
)