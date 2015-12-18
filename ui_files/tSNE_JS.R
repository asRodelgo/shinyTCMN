# tSNE javascript------------------------------------------------------

div(id = "tsne_js",
    #useShinyjs(),
    #extendShinyjs(script = source(file.path("js", "tsne.js"), local = TRUE)$value),
    fluidRow(  
      numericInput('inNumIter', label = "Number of Iterations:", value = 100, 
                   min = 10, max = 1000),
      numericInput('maxNumNeigh', label = "Max Number of Neighbors:", value = 10, 
                   min = 2, max = 100),
      actionButton('tsne_go', "GO", class = "tsne-go"),
      plotOutput('tSNE_plot',height=500)
    )
)