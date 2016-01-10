# tSNE ------------------------------------------------------

div(id = "hexamaps",
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
        numericInput('inFactor', label = "Spread factor:", value = 12, 
                      min = 1, max = 100),
        actionButton('hexamaps_go', "Generate hexamap")#, class = "tsne-go"),
      ),
      mainPanel(
        #column(10, 
        h5("Hexamaps are displayed using the distances obtained from applying tSNE on the T&C dataset"),
        h5("The example below shows this technique using all the TCMN data available (more than 140 variables displayed in 2D). It can help identify countries with similiar Trade and Competitiveness policies"),
        h6("Based on: ",
           a("Hexamaps", 
             href = "http://www.56n.dk/create-your-own-hexamaps/")),
        plotOutput('hexamaps',height=600)
      )
    )
)