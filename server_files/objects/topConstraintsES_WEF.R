# ES constraints -----------------------------------------------
esConstraints <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".top5constraintsES", args = list(
    couName = input$inCouSel
  ))
})

output$top5constraintsES <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .top5constraintsES(input$inCouSel)
  #})
}, bg = "transparent",height = 200)

# WEF constraints -----------------------------------------------
wefConstraints <- reactive({
  #validate(need(input$inCouSel, message = "Loading..."))
  
  do.call(".top5constraintsWEF", args = list(
    couName = input$inCouSel
  ))
})

output$top5constraintsWEF <- renderPlot({
  #input$country_go # button reactive 
  #isolate({ # Use isolate() to avoid dependency on input values
  .top5constraintsWEF(input$inCouSel)
  #})
}, bg = "transparent",height = 200)
