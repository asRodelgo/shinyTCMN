# options(shiny.trace=TRUE)

#object <- get(".shinystan_temp_object", envir = shinystan:::.sso_env)
source("global_utils.R", local = TRUE) #global functions available for the whole session
#source(file.path("server_files","utilities","extract_sso.R"), local = TRUE) # stan objects

# BEGIN server ------------------------------------------------------
# ___________________________________________________________________
function(input, output, session) {
  
  # Source all files from server_files directory and subdirectories
  files <- list.files("server_files", full.names = TRUE, recursive = TRUE)
  for (f in files) source(f, local = TRUE)
  
  # source PDF tables to make them dynamic on the selected country
  #source(file.path("reporting","TCMN_charts_PDF.R"))
  
  # Home page table of contents entries
  toc_entries <- c("Macro", "Private", "Policy", "TradePolicy")
  
  # Names of inputId triggers  
  inputs <- c("inCouSel", "inPeriod")
  
  observe({
    # Link to pages from home page table of contents
    local({
      lapply(toc_entries, function(x) {
        id <- paste0("toc_", tolower(x))
        shinyjs::onclick(id, updateTabsetPanel(session, "nav", selected = x))
      })
    })
  })
  # hide/show elements between home and main navigation
  observe({
    if(!(input$inCouSelHome=="Select a country")){
      shinyjs::show(id="dataTab")
      shinyjs::hide(id="homeTab")
      shinyjs::show(id="homeButtons")
      # pass the selected country in the home page to the next screen
      updateSelectInput(session, "inCouSel",
                        choices=countryNames$Country, 
                        selected=input$inCouSelHome)
    }
  })  
  
  # output Country name --------------------------------
  output$outCouSel <- renderText(input$inCouSel) 
  output$outCouSel2 <- renderText(input$inCouSel2)
  #output$outCouSelLPI <- renderText(input$inCouSelLPI)
  output$outRegSel <- renderText({.getRegion(input$inCouSel)})
  
  output$hideHomePanel <- reactive({input$inCouSel}) # condition to hide home panel
  outputOptions(output, "hideHomePanel", suspendWhenHidden=TRUE) # add this line to make it work
 
  # Home button --------------
  observeEvent(input$country_go, {
   
      shinyjs::onclick("country_go", updateTabsetPanel(session, "nav", selected = "macro"))
  })
  
  # output global Compet Indic values ---------
  output$outCouGCI <- renderText(paste0(" (Rank: ",round(filter(TCMN_data, CountryCode == .getCountryCode(input$inCouSel), Key=="P00b")$Observation,1),")"))
  output$outCou2GCI <- renderText(paste0(" (Rank: ",round(filter(TCMN_data, CountryCode == .getCountryCode(input$inCouSel2), Key=="P00b")$Observation,1),")"))
  output$outRegGCI <- renderText(paste0(" (Avg Rank: ",round(filter(TCMN_data, CountryCode == .getRegionCode(input$inCouSel), Key=="P00b")$Observation,1),")"))
  
       # test ------
#   randomVals <- eventReactive(input$tsne_go, {
#     runif(input$inNumIter)
#   })
  # --------
  # tSNE plot
#   plot_tsne <- eventReactive(input$tsne_go, {
#     runif(input$inNumIter,input$maxNumNeigh)
#   })
#   output$tSNE_plot <- renderPlot({
#     tSNE_plot(plot_tsne())
#   })
  
  
#   observe({
#     # Toggle options dropdowns
#     lapply(seq_along(inputs), function(j){
#       shinyjs::onclick(paste0(options_inputs[j], "_options_show"),
#                        shinyjs::toggle(id = paste0(options_inputs[j], "_options"), 
#                                        anim = TRUE, animType = "slide", time = 0.4))
#     })
#     # Enable/disable options
#     lapply(seq_along(dens_inputs), function(j) {
#       shinyjs::toggleState(id = paste0("dens_", dens_inputs[j]), 
#                            condition = input$dens_chain_split == "Together")
#     })
#     shinyjs::toggleState(id = "ac_flip", condition = input$ac_combine == FALSE)
#     # Links to glossary
#     shinyjs::onclick("open_glossary_from_table",
#                      updateTabsetPanel(session, "nav", selected = "Glossary"))
#     shinyjs::onclick("open_glossary_from_nuts_table", 
#                      updateTabsetPanel(session, "nav", selected = "Glossary"))
#   })
} 
# END server ------------------------------------------------------
# _________________________________________________________________