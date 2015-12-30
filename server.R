# options(shiny.trace=TRUE)

#object <- get(".shinystan_temp_object", envir = shinystan:::.sso_env)
source("global_utils.R", local = TRUE) #global functions available for the whole session
#source(file.path("server_files","utilities","extract_sso.R"), local = TRUE) # stan objects

# BEGIN server ------------------------------------------------------
# ___________________________________________________________________
function(input, output, session) {
  
#   observe({
#     # Stop the app when "Save & Close" button is clicked
#     if (input$save_and_close_button > 0) 
#       stopApp(object)
#   })
  
  # Source all files from server_files directory and subdirectories
  files <- list.files("server_files", full.names = TRUE, recursive = TRUE)
  for (f in files) source(f, local = TRUE)
  
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
  
  # output COuntry name --------------------------------
  output$outCouSel <- renderText(input$inCouSel)  
 
  # Home button !! not working globally !! --------------
  observeEvent(input$country_go, {
   
      shinyjs::onclick("country_go", updateTabsetPanel(session, "nav", selected = "macro"))
  })
  
  # download PDF -----------------------------
  output$downloadReport <- downloadHandler(
    filename = 'pdf/TCMN_report_TUN.pdf',
    content = function(file) file.copy('pdf/TCMN_report_TUN.pdf', file, overwrite = TRUE),
    contentType = 'application/pdf'
  )

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