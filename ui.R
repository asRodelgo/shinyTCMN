
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)

#object <- get(".shinystan_temp_object", envir = shinystan:::.sso_env)
source("global_utils.R", local = TRUE)
#rm(object) #remove object
#gc() #garbage collect


# Begin shinyUI -----------------------------------------------------------
# _________________________________________________________________________
tagList(
  tags$noscript(style = "color: orange; font-size: 30px; text-align: center;", 
                "Please enable JavaScript to use shinytcmn."),
  shinyjs::useShinyjs(),
  includeCSS("css/shinytcmn.css"),
  # navbarPage(save_and_close, id = "nav", #title = NULL,
  navbarPage(wb_logo(), id = "wb-logo", #title = NULL,
             windowTitle = "ShinyTCMN", collapsible = TRUE, 
             inverse = FALSE, position = "fixed-top",
             theme = shinythemes::shinytheme("flatly"),
             
             #### HOME PAGE ####
             tabPanel(title = strong(style = "color: #009900;", "TCMN home"),
                      value = "home",
                      tcmn_logo(),
                      br(),
                      source(file.path("ui_files", "country_selector_home.R"), local = TRUE)$value,
                      br(),br(),br(),br(),
                      includeHTML("html/home_page_links.html")
             ),
             
             #### PAGE: Macro ####
             tabPanel(title = "Macro Indicators", icon = icon("stats", lib = "glyphicon"),
                      tabsetPanel(
                        #### tables ####
                        tabPanel("Tables",
                                 h5("Some tables"),
                                 source(file.path("ui_files", "country_selector.R"), local = TRUE)$value
                        ),
                        #### Charts ####
                        tabPanel("Charts",
                                 h5("Some charts"),
                                 br(),
                                 source(file.path("ui_files", "charts_ui.R"), local = TRUE)$value,
                                 br()
                        ),
                        #### Metadata ####
                        tabPanel("Metadata",
                                 h5("Tell me stuff about those indicators")
                        )
                        
                      ) # End tabsetPanel
             ), # End Macro
             
             #### PAGE: Private Sector Views ####
             tabPanel(title = "Private Sector's Views", icon = icon("coins", lib = "glyphicon"),
                      #withMathJax(),
                      tabsetPanel(
                        #### tables ####
                        tabPanel("Tables",
                                 h5("Some tables")
                        ),
                        #### charts ####
                        tabPanel("Charts",
                                 h5("Some charts")
                        ),
                        #### Metadata ####
                        tabPanel("Metadata",
                                 h3("Tell me stuff about those indicators")
                        )
                      ) # End tabsetPanel
             ), # End Private Sector
             
             #### PAGE: Policy Indicators ####
             tabPanel(title = "Policy Indicators", icon = icon("group", lib = "glyphicon"),
                      tabsetPanel(  
                        #### tables ####
                        tabPanel("Tables", icon = icon("th-large", lib = "glyphicon"),
                                 splitLayout(h5("Left side"), h5("Right side"))
                        ),
                        #### charts #####
                        tabPanel("charts",icon = icon("stats", lib = "glyphicon"),
                                helpText(style = "font-size: 11px", "For Stan models using the NUTS algorithm, red points indicate iterations that encountered a divergent transition.",  
                                         "Yellow points indicate a transition that hit the maximum treedepth",
                                         "rather than terminated its evolution normally."),
                                hr()
                        ),
                        #### metadata #####
                        tabPanel("Metadata", 
                                helpText(style = "font-size: 12px;", "Use your mouse and trackpad to rotate the plot and zoom in or out.")
                        )
                      ) # End tabsetPanel
             ), # End Policy Indicators
             
             #### MENU: Trade Policy ####
             tabPanel(title = "Trade Policy",
                        tabsetPanel(  
                           #### tables ####
                           tabPanel("Tables", icon = icon("cargo", lib = "glyphicon"),
                                    splitLayout(h5("Left side"), h5("Right side"))
                           ),
                           #### charts #####
                           tabPanel("charts",icon = icon("stats", lib = "glyphicon"),
                                    helpText(style = "font-size: 11px", "For Stan models using the NUTS algorithm, red points indicate iterations that encountered a divergent transition.",  
                                             "Yellow points indicate a transition that hit the maximum treedepth",
                                             "rather than terminated its evolution normally."),
                                    hr()
                           ),
                           #### metadata #####
                           tabPanel("Metadata", 
                                    helpText(style = "font-size: 12px;", "Use your mouse and trackpad to rotate the plot and zoom in or out.")
                           )
                        ) # End tabsetPanel
             ) # End Trade Policy
  ) # End navbarPage
) # End tagList