
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
# use javascript
library(shinyjs)
library(V8)

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
  fluidPage(
    fluidRow(
      column(3,wb_logo()),
      column(9, tcmn_logo())
    ),
    fluidRow(
      column(1, div(uiOutput('outFlag'), class = "flag")),
      column(2, h5("Explore by country:"),
      selectInput('inCouSel', NULL, choices=c("Select a country",countryNames$Country),selectize=FALSE)
      )
    )
  ),
  # navbarPage(save_and_close, id = "nav", #title = NULL,
  navbarPage(#tcmn_logo(), id = "tcmn-logo", 
             title = NULL,
             windowTitle = "ShinyTCMN", collapsible = TRUE, 
             inverse = FALSE, position = "fixed-top",
             theme = shinythemes::shinytheme("flatly"),
             #### HOME PAGE ####
             tabPanel(title = strong(style = "color: black", "TCMN home"),
                      value = "home",
                      #  source(file.path("ui_files", "country_selector_home.R"), local = TRUE)$value
                      #),
                      div(style = "margin-top: -10px; height:450px",
                          img(src = "tSNE_image.png")),
                      #br(),
                      includeHTML("html/home_page_links.html")
             ),
             
             #### PAGE: Macro ####
             tabPanel(title = "Macro Indicators", icon = icon("stats", lib = "glyphicon"),
                      navlistPanel(
                        #### tables ####
                        tabPanel("Macro tables",
                                 h5("Macroeconomic Indicators"),
                                 tags$style(HTML("
                                                 .jqstooltip{
                                                 box-sizing: content-box;
                                                 }")), # adjust tooltips in datatables
                                 dataTableOutput('tableSpark')
                                 #DT::dataTableOutput('table2')
                        ),
                        #### GVA ####
                        tabPanel("Gross Value Added",
                                  source(file.path("ui_files", "GVA_Treemap.R"), local = TRUE)$value,
                                  br(),
                                  source(file.path("ui_files", "GVA_Table.R"), local = TRUE)$value
                        ),
                        #### Imp/Exp ####
                        tabPanel("Imports and Exports",
                                 h5("Imports and Exports"),
                                 source(file.path("ui_files", "ExpImp_HF.R"), local = TRUE)$value,
                                 fluidRow(
                                   column(7,DT::dataTableOutput('expTable')),
                                   column(5,source(file.path("ui_files", "top_Export.R"), local = TRUE)$value)
                                 ),
                                 fluidRow(
                                   column(7,DT::dataTableOutput('impTable')),
                                   column(5,source(file.path("ui_files", "top_Import.R"), local = TRUE)$value)
                                 )
                        ),
                        tabPanel("Doing Business",
                                   h5("Doing Business Ranks"),
                                   tags$style(HTML("
                                                   .jqstooltip{
                                                   box-sizing: content-box;
                                                   }")), # adjust tooltips in datatables
                                   dataTableOutput('db_Table')
#                                  h5("Download full TCMN report for the selected country"),
#                                  downloadButton('downloadReport', 'PDF report')
                        ),
                        tabPanel("Competitiveness Indicators",
                                 source(file.path("ui_files", "compet_Indic.R"), local = TRUE)$value
                        ),
                        tabPanel("Logistic Performance Indicators",
                                 source(file.path("ui_files", "LPI.R"), local = TRUE)$value
                        ),
                        tabPanel("World Governance Indicators",
                                 source(file.path("ui_files", "WGI.R"), local = TRUE)$value
                        ),
                        tabPanel("Trade Policy",
                                 h5("Trade Policy Indicators"),
                                 tags$style(HTML("
                                                 .jqstooltip{
                                                 box-sizing: content-box;
                                                 }")), # adjust tooltips in datatables
                                 dataTableOutput('tradePolicy_Table')
                        ),
                        tabPanel("Private Sector's Views",
                                 source(file.path("ui_files", "privateSector.R"), local = TRUE)$value
                        )
                      ) # End navlistPanel
             ), # End Macro
             
             #### PAGE: Private Sector Views ####
             tabPanel(title = "Private Sector's Views", icon = icon("stats", lib = "glyphicon"),
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
             tabPanel(title = "Performance Indicators", icon = icon("stats", lib = "glyphicon"),
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
             #### MENU: More ####
             navbarMenu(title = "More",
                        #### model code ####
                        tabPanel(title = "TSNE",
                                source(file.path("ui_files", "tSNE.R"), local = TRUE)$value
                        ), 
                        #### notepad ####
                        tabPanel(title = "TSNE JS"
                                #source(file.path("ui_files", "tSNE_JS.R"), local = TRUE)$value
                        ),
                        #### about ####
                        tabPanel(title = "About", 
                                 logo_and_name()
                        #         div(style = "margin-top: 75px;",
                        #             source(file.path("ui_files", "about.R"), local = TRUE)$value
                        #         )
                        ),
                        #### glossary ####
                        tabPanel(title = "Glossary"
                        #         div(style = "background-color: white;",
                        #             h1(style = "text-align: center;", "Glossary"),
                        #             source(file.path("ui_files", "glossary.R"), local = TRUE)$value,
                        #             hr(),
                        #             stan_manual()
                        #         )
                        ),
                        #### help ####
                        tabPanel(title = "Help"
                        #         h1(style = "text-align: center;", "Help"),
                        #         source(file.path("ui_files", "help.R"), local = TRUE)$value
                        #
                        )
             ) # End navbarMenu
             
  ) # End navbarPage
) # End tagList