
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
      selectInput('inCouSel', NULL, choices=c("Select a country",countryNames$Country), selected = 'Afghanistan', selectize=FALSE)
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
                      br(),
                      includeHTML("html/home_page_links.html"),
                      column(2,''),
                      column(8,h5("Macroeconomic Indicators"),
                             h6("Sources: ",
                                a(TCMN_sources[TCMN_sources$Source=="MFM",]$SourceDescription, 
                                  href = TCMN_sources[TCMN_sources$Source=="MFM",]$url),"; ",
                                a(TCMN_sources[TCMN_sources$Source=="WDI",]$SourceDescription, 
                                  href = TCMN_sources[TCMN_sources$Source=="WDI",]$url),"; ",
                                a(TCMN_sources[TCMN_sources$Source=="WEO",]$SourceDescription, 
                                  href = TCMN_sources[TCMN_sources$Source=="WEO",]$url)),
                             dataTableOutput('tableHome'))
                      #column(5,div(style = "margin-top: -10px; height:450px",
                      #    img(src = "tSNE_image.png"))),
                      #br(),
                      
             ),
             
             #### PAGE: Macro ####
             tabPanel(title = "Tables and Charts", icon = icon("stats", lib = "glyphicon"),
                      navlistPanel(
                        #### tables ####
                        tabPanel("Macro tables",
                                 h5("Macroeconomic Indicators"),
                                 h6("Sources: ",
                                    a(TCMN_sources[TCMN_sources$Source=="MFM",]$SourceDescription, 
                                      href = TCMN_sources[TCMN_sources$Source=="MFM",]$url),"; ",
                                    a(TCMN_sources[TCMN_sources$Source=="WDI",]$SourceDescription, 
                                      href = TCMN_sources[TCMN_sources$Source=="WDI",]$url),"; ",
                                    a(TCMN_sources[TCMN_sources$Source=="UNCTADSTAT",]$SourceDescription, 
                                      href = TCMN_sources[TCMN_sources$Source=="UNCTADSTAT",]$url),"; ",
                                    a(TCMN_sources[TCMN_sources$Source=="WEO",]$SourceDescription, 
                                      href = TCMN_sources[TCMN_sources$Source=="WEO",]$url)),
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
                                   column(6,source(file.path("ui_files", "top_Export.R"), local = TRUE)$value),
                                   column(6,source(file.path("ui_files", "top_Import.R"), local = TRUE)$value)
                                 ),
                                 fluidRow(
                                   column(6,DT::dataTableOutput('expTable')),
                                   column(6,DT::dataTableOutput('impTable'))
                                 )
                        ),
                        tabPanel("Doing Business",
                                   h5("Doing Business Ranks"),
                                   h6("Source: ",
                                    a(TCMN_sources[TCMN_sources$Source=="DB",]$SourceDescription, 
                                      href = TCMN_sources[TCMN_sources$Source=="DB",]$url)),
                                   tags$style(HTML("
                                                   .jqstooltip{
                                                   box-sizing: content-box;
                                                   }")), # adjust tooltips in datatables
                                   dataTableOutput('db_Table')
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
                                 h6("Sources: ",
                                    a(TCMN_sources[TCMN_sources$Source=="WITS_TARIFF",]$SourceDescription, 
                                      href = TCMN_sources[TCMN_sources$Source=="WITS_TARIFF",]$url),"; ",
                                    a(TCMN_sources[TCMN_sources$Source=="WTO",]$SourceDescription, 
                                      href = TCMN_sources[TCMN_sources$Source=="WTO",]$url)),
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
             tabPanel(title = "Reports and Downloads", icon = icon("th-large", lib = "glyphicon"),
                      #withMathJax(),
                      tabsetPanel(
                        #### tables ####
                        tabPanel("Data downloads",
                                 h5("Bulk download data for all countries:"),
                                 downloadLink('bulkDownload', 'TCMN dataset'),
                                 h5("Download Products Imports for all countries:"),
                                 downloadLink('mWitsDownload', 'WITS imports'),
                                 h5("Download Products Exports for all countries:"),
                                 downloadLink('xWitsDownload', 'WITS exports'),
                                 h5("Download list of indicators:"),
                                 downloadLink('indicatorsDownload', 'TCMN indicators')
                        ),
                        #### charts ####
                        tabPanel("PDF Country Report",
                                 h5("Download full TCMN report for the selected country"),
                                 downloadButton('downloadDynamicReport', 'PDF report')
                                 
                        )         
                      ) # End tabsetPanel
             ), # End Private Sector
             
             #### PAGE: Policy Indicators ####
             tabPanel(title = "Metadata", icon = icon("th-small", lib = "glyphicon"),
                      tabsetPanel(  
                        #### data sources ####
                        tabPanel("Data Sources",
                                 dataTableOutput('sourcesTable')
                        ),
                        #### indicators #####
                        tabPanel("Indicators",
                                 dataTableOutput('indicatorsTable')
                        ),
                        #### countries #####
                        tabPanel("Country Classification", 
                                 dataTableOutput('countriesTable')
                        )
                      ) # End tabsetPanel
             ), # End Policy Indicators
             #### MENU: More ####
             navbarMenu(title = "Data Analysis",
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