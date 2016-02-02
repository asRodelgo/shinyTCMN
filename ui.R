
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinythemes)
library(shinyBS)
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
                "Please enable JavaScript to use Trade and Competitiveness Monitoring note and Operations."),
  shinyjs::useShinyjs(),
  includeCSS("css/shinytcmn.css"),
  fluidPage(
    #fluidRow(
    #  column(3,wb_logo()),
    #  #column(9, tcmn_logo())
    #  column(9, tcmn_banner())
    #),
    fluidRow(
      #column(2, h5("Explore by country:")),
      
      
      shinyjs::hidden( # hide main navigation from the home page
        div(id="homeButtons",
            column(4, h3("Trade and Competitiveness Data and Operations Snapshots", style="color:#3399ff")),
            column(3,h5("Select a country:"),
                   selectInput('inCouSel', NULL, choices=c("Select a country",countryNames$Country), selectize=FALSE)),
            column(2, div(uiOutput('outFlag'), class = "flag")),
            column(3, h5("Download a PDF report for the selected country:"),
                   downloadButton('downloadReportHome', 'Data'),
                   downloadButton('downloadReportOperHome', 'Operations'),
                   downloadButton('downloadReportFullHome', 'Full'))
            )
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
#              tabPanel(title = strong(style = "color: black", "TCMN home"),
#                       value = "home",
#                       #  source(file.path("ui_files", "country_selector_home.R"), local = TRUE)$value
#                       #),
#                       br(),
#                       includeHTML("html/home_page_links.html"),
#                       column(2,''),
#                       column(8,h5("Macroeconomic Indicators"),
#                              h6("Sources: ",
#                                 a(TCMN_sources[TCMN_sources$Source=="MFM",]$SourceDescription, 
#                                   href = TCMN_sources[TCMN_sources$Source=="MFM",]$url),"; ",
#                                 a(TCMN_sources[TCMN_sources$Source=="WDI",]$SourceDescription, 
#                                   href = TCMN_sources[TCMN_sources$Source=="WDI",]$url),"; ",
#                                 a(TCMN_sources[TCMN_sources$Source=="WEO",]$SourceDescription, 
#                                   href = TCMN_sources[TCMN_sources$Source=="WEO",]$url)),
#                              dataTableOutput('tableHome'))
#                       #column(5,div(style = "margin-top: -10px; height:450px",
#                       #    img(src = "tSNE_image.png"))),
#                       #br(),
#                       
#              ),
#              
             #### PAGE: Macro ####
          
             tabPanel(title = "Data Categories", icon = icon("stats", lib = "glyphicon"),
#                   conditionalPanel( # hide side menu if no country has been selected
#                       condition="output.hideHomePanel!='Select a country'",
#                   tabPanel("T&C Snapshots",
#                            source(file.path("ui_files", "TC_Home.R"), local = TRUE)$value
#                   ),
                      #navlistPanel(
                        #### TCMN home ####
                #shinyjs::show(
                  div(id = "homeTab",
                        tabPanel("T&C Snapshots",
                                 source(file.path("ui_files", "TC_Home.R"), local = TRUE)$value
                        )
                 # )
                ),
                shinyjs::hidden(
                  div(id = "dataTab",
                      navlistPanel(
                        #### tables ####
                        tabPanel("Macroeconomic indicators",
                                 source(file.path("ui_files", "macroTables.R"), local = TRUE)$value
                        ),
                        #### GVA ####
                        tabPanel("Gross Value Added",
                                  source(file.path("ui_files", "GVA_Treemap.R"), local = TRUE)$value,
                                  br(),
                                  source(file.path("ui_files", "GVA_Table.R"), local = TRUE)$value
                        ),
                        #### Imp/Exp ####
                        tabPanel("Imports and Exports",
                                 h4("Imports and Exports"),
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
                                 source(file.path("ui_files", "dbTable.R"), local = TRUE)$value
                        ),
                        tabPanel("Competitiveness Indicators",
                                 source(file.path("ui_files", "Compet_Indic.R"), local = TRUE)$value
                        ),
                        tabPanel("Logistic Performance Indicators",
                                 source(file.path("ui_files", "LPI.R"), local = TRUE)$value
                        ),
                        tabPanel("World Governance Indicators",
                                 source(file.path("ui_files", "WGI.R"), local = TRUE)$value
                        ),
                        tabPanel("Trade Policy",
                                 source(file.path("ui_files", "tradePolicy.R"), local = TRUE)$value
                        ),
                        tabPanel("Private Sector's Views",
                                 source(file.path("ui_files", "privateSector.R"), local = TRUE)$value
                        )
                      ) # End navlistPanel
                    #) # end conditionalPanel
                    ) # end div
                  ) # end hidden section
             ), # End Macro
             #### PAGE: Projects Portfolio ####
             tabPanel(title = "Operations",icon = icon("folder-open", lib = "glyphicon"),
                      tabsetPanel(  
                        #### projects overview ####
                        tabPanel("Overview",
                                 source(file.path("ui_files", "projectsOverview.R"), local = TRUE)$value
                        ),
                        #### sectors and themes #####
                        tabPanel("Sectors and Themes",
                                 source(file.path("ui_files", "sectorsAndThemes.R"), local = TRUE)$value
                        ),
                        #### people #####
                        tabPanel("Project teams",
                                 source(file.path("ui_files", "projectsTeams.R"), local = TRUE)$value
                        )#,
                        #### map #####
                        #tabPanel("Map",
                        #        source(file.path("ui_files", "projectsMap.R"), local = TRUE)$value
                        #)
                      ) # End tabsetPanel
             ), # End Projects Portfolio
             #### MENU: Data Analysis ####
             navbarMenu(title = "Data Analysis",icon = icon("tasks", lib = "glyphicon"),
                        #### model code ####
                        tabPanel(title = "TSNE",
                                 source(file.path("ui_files", "tSNE.R"), local = TRUE)$value
                        ), 
                        #### notepad ####
#                         tabPanel(title = "PDF generator",
#                                  h5("Generate and Download TCMN country reports (ONLY WORKS LOCALLY)"),
#                                  #selectInput('inCouMult', NULL, countryNames$Country, multiple=TRUE,selectize=FALSE),
#                                  actionButton('downloadMultipleReports', 'Generate PDF reports')
#                                  #uiOutput('downloadMultipleReports', 'Generate PDF reports'),
#                                  #textOutput('generatePDF_log')
#                         ),
                        #### hexamaps ####
                        tabPanel(title = "Hexamaps",
                                 source(file.path("ui_files", "hexamaps.R"), local = TRUE)$value
                                 #
                        )
             ), # End navbarMenu
             #### PAGE: Downloads ####
             tabPanel(title = "Downloads", icon = icon("file", lib = "glyphicon"),
                      #withMathJax(),
                      tabsetPanel(
                        #### data downloads ####
                        tabPanel("Data downloads",
                                 source(file.path("ui_files", "dataDownloads.R"), local = TRUE)$value  
                        ),
                        #### Report downloads ####
                        tabPanel("PDF Country Report",
                                 h4("Available reports for the selected country:"),br(),
                                 h5("Full T&C data and operations snapshots (4 pages)"),
                                 downloadButton('downloadReportFull', 'PDF full report'),br(),
                                 h5("T&C data snapshots (2 pages)"),
                                 downloadButton('downloadReportData', 'PDF data report'),br(),
                                 h5("T&C operations snapshots (2 pages)"),
                                 downloadButton('downloadReportOperations', 'PDF operations report')
                                 
                        )         
                      ) # End tabsetPanel
             ), # End Downloads
             #### PAGE: Metadata ####
             tabPanel(title = "Metadata", icon = icon("th-list", lib = "glyphicon"),
                      tabsetPanel(  
                        #### data sources ####
                        tabPanel("Data Sources",
                                 fluidRow(
                                   column(12,dataTableOutput('sourcesTable')))
                        ),
                        #### indicators #####
                        tabPanel("Indicators",
                                 fluidRow(
                                 column(12,dataTableOutput('indicatorsTable')))
                        ),
                        #### countries #####
                        tabPanel("Country Classification",
                                 fluidRow(
                                 column(12,dataTableOutput('countriesTable')))
                        )
                      ) # End tabsetPanel
             ), # End Metadata
             #### PAGE: Projects Portfolio ####
             tabPanel(title = "Contact",
                      source(file.path("ui_files", "contact.R"), local = TRUE)$value
                      )
             
  ) # End navbarPage
) # End tagList