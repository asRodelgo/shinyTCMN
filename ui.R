
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
    #fluidRow(
    #  column(3,wb_logo()),
    #  #column(9, tcmn_logo())
    #  column(9, tcmn_banner())
    #),
    fluidRow(
      #column(2, h5("Explore by country:")),
      column(4, h3("Trade and Competitiveness Monitoring Note", style="color:#3399ff")),
      column(3,h5("Select a country:"),
             selectInput('inCouSel', NULL, choices=c("Select a country",countryNames$Country), selected = 'Afghanistan', selectize=FALSE)),
      column(2, div(uiOutput('outFlag'), class = "flag"))
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
                                 source(file.path("ui_files", "Compet_Indic.R"), local = TRUE)$value
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
             tabPanel(title = "Reports and Downloads", icon = icon("file", lib = "glyphicon"),
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
                                 downloadButton('downloadReport', 'PDF report')
                                 
                        )         
                      ) # End tabsetPanel
             ), # End Private Sector
             
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
             tabPanel(title = "Projects Portfolio",icon = icon("folder-open", lib = "glyphicon"),
                      tabsetPanel(  
                        #### data sources ####
                        tabPanel("Overview",
                                 fluidRow(
                                   column(12,dataTableOutput('projectsTable')))
                        ),
                        #### indicators #####
                        tabPanel("Sectors and Themes"
                                 #fluidRow(
                                #   column(12,dataTableOutput('indicatorsTable')))
                        ),
                        #### countries #####
                        tabPanel("Country teams"
                                 #fluidRow(
                                #   column(12,dataTableOutput('countriesTable')))
                        )
                      ) # End tabsetPanel
             ), # End Projects Portfolio
             #### MENU: More ####
             navbarMenu(title = "Data Analysis",icon = icon("tasks", lib = "glyphicon"),
                        #### model code ####
                        tabPanel(title = "TSNE",
                                source(file.path("ui_files", "tSNE.R"), local = TRUE)$value
                        ), 
                        #### notepad ####
                        tabPanel(title = "PDF generator",
                                h5("Generate and Download TCMN country reports (ONLY WORKS LOCALLY)"),
                                #selectInput('inCouMult', NULL, countryNames$Country, multiple=TRUE,selectize=FALSE),
                                actionButton('downloadMultipleReports', 'Generate PDF reports')
                                #uiOutput('downloadMultipleReports', 'Generate PDF reports'),
                                #textOutput('generatePDF_log')
                        ),
                        #### help ####
                        tabPanel(title = "Hexamaps",
                                 source(file.path("ui_files", "hexamaps.R"), local = TRUE)$value
                        #         source(file.path("ui_files", "help.R"), local = TRUE)$value
                        #
                        )
             ) # End navbarMenu
             
  ) # End navbarPage
) # End tagList