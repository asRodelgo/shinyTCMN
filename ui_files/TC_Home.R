# T&C home -------------------------------
fluidPage(
  #column(3,wb_logo(),style="text-align: center;width:20%;height:20%"),
  column(12,h1("Trade and Competitiveness Data and Operations Snapshots", style="color:#3399ff;text-align: center")),
  br(),
  column(2),
  column(8,h4("The Trade and Competitiveness Data and Operations Snapshots project consists of a full cycle of data collection, processing and dissemination of key T&C indicators taken from more than 12 data sources and selected by the regional country leaders and lead economists in the field.", 
     style="color:#646464;text-align:center;left:100px;right:100px")),
  column(2),
  br(),br(),
  column(12,h4("Select a country and start exploring!", style="color:#3399ff;text-align: center"),
    align="center",
    selectInput('inCouSelHome', NULL, choices=c("Select a country",countryNames$Country), selected = 'Select a country', selectize=FALSE)
  ),
br(),br(),
column(12,h4("In this site you will be able to:"), style="color:#646464;text-align: center"),
br(),
column(12,h4("Browse T&C data and operations for all countries through tables, charts and maps", style="color:#646464;text-align: center"),
       h4("Access data analysis and other featured data stories", style="color:#646464;text-align: center"),
       h4("Download summary data and operations PDF reports for a selected country", style="color:#646464;text-align: center")),
br(),
column(12,h4("Download Operations by Country Departments", style="color:#3399ff;text-align: center"),
       align="center",
       selectInput('inCouDepHome', NULL, choices=list("Select a country department",`East Asia and Pacific` = unique(filter(countryDeps, RegionCodeALL == "R1")$CMU),
                                                      `Europe and Central Asia` = unique(filter(countryDeps, RegionCodeALL == "R2")$CMU),
                                                      `Latin America and Caribbean` = unique(filter(countryDeps, RegionCodeALL == "R3")$CMU),
                                                      `Middle East and North Africa` = unique(filter(countryDeps, RegionCodeALL == "R4")$CMU),
                                                      `South Asia` = unique(filter(countryDeps, RegionCodeALL == "R6")$CMU),
                                                      `Sub-Saharan Africa` = unique(filter(countryDeps, RegionCodeALL == "R7")$CMU)), 
                                          selected = "Select a country department", selectize=FALSE),
       downloadButton('downloadRepsDeps', 'Download Reports')
  #includeHTML("html/home_page_links.html")
)
)