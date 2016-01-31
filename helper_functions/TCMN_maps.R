############################
# Maps and other geographical visual. ---------------------
# Use this piece of code to retrieve centroid coordinates for all countries mapped to ISO3
# Already downloaded and incorporated into the read_data.R script. 
# Review if new countries are needed
##
#require(maps) # for world map
#require(mapdata) # for world map
#require(maptools) # for handling spatial objects + updated maps
#gpclibPermit() #

#load(url("http://spatial.nhh.no/R/etc/TM_WORLD_BORDERS_SIMPL-0.2.RData"))

#coordinates <- data.frame(couISO3 = wrld_simpl$ISO3,Long = wrld_simpl$LON,Lat = wrld_simpl$LAT)
#write.csv(coordinates, "data/countryCoords.csv",row.names=FALSE)
############################ 

# Country projects map data preparation ----------------
.projectsMapData <- function(status,dateRange,prodLine){
  
  #cou <- .getCountryCode(couName)
  fromDate <- as.character(dateRange[[1]])
  toDate <- as.character(dateRange[[2]])
  
  ## TC data
  #couISO2 <- .getISO2(couName)
  # calculate total amount per project
  dataTC <- TCprojects %>%
    group_by(PROJ_ID) %>%
    mutate(Project_Amount = IBRD_CMT_USD_AMT + GRANT_USD_AMT + IDA_CMT_USD_AMT,
           Prod_Line = ifelse(tolower(substr(PROD_LINE_TYPE_NME,1,4))=="lend","Financing",
                              ifelse(tolower(substr(PROD_LINE_TYPE_NME,1,3))=="aaa",
                                     "Advisory Services and Analytics (ASA) IBRD",PROD_LINE_TYPE_NME)),
           ProjectOrder = ifelse(PROJECT_STATUS_NME=="Active",1,ifelse(PROJECT_STATUS_NME=="Pipeline",2,3))) %>%
    select(-IBRD_CMT_USD_AMT, -GRANT_USD_AMT, -IDA_CMT_USD_AMT) %>%
    filter(PROJECT_STATUS_NME %in% c("Closed","Active","Pipeline")) %>%
    filter(!(tolower(substr(Prod_Line,1,8))=="standard"))
  # Need ISO3 codes
  dataTC <- merge(dataTC, countries[,c("CountryCodeISO3","CountryCodeISO2")], by.x="WBG_CNTRY_KEY",by.y="CountryCodeISO2")
#
#   dataTC <- dataTC %>%
#     group_by(PROJ_ID,WBG_CNTRY_KEY) %>%
#     mutate(countryCode = .getISO3fromISO2(WBG_CNTRY_KEY))
  
  # select relevant variables
  dataTC <- select(dataTC, countryCode=CountryCodeISO3, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME,
                   Approval_Date = BD_APPRVL_DATE, Project_Status = PROJECT_STATUS_NME,
                   Project_Amount, ProjectOrder)
  # filter by date range
  dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # filter by status
  dataTC <- filter(dataTC, Project_Status==status)
  # filter by type
  dataTC <- filter(dataTC, Prod_Line==prodLine)
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line), ProjectOrder)
  dataTC <- select(dataTC,-ProjectOrder) # drop ProjectOrder
  
  ### IFC projects ----------
  dataIFC <- filter(IFCprojects, (PROJECT_STAGE=="PIPELINE") | (PROJECT_STATUS %in% c("ACTIVE", "PIPELINE", "CLOSED")),
                    PROJECT_TYPE == "AS PROJECTS WITH CLIENT(S)")
  dataIFC <- mutate(dataIFC, Prod_Line = "Advisory Services and Analytics (ASA) IFC",
                    countryCode = COUNTRY_CODE,
                    Project_Status = ifelse(PROJECT_STAGE=="PIPELINE","Pipeline",ifelse(PROJECT_STATUS=="CLOSED","Closed","Active")))
  dataIFC <- mutate(dataIFC, ProjectOrder = ifelse(Project_Status=="Active",1,ifelse(Project_Status=="Pipeline",2,3)))
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  
  # keep relevant columns
  dataIFC <- select(dataIFC, countryCode, PROJ_ID, Prod_Line, Project_Name = PROJECT_NAME,
                    Approval_Date = ASIP_APPROVAL_DATE, Project_Status, Project_Amount = TOTAL_FUNDS_MANAGED_BY_IFC,
                    ProjectOrder
  )
  # filter by date range
  dataIFC <- filter(dataIFC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # filter by status
  dataIFC <- filter(dataIFC, Project_Status==status)
  # filter by type
  dataIFC <- filter(dataIFC, Prod_Line==prodLine)
  # arrange
  dataIFC <- arrange(as.data.frame(dataIFC), ProjectOrder)
  dataIFC <- select(dataIFC,-ProjectOrder) # drop ProjectOrder
  
  # Append both ----------------------
  data <- rbind_list(dataTC, dataIFC)
  # remove duplicates
  data <- data[!duplicated(data$PROJ_ID),]
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  # merge with countryCoords to add long and lat
  data <- merge(data, countryCoords, by.x = "countryCode", by.y="couISO3")
  
  #names(data) <- c("Country Code","Project ID","Line", "Project Name", "Approval Date", "Status", "Amount (in US$)")
  
  return(data)
}

#############


#.projectsMap <- function(){
  #function(status,dateRange,prodLine){
  
  ##### This piece of code is intended for choropleths
  #library(rgdal) # to read .shp files containing country shapes
  #library(maps)
  ## From http://data.okfn.org/data/datasets/geo-boundaries-world-110m
  ## downloaded geojson file and kept it in data folder
  #mapCountries <- readOGR("data/countries.geojson", "OGRGeoJSON")
  #map <- leaflet(mapCountries)
  #cou <- .getCountryCode(couName)
  #data <- .filterTCProjects(couName)
  #leaflet(data = map) %>%  
  #   addTiles() %>%
  #   addPolygons(fillColor = "blue", 
  #                 fillOpacity = 0.8, 
  #                 color = "#BDBDC3", 
  #                 weight = 1,
  #                 popup = .showPopup(couName)
  #                 ) %>%
  #   setView(lng = 0, lat = 0, zoom = 2)
  
  #### This piece of code is intended for markers on countries
#   leaflet(data = countryCoords) %>%
#     addTiles() %>%
#     addMarkers(lng = ~Long, lat = ~Lat, popup = .showPopup(status,timeRange,type)) %>%
#     setView(lng = 0, lat = 0, zoom = 2)
# 
# }

# Show a popup with data at the given location ----------------
.showPopup <- function(status,dateRange,prodLine,countryCode, lat, lng) {
  
  #cou <- .getCountryCode(couMap)
  data <- .projectsMapData(status,dateRange,prodLine)
  data <- filter(data, countryCode==countryCode)
  
  content <- paste(status,dateRange,prodLine,countryCode,lat,lng)
  #content <- sprintf("Num rows", nrow(data))
#   content <- as.character(tagList(
#     tags$h4("Country:", data$countryCode[1]),
#     #tags$strong(HTML(sprintf("%s, %s %s",
#     #                         selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
#     #))), tags$br(),
#     sprintf("Project ID", data$PROJ_ID[1]), tags$br())
#   )
  
  #return(content)
  leafletProxy("projectsMap") %>% addPopups(lng, lat, content, layerId = countryCode)
  
}

