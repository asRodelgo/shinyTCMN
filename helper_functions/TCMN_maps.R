# Maps and other geographical visual. ---------------------
# Use this piece of code to retrieve centroid coordinates for all countries mapped to ISO3
# require(maps) # for world map
# require(mapdata) # for world map
# require(maptools) # for handling spatial objects + updated maps
# gpclibPermit() #
# 
# load(url("http://spatial.nhh.no/R/etc/TM_WORLD_BORDERS_SIMPL-0.2.RData"))
# 
# coordinates <- data.frame(couISO3 = wrld_simpl$ISO3,Long = wrld_simpl$LON,Lat = wrld_simpl$LAT)
# write.csv(coordinates, "data/countryCoords.csv",row.names=FALSE)

.projectsMap <- function(couName){
  
  cou <- .getCountryCode(couName)
  
  leaflet(data = countryCoords) %>%
    addTiles() %>%
    addMarkers(lng = ~Long, lat = ~Lat, popup = .showPopup(couName)) %>%
    setView(lng = 0, lat = 0, zoom = 2)

}

# Show a popup at the given location ----------------
.showPopup <- function(couName) {
  
  cou <- .getCountryCode(couName)
  dataTC <- .filterTCProjects(couName)
  
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Status=PROJECT_STATUS_NME, 
                   Approval_Date = BD_APPRVL_DATE, Project_Amount, ProjectOrder)
  # filter by date range
  #dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # remove duplicates
  dataTC <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line), ProjectOrder)
  dataTC <- select(dataTC,-ProjectOrder,-Approval_Date) # drop ProjectOrder
  
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects(couName)
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Prod_Line, Project_Name = PROJECT_NAME,
                    Approval_Date = ASIP_APPROVAL_DATE, Project_Status, Project_Amount = TOTAL_FUNDS_MANAGED_BY_IFC,
                    ProjectOrder)
  # remove duplicates
  dataIFC <- dataIFC[!duplicated(dataIFC$PROJ_ID),]
  # filter by date range
  #dataIFC <- filter(dataIFC, (Approval_Date >= fromDate) & (Approval_Date <= toDate)) #select country
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  # arrange
  dataIFC <- arrange(as.data.frame(dataIFC), ProjectOrder)
  dataIFC <- select(dataIFC,-ProjectOrder) # drop ProjectOrder
  # Append both -----------------------
  data <- rbind_list(dataTC, dataIFC)
  # remove duplicates
  data <- data[!duplicated(data$PROJ_ID),]
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  
  content <- as.character(tagList(
    tags$h4("Amount:", data$Project_Amount),
    #tags$strong(HTML(sprintf("%s, %s %s",
    #                         selectedZip$city.x, selectedZip$state.x, selectedZip$zipcode
    #))), tags$br(),
    sprintf("Project ID", data$PROJ_ID), tags$br()))
  
  return(content)
  #leafletProxy("projectsMap") %>% addPopups(lng, lat, content, layerId = couName)
}

