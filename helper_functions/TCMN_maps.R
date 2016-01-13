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

.projectsMap <- function(){
  
  leaflet(countryCoords) %>%
    addTiles(
      #urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      #attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    addMarkers(~Long, ~Lat,popup= ~couISO3) %>%
    setView(lng = 10, lat = 20, zoom = 3)

}

