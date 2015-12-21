# managing Country dimension ----------------------------------------------------

# .getCountryNameList <- function(){
#   
#   countryNames <- filter(countries, !(CountryCodeISO2==""))
#   countryNames <- select(countryNames, CountryCodeISO3, Country)# remove CountryISO2
#   #return(countryNames)
# }

.getCountryCode <- function(couName){
  
  countryCode <- filter(countries, Country==couName)$CountryCodeISO3
  #return(countryCode)
}