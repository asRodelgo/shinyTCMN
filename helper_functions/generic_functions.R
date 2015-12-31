# general purpose helper functions ----------------------------------------------------

#####
##### Auxiliary functions
#####

.getISO2 <- function(couName){
  
  #countryISO2 <- tolower(as.character(countries[countries$CountryCodeISO3==couName,]$CountryCodeISO2))
  countryISO2 <- tolower(as.character(filter(countries,Country==couName)$CountryCodeISO2))
#   if (length(countryISO2)==1){
#     return(countryISO2)
#   } else{
#     return(0)
#   }
  #return(countryISO2)
}

.getRegion <- function(couName){
  
  cou <- .getCountryCode(couName)
  region <- as.character(countries[countries$CountryCodeISO3==cou,]$RegionShort) 
}

.getCountryCode <- function(couName){
  
  countryCode <- filter(countries, Country==couName)$CountryCodeISO3
  if (length(countryCode)==1){
    return(countryCode)
  } else{
    return(0)
  }
  #return(countryCode)
}

# country flags -----------------------------------
.outFlag <- function(couName){
  
  iso <- .getISO2(couName)  
  if (paste0(iso,".png")==".png"){
    
    tags$img(src="world.png", width="40%")  
    
  } else{
    
    tags$img(src=paste0(iso,".png"), width="40%")  
  } 
  
}

