# general purpose helper functions ----------------------------------------------------

#####
##### Auxiliary functions
#####

.getISO2 <- function(couName){
  
  #countryISO2 <- tolower(as.character(countries[countries$CountryCodeISO3==couName,]$CountryCodeISO2))
  countryISO2 <- tolower(as.character(filter(countries,Country==couName)$CountryCodeISO2))
  #return(countryISO2)
}

.getCountryCode <- function(couName){
  
  countryCode <- filter(countries, Country==couName)$CountryCodeISO3
  #return(countryCode)
}

# country flags -----------------------------------
.outFlag <- function(couName){
  
  iso <- .getISO2(couName)  
  #images <- list.files(path="./www",pattern="*.png")
  tags$img(src=paste0(iso,".png"), width="40%")
}

