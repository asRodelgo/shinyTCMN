# This scripts reads data for developing version of the app where data is stored in a different place
# 

TCMN_data <- read.csv("/Users/asanchez3/srv/shiny-server/shinyTCMN-data/data/TCMN_data.csv", colClasses = c(rep("character",4),rep("numeric",2),rep("character",2)))

# country table ----------------------------
countries <- read.csv("/Users/asanchez3/srv/shiny-server/shinyTCMN-data/data/CountryClassification.csv", stringsAsFactors = FALSE)
# Avoid ISO2 code for Namibia be confused by NA
countries[countries$CountryCodeISO3=="NAM",]$CountryCodeISO2 <- "NA"
countries <- arrange(countries, Country)

# list of only countries (useful for selectors and others)
countryNames <- filter(countries, !(CountryCodeISO2==""))
countryNames <- select(countryNames, CountryCodeISO3, Country)# remove CountryISO2
# list of country departments
countryDeps <- filter(countries, !(CMU==""))
countryDeps <- arrange(select(countryDeps, CountryCodeISO3, RegionCodeALL, Region ,CMU), CMU)

# country Coordinates --------------
#countryCoords <- read.csv("/Users/asanchez3/srv/shiny-server/shinyTCMN-data/data/countryCoords.csv", stringsAsFactors = FALSE)

# indicator table ----------------------------
indicators <- read.csv("/Users/asanchez3/srv/shiny-server/shinyTCMN-data/data/IndicatorClassification.csv", stringsAsFactors = FALSE)

# TCMN specific source ----------------------------
TCMN_sources <- read.csv("/Users/asanchez3/srv/shiny-server/shinyTCMN-data/data/TCMN_sources.csv", stringsAsFactors = FALSE)

# TCMN specific indicators ----------------------------
TCMN_indic <- read.csv("/Users/asanchez3/srv/shiny-server/shinyTCMN-data/data/TCMN_Indicators.csv", stringsAsFactors = FALSE)

# TCMN specific datasets ----------------------------
TCMN_datasets <- read.csv("/Users/asanchez3/srv/shiny-server/shinyTCMN-data/data/TCMN_datasets.csv", stringsAsFactors = FALSE)

# WITS Imports ----------------------------
mWits <- read.csv("/Users/asanchez3/srv/shiny-server/shinyTCMN-data/data/mWits.csv", colClasses = c(rep("character",3),rep("numeric",2),rep("character",2)))

# WITS Exports ----------------------------
xWits <- read.csv("/Users/asanchez3/srv/shiny-server/shinyTCMN-data/data/xWits.csv", colClasses = c(rep("character",3),rep("numeric",2),rep("character",2)))

# IBRD T&C projects portfolio --------------
TCprojects <- read.csv("/Users/asanchez3/srv/shiny-server/shinyTCMN-data/data/TCprojects.csv", stringsAsFactors = FALSE)

# IFC projects portfolio --------------
IFCprojects <- read.csv("/Users/asanchez3/srv/shiny-server/shinyTCMN-data/data/IFCprojects.csv", stringsAsFactors = FALSE)

# SCD/CPF most recent --------------
mostRecentDocs <- read.csv("/Users/asanchez3/srv/shiny-server/shinyTCMN-data/data/SCDCPFdocuments.csv", stringsAsFactors = FALSE)

# SCD/CPF planned --------------
plannedDocs <- read.csv("/Users/asanchez3/srv/shiny-server/shinyTCMN-data/data/Planneddocuments.csv", stringsAsFactors = FALSE)
