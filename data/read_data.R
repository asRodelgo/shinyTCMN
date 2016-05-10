# Read global data files ---------------------------------------------

# Initialize some variables
thisYear <- substr(Sys.Date(),1,4) #current year
options(scipen=999) ## Prevent scientific notation

# big data file ----------------------------

# fast read
#TCMN_data <- fread("data/TCMN_data.csv")
#TCMN_data <- as.data.frame(TCMN_data, stringsAsFactors=FALSE)
# normal speed read
TCMN_data <- read.csv("data/TCMN_data.csv", colClasses = c(rep("character",4),rep("numeric",2),rep("character",2)))

# country table ----------------------------
countries <- read.csv("data/CountryClassification.csv", stringsAsFactors = FALSE)
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
countryCoords <- read.csv("data/countryCoords.csv", stringsAsFactors = FALSE)

# indicator table ----------------------------
indicators <- read.csv("data/IndicatorClassification.csv", stringsAsFactors = FALSE)

# TCMN specific source ----------------------------
TCMN_sources <- read.csv("data/TCMN_sources.csv", stringsAsFactors = FALSE)

# TCMN specific indicators ----------------------------
TCMN_indic <- read.csv("data/TCMN_Indicators.csv", stringsAsFactors = FALSE)

# TCMN specific datasets ----------------------------
TCMN_datasets <- read.csv("data/TCMN_datasets.csv", stringsAsFactors = FALSE)

# WITS Imports ----------------------------
mWits <- read.csv("data/mWits.csv", colClasses = c(rep("character",3),rep("numeric",2),rep("character",2)))

# WITS Exports ----------------------------
xWits <- read.csv("data/xWits.csv", colClasses = c(rep("character",3),rep("numeric",2),rep("character",2)))

# IBRD T&C projects portfolio --------------
TCprojects1 <- read.csv("/Users/asanchez3/shinyTCMN/data/TCprojects1.csv", stringsAsFactors = FALSE)
TCprojects2 <- read.csv("/Users/asanchez3/shinyTCMN/data/TCprojects2.csv", stringsAsFactors = FALSE)
TCprojects <- rbind(TCprojects1,TCprojects2)
                                                             
# IFC projects portfolio --------------
IFCprojects <- read.csv("data/IFCprojects.csv", stringsAsFactors = FALSE)

# SCD/CPF most recent --------------
mostRecentDocs <- read.csv("data/SCDCPFdocuments.csv", stringsAsFactors = FALSE)

# SCD/CPF planned --------------
plannedDocs <- read.csv("data/Planneddocuments.csv", stringsAsFactors = FALSE)

