#######################################################
# R functions to generate tables in TCMN report
#
# asanchezrodelgo@ifc.org - Dec 2015
#######################################################

#####
##### Tables
#####

.macroInd <- function(couName){
  
  
    cou <- .getCountryCode(couName)
    data <- filter(TCMN_data, CountryCode==cou, Subsection=="table1")
    if (nrow(data)>0){
      # keep the latest period (excluding projections further than 2 years)
      data <- filter(data, Period <= (as.numeric(thisYear)))
      data <- data %>%
        group_by(Key) %>%
        filter(Period == max(Period))
      # add Period to Indicator name
      data$IndicatorShort <- paste(data$IndicatorShort, " (",data$Period,")", sep="")
      # Scale Observations
      data <- mutate(data, ObsScaled = Scale*Observation)
      # format numbers
      data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
      
      data <- arrange(data, Key)
      data <- data[,c("IndicatorShort", "ObsScaled")] # short indicator name and scaled data
      data <- as.data.frame(t(data)) # transpose the data
      return(data)
      
    } else {
      
      return(graphics::text(1.5, 1,"Data not available", col="red", cex=2))
    }
  
}


#############

.macroInd_Big <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode==cou, Subsection=="table2")
  # keep the latest period (excluding projections further than 2 years)
  data <- filter(data, Period <= (as.numeric(thisYear) + 1))
  # remove NAs rows
  data <- filter(data, !is.na(Observation))
  # calculate average for 1st column
  data_avg <- data %>%
    group_by(Key) %>%
    filter(Period < (as.numeric(thisYear)-3)) %>%
    mutate(historical_avg = mean(Observation))
  # add average as one of the time periods
  data_avg <- mutate(data_avg, Period = paste("Avg ",as.numeric(thisYear)-13,"-",as.numeric(thisYear)-4,sep=""),
                     Observation = historical_avg, ObsScaled = Scale*historical_avg)
  
  data_avg <- data_avg[!duplicated(data_avg$Key),] # remove duplicates
  data_avg <- select(data_avg, -historical_avg, -ObsScaled) # remove some variables
  
  #keep only periods of interest in data
  data <- filter(data, Period > (as.numeric(thisYear) - 4))
  data <- rbind(data, data_avg) # add rows to data
  # Scale Observations
  data <- mutate(data, ObsScaled = Scale*Observation)
  data <- arrange(data, Key)
  data <- select(data, Key, IndicatorShort, Period, ObsScaled)
  # format numbers
  data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                           big.mark=",",small.mark=".", small.interval=3)
  
  # Add notes references (automate it using TCMN_indic "Note" field
  #TCMN_indic <- read.csv("TCMN_Indicators.csv", stringsAsFactors = FALSE)
  
  # final table format
  data <- spread(data, Period, ObsScaled)
  data <- data[,-1] #drop the Key column
  data <- data[,c(1,ncol(data),2:(ncol(data)-1))] # reorder columns
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  return(data)
  
}

#############


.ESTable <- function(couName){
  
  cou <- .getCountryCode(couName)
  couRegion <- as.character(countries[countries$CountryCodeISO3==cou,]$RegionCodeES)  # obtain the region for the selected country
  data <- filter(TCMN_data, CountryCode %in% c(cou,couRegion, "RWe") & Subsection=="table3") #select country, region and world
  
  # country, Region, World descriptors
  country <- as.character(countries[countries$CountryCodeISO3==cou,]$Country)
  region <- as.character(countries[countries$CountryCodeISO3==cou,]$Region) 
  world <- "All Countries"
  
  neighbors <- data.frame(CountryCode=c(cou,couRegion,"RWe"),colName=c(country,region,world), stringsAsFactors = FALSE)
  
  # keep the latest period (excluding projections further than 2 years)
  data <- filter(data, Period <= (as.numeric(thisYear) + 1))
  # remove NAs rows
  data <- filter(data, !is.na(Observation))
  
  # prepare for table
  data <- merge(data, neighbors, by="CountryCode")
  data <- select(data, IndicatorShort, Observation, colName)
  data <- spread(data, colName, Observation)
  data <- data[,c(1,4,3,2)]# reorder columns
  names(data)[1] <-""
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  return(data)
}


#############

.PolicyTable <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode == cou, Subsection=="table4") #select country, region and world
  
  # prepare for table
  data <- select(data, IndicatorShort, Period, Observation)
  # format numbers
  data$Observation <- format(data$Observation, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  
  data$Observation <- as.numeric(data$Observation)
  data <- spread(data, Period, Observation)
  
  
  # calculate difference in Rank
  data$ChangeRank <- data[,2] - data[,3]
  
  names(data) <- c("",paste("DB",names(data)[2],"Rank"),paste("DB",names(data)[3],"Rank"),"Change in Rank")
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  return(data)
  
}


#############

.PolicyFacilTable <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode == cou, Subsection=="table5") #select country, region and world
  
  # prepare for table
  data <- select(data, IndicatorShort, Period, Observation)
  # format numbers
  data$Observation <- format(data$Observation, digits=2, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  
  data$Observation <- as.numeric(data$Observation)
  data <- filter(data, Period %in% c(min(Period),max(Period)))
  
  data <- spread(data, Period, Observation)
  
  names(data)[1] <- ""
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  return(data)
  
}



#############

.topExportsTable <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(xWits, CountryCode == cou) #select country, region and world
  
  # prepare for table
  data <- select(data, ProductDescription, ProductCode, Period, TradeValue)
  
  # compute the percentage of total value
  data <- mutate(data, percTotalValue = 100*TradeValue/sum(TradeValue, na.rm = TRUE))
  
  # format numbers
  data$TradeValue <- format(data$TradeValue, digits=0, decimal.mark=".",
                            big.mark=",",small.mark=".", small.interval=3)
  
  data$percTotalValue <- format(data$percTotalValue, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  
  # get top 5
  data <- head(arrange(data, desc(TradeValue)),5)
  data <- select(data, -Period)
  names(data) <- c("Product (SITC4)", "Code", "Trade Value (millions of US$)", "Percent of total export value")
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  return(data)
  
}


#############

.topImportsTable <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(mWits, CountryCode == cou) #select country, region and world
  
  # prepare for table
  data <- select(data, ProductDescription, ProductCode, Period, TradeValue)
  
  # compute the percentage of total value
  data <- mutate(data, percTotalValue = 100*TradeValue/sum(TradeValue, na.rm = TRUE))
  
  # format numbers
  data$TradeValue <- format(data$TradeValue, digits=0, decimal.mark=".",
                            big.mark=",",small.mark=".", small.interval=3)
  
  data$percTotalValue <- format(data$percTotalValue, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  
  # get top 5
  data <- head(arrange(data, desc(TradeValue)),5)
  data <- select(data, -Period)
  names(data) <- c("Product (MT MFN Cats.)", "Code", "Trade Value (millions of US$)", "Percent of total export value")
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  return(data)
  
}
