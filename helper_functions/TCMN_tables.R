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
    #if (nrow(data)>0){
      # keep the latest period (excluding projections further than 2 years)
      data <- filter(data, Period <= (as.numeric(thisYear)))
      # add Period to Indicator name
      data$IndicatorShort <- paste(data$IndicatorShort, " (",data$Period,")", sep="")
      # Scale Observations
      data <- mutate(data, ObsScaled = Scale*Observation)
      
      data <- arrange(data, Key)
      # add sparkline column
      dataSpark <- data %>% 
        group_by(Key) %>%
        summarise(Trend = paste(ObsScaled, collapse = ","))
      
      data <- merge(data, dataSpark, by="Key")
      # format numbers
      data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
      
      data <- data %>%
        group_by(Key) %>%
        filter(Period == max(Period))
      
      data <- data[,c("IndicatorShort", "Source","ObsScaled","Trend")] # short indicator name and scaled data
      #data <- as.data.frame(t(data)) # transpose the data
      return(data)
      
    #} else {
    #  
    #  return(graphics::text(1.5, 1,"Data not available", col="red", cex=2))
    #}
  
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

.macroInd_Big_Spark <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode==cou, substr(Subsection,1,6)=="table2")
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
  #data <- filter(data, Period > (as.numeric(thisYear) - 4))
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
  
  # add sparkline column
  dataSpark <- data %>% 
    group_by(Key) %>%
    summarise(Trend = paste(ObsScaled, collapse = ","))
  
  data <- merge(data, dataSpark, by="Key")
  
  # final table format
  data <- spread(data, Period, ObsScaled)
  data <- data[,-1] #drop the Key column
  data <- data[,c(1,ncol(data),3:(ncol(data)-1),2)] # reorder columns
  # keep only columns I want to show
  data <- data[,c(1,2,(ncol(data)-5):ncol(data))]
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  return(data)
  
}

#############

.macroInd_Spark_Split <- function(couName, table){
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode==cou, Subsection==table)
  
  if (nrow(data[data$CountryCode==cou,])>0){    
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
    #data <- filter(data, Period > (as.numeric(thisYear) - 4))
    data <- bind_rows(data, data_avg) # add rows to data
    # Scale Observations
    data <- mutate(data, ObsScaled = Scale*Observation)
    data <- arrange(data, Key)
    data <- select(data, Key, IndicatorShort, Period, ObsScaled)
    
    # restrict to 2 decimal places
    data$ObsScaled <- round(data$ObsScaled,2)
    # add sparkline column
    dataSpark <- data %>% 
      group_by(Key) %>%
      summarise(Trend = paste(ObsScaled, collapse = ","))
    # merge
    data <- merge(data, dataSpark, by="Key")
    
    # format numbers
    data <- mutate(data, ObsScaled = ifelse(Key=="M05b",format(ObsScaled, digits=0, decimal.mark=".",
                                                     big.mark=",",small.mark=".", small.interval=3),
                                                      format(ObsScaled, digits=1, decimal.mark=".",
                                                             big.mark=",",small.mark=".", nsmall=1)))
    
    # Add notes references (automate it using TCMN_indic "Note" field
    #TCMN_indic <- read.csv("TCMN_Indicators.csv", stringsAsFactors = FALSE)
    
    # final table format
    data <- spread(data, Period, ObsScaled)
    data <- data[,-1] #drop the Key column
    data <- data[,c(1,ncol(data),3:(ncol(data)-1),2)] # reorder columns
    # keep only columns I want to show
    data <- data[,c(1,2,(ncol(data)-5):ncol(data))]
    # substitute NAs for "---" em-dash
    data[is.na(data)] <- "---"
    names(data)[1] <- "Indicator"
  } else{
    
    data[!is.na(data)] <- ""
    
  } 
  
  return(data)
  
}

#############



.ESTable <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode==cou & Subsection=="table3") #select country, region and world

  if (nrow(data[data$CountryCode==cou,])>0){
    couRegion <- as.character(countries[countries$CountryCodeISO3==cou,]$RegionCodeES)  # obtain the region for the selected country
    data <- filter(TCMN_data, CountryCode %in% c(cou,couRegion, "RWe") & Subsection=="table3") #select country, region and world
  
    # country, Region, World descriptors
    country <- as.character(countries[countries$CountryCodeISO3==cou,]$Country)
    region <- as.character(countries[countries$CountryCodeISO3==cou,]$Region) 
    world <- "All Countries"
    neighbors <- data.frame(CountryCode=c(cou,couRegion,"RWe"),colName=c(country,region,world), stringsAsFactors = FALSE)
    
#     # for column reordering purposes
#     world <- "Z All Countries"
#     neighbors$colName <- paste(substr(neighbors$CountryCode,2,2),neighbors$colName)
#     neighbors <- mutate(neighbors, colName = ifelse(substr(colName,2,4)==" Z ",colName=="Z All Countries",colName))
    # keep the latest period (excluding projections further than 2 years)
    data <- filter(data, Period <= (as.numeric(thisYear) + 1))
    # remove NAs rows
    data <- filter(data, !is.na(Observation))
    
    # prepare for table
    data <- merge(data, neighbors, by="CountryCode")
    data <- select(data, IndicatorShort, Observation, colName)
    data <- spread(data, colName, Observation)
    
    if (ncol(data)==4){
      data <- data[,c(1,4,3,2)]  
    }
    
    names(data)[1] <-"Indicator"
    
    # substitute NAs for "---" em-dash
    data[is.na(data)] <- "---"
  } else{
    
    data[!is.na(data)] <- ""
  } 
  
  return(data)
}


#############

.PolicyTable <- function(couName, dbType){
  
  if (dbType == "dbDTF") {
    dbTable <- "table4b"
    dbTitle <- "DTF (% points)"
    dbDigits <- 4
    dbSign <- -1
  } else {
    dbTable <- "table4"
    dbTitle <- "Rank"
    dbDigits <- 0
    dbSign <- 1
  }
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode == cou, Subsection==dbTable) #select country, region and world
  if (nrow(data[data$CountryCode==cou,])>0){
    # prepare for table
    data <- select(data, IndicatorShort, Period, Observation)
    data <- arrange(data, Period)
    # format numbers
    data$Observation <- format(data$Observation, digits=dbDigits, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
    data$Observation <- as.numeric(data$Observation)
    
    data <- spread(data, Period, Observation)
    # reorder rows. Want overall indicator on top
    order <- c(2,1,seq(3,nrow(data),1))
    data <- cbind(data,order)
    data <- arrange(data, order)
    data <- select(data, -order)
    # this part creates the sparkline structure for the last column of the table
    data$trend <- dbSign*data[,2] - dbSign*data[,3]
    data <- data %>%
      group_by(IndicatorShort) %>%
      mutate(trend = paste0("0,",trend))
      
    names(data) <- c("Indicator",paste("DB",names(data)[2],dbTitle),paste("DB",names(data)[3],dbTitle),"Change")
    
    # substitute NAs for "---" em-dash
    data[is.na(data)] <- "---"
  
  } else{
    
    data[!is.na(data)] <- ""
  }   
  return(data)
  
}

#############

.PolicyFacilTable <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode == cou, Subsection=="table5") #select country, region and world

  if (nrow(data[data$CountryCode==cou,])>0){  
    # prepare for table
    data <- mutate(data, ObsScaled = Scale*Observation)
    data <- select(data, Key,IndicatorShort, Period, ObsScaled)
    data <- arrange(data, Key, Period)
    # format numbers
    data$ObsScaled <- format(data$ObsScaled, digits=2, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
    
    data$ObsScaled <- as.numeric(data$ObsScaled)
    
    #data <- filter(data, Period %in% c(min(Period),max(Period)))
    
    # add sparkline column
    dataSpark <- data %>% 
      group_by(Key) %>%
      summarise(Trend = paste(ObsScaled, collapse = ","))
    
    data <- merge(data, dataSpark, by="Key")
    
    data <- spread(data, Period, ObsScaled)
    data <- data[,-1] #drop the Key column
    data <- data[,c(1,3:ncol(data),2)] # reorder columns
    
    names(data)[1] <- "Indicator"
    
    # substitute NAs for "---" em-dash
    data[is.na(data)] <- "---"
  } else{
    
    data[!is.na(data)] <- ""
  }
  return(data)
  
}



#############

.topExportsTable <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(xWits, CountryCode == cou) #select country, region and world

  if (nrow(data[data$CountryCode==cou,])>0){    
    # prepare for table
    data <- select(data, ProductDescription, ProductCode, Period, TradeValue)
    # filter by latest period
    data <- filter(data, Period==max(Period))
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
  } else{
    
    data <- data[,3:5]  
    data[!is.na(data)] <- ""
  } 

  return(data)
  
}


#############

.topImportsTable <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(mWits, CountryCode == cou) #select country, region and world
  
  if (nrow(data[data$CountryCode==cou,])>0){    
    # prepare for table
    data <- select(data, ProductDescription, ProductCode, Period, TradeValue)
    
    # filter by latest period
    data <- filter(data, Period==max(Period))
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

} else{
  
  data <- data[,3:5]  
  data[!is.na(data)] <- ""
} 
      
  return(data)
  
}

#############

.GVA_Table <- function(couName){
  
  cou <- .getCountryCode(couName)
  data <- filter(TCMN_data, CountryCode==cou, Subsection=="chart2")
  if (nrow(filter(data, CountryCode==cou))>0){
    #data <- filter(data, Period==max(Period))
    data <- select(data, IndicatorShort, Period, Observation)
    data <- filter(data, !(IndicatorShort=="Industry")) # remove "industry" category
    # Create the "Other" category
    data <- data %>%
      group_by(Period) %>%
      mutate(Other = 100 - sum(Observation))
    
    dataOther <- select(data, Period, Other)
    dataOther <- dataOther[!duplicated(dataOther),]
    dataOther <- mutate(dataOther, IndicatorShort = "Other")
    dataOther <- select(dataOther, IndicatorShort, Period, Observation = Other)
    data <- select(data, -Other)
    data <- rbind(data, dataOther)
      
    data$Observation <- round(as.numeric(data$Observation),2)
    # format numbers
    data$Observation <- format(data$Observation, digits=0, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
    data$Observation <- as.numeric(data$Observation)
    
    # add sparkline column
    dataSpark <- data %>%
      group_by(IndicatorShort) %>%
      arrange(Period) %>%
      summarise(Trend = paste(Observation, collapse = ","))
    
    data <- merge(data, dataSpark, by="IndicatorShort")
    # final table format
    data <- spread(data, Period, Observation)
    aux <- names(data)[ncol(data)]
    names(data)[ncol(data)] <- "Percent"
    data <- arrange(data, desc(Percent)) # order desc by the bigger sector
    names(data)[ncol(data)] <- aux
    
    data <- data[,c(1,(ncol(data)-3):ncol(data),2)] # reorder columns and keep only columns I want to show
    # substitute NAs for "---" em-dash
    data[is.na(data)] <- "---"
    names(data)[1] <- "Sector"
    
  } else{
    
    data <- data[,1:6]  
    data[!is.na(data)] <- ""
  } 
  
  return(data)
  
}