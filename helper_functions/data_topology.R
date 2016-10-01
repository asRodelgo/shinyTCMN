# This script contains functions to generate the data topology or data story teller


# Data processing

.prepare_data <- function(){
  
  data_filter <- TCMN_data %>%
    filter(!grepl("M",Period) & CountryCode %in% countryNames$CountryCodeISO3 & !(Subsection %in% c("table4","chart5"))) %>%
    dplyr::select(CountryCode,Period,Observation,Source,IndicatorShort) %>%
    distinct(.keep_all=TRUE)
  
  data_merge <- merge(data_filter,countries[,c("CountryCodeISO3","RegionShort","RegionShortIncome","CountryShort")],
                      by.x="CountryCode", by.y="CountryCodeISO3")
  
  data_spread <- spread(data_merge, IndicatorShort, Observation)
  
  # remove all NA rows
  data_tsne <- data_spread[rowSums(is.na(data_spread))<ncol(data_spread[,-c(1:6)]),]
  
  # impute remaining NAs
  set.seed(123) #for reproducibility
  for (i in 7:ncol(data_tsne)){
    
    data_tsne[is.na(data_tsne[,i]),i] <- mean(as.numeric(data_tsne[,i]), na.rm = TRUE) * rnorm(length(data_tsne[is.na(data_tsne[,i]),i]),1,0.1)
  }
  
  # scale to [0,1] to improve tsne final shape
  maxs <- apply(data_tsne[,-c(1:6)], 2, max) 
  mins <- apply(data_tsne[,-c(1:6)], 2, min)
  data_tsne[,-c(1:6)] <- as.data.frame(scale(data_tsne[,-c(1:6)], center = mins, scale = maxs - mins))

  return(data_tsne)
}

# TSNE writer

.generateTSNE <- function(){
  
  data_tsne <- .prepare_data()
  data_tsne_sample <- filter(data_tsne, Period < "2017")
  
}

# Plots section

