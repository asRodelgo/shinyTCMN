# tsne chart ---------------------------------------------------------
.tSNE_plot <- function(num_iter, max_num_neighbors, period){
  
  ### read the data
  #TCMN_data <- fread("data/TCMN_data.csv")
  TCMN_data <- as.data.frame(TCMN_data, stringsAsFactors=FALSE)
  TCMN_data <- filter(TCMN_data, Period == period)# filter by period
  # rows correspond to countries
  TCMN_data <- spread(TCMN_data, CountryCode, Observation)
  TCMN_data <- as.data.frame(t(TCMN_data), stringsAsFactors = FALSE)
  # keep relevant rows
  TCMN_data <- TCMN_data[-c(1:7),]
  # remove NA rows
  row.all.na <- apply(TCMN_data[,2:ncol(TCMN_data)],1,function(x){all(is.na(x))})
  TCMN_data <- TCMN_data[!row.all.na,]

  TCMN_data <- mutate(TCMN_data, CountryCode = row.names(TCMN_data))
  TCMN_data <- mutate_each(TCMN_data, funs(as.numeric), -CountryCode)
  # Remove regions, keep only individual countries for now
  TCMN_data <- filter(TCMN_data, !(substr(CountryCode, 2,2) %in% c(1,2,3,4,5,6,7,8,9)))
  
  TCMN_data$CountryCode <- as.factor(TCMN_data$CountryCode)
  set.seed(123)
  # colors by region
  #countries <- read.csv("data/CountryClassification.csv")
  TCMN_data <- merge(TCMN_data, countries[,c("Country","CountryCodeISO3", "Region", "CountryCodeISO2")], 
                     by.x="CountryCode", by.y="CountryCodeISO3")
  # Further remove regions. ISO2 is empty for regions
  TCMN_data <- filter(TCMN_data, !(CountryCodeISO2==""))
  TCMN_data <- select(TCMN_data, -CountryCodeISO2)# remove CountryISO2

  colors <- rainbow(length(unique(TCMN_data$Region)))
  names(colors) <- unique(TCMN_data$Region)
  # Don't remove region
  # TCMN_data <- TCMN_data[,-ncol(TCMN_data)]
  # remove columns with all NAs
  col.all.na <- apply(TCMN_data,2,function(x){all(is.na(x))})
  TCMN_data <- TCMN_data[,!col.all.na]
  # impute NAs with the mean plus an error to avoid overlapping of country labels
  set.seed(123) #for reproducibility
  for (i in 2:ncol(TCMN_data)){
    
    TCMN_data[is.na(TCMN_data[,i]),i] <- mean(as.numeric(TCMN_data[,i]), na.rm = TRUE) * rnorm(length(TCMN_data[is.na(TCMN_data[,i]),i]),1,0.1)
  }
  
  ecb2 <- function(x,y){
    #flush.console();
    plot(x=x,t='n', axes=FALSE, frame.plot = FALSE, xlab = "",ylab = ""); 
    graphics::text(x=x,labels=as.character(TCMN_data$Country), col=colors[as.character(TCMN_data$Region)])
    #legend(-50,-1, unique(TCMN_data$Region),fill=colors[as.character(TCMN_data$Region)],
    #       bty="n", cex=0.5)
    #Sys.sleep(.09)
  }
  set.seed(456) # reproducitility again
  tSNE_plot <- tsne(TCMN_data[,-c(1,ncol(TCMN_data)-1,ncol(TCMN_data))], epoch_callback = ecb2, 
                     max_iter=as.numeric(num_iter), 
                     perplexity=as.numeric(max_num_neighbors), 
                     epoch=num_iter)
  
}  



# Rtsne chart ---------------------------------------------------------
.RtSNE_plot <- function(num_iter, max_num_neighbors){
  
  require(Rtsne)
  require(plyr)
  require(dplyr)
  require(tidyr)
  require(data.table)
  
  ### read the data
  training <- fread("/Users/asanchez3/Desktop/Data Analysis/TauMu/training.csv")
  training <- as.data.frame(training)
  training$signal <- as.factor(training$signal)
  set.seed(123)
  trainReduced <- sample_n(training,250)
  colors <- rainbow(length(unique(trainReduced$signal)))
  names(colors) <- unique(trainReduced$signal)
  
  tsne_out <- Rtsne(as.matrix(trainReduced[,c(2:48,50)]),
                    perplexity = max_num_neighbors,
                    max_iter = num_iter)
  
  plot(tsne_out$Y, t='n')
  graphics::text(tsne_out$Y, col=colors[trainReduced$signal], labels=as.character(trainReduced$signal))
  
  
}  

