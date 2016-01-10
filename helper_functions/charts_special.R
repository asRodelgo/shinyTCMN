# tsne chart ---------------------------------------------------------
.tSNE_plot <- function(couName, num_iter, max_num_neighbors, period){
  
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
  
#   library(hexamapmaker)
#   hexamap <- function(z){
#     
#     z <- tSNE_plot[1:20,]
#     data_j <- data.frame(id=as.character(TCMN_data$Country[1:20]), x=z[,1], y=z[,2])
#     #data_z$id <- as.character(data_z$id)
#     zz <- hexamap(data_j)
#     # Plot hexamaps
#     p <- ggplot(zz, aes(x, y, group = labels)) +
#       geom_polygon(colour="black", fill = colors[as.character(TCMN_data$Region)]) +
#       coord_fixed(ratio = 1)
#     
#     hexalabel(zz,p)
#     
#   }
    
  set.seed(456) # reproducitility
  tSNE_plot <- tsne(TCMN_data[,-c(1,ncol(TCMN_data)-1,ncol(TCMN_data))], epoch_callback = ecb2, 
                     max_iter=as.numeric(num_iter), 
                     perplexity=as.numeric(max_num_neighbors), 
                     epoch=num_iter)
  # calculate the euclidean distance between the selected country and the rest
  dist_mat <- cbind(tSNE_plot,TCMN_data$Country)
  dist_mat <- as.data.frame(dist_mat, stringsAsFactors=FALSE)
  dist_mat$V1 <- as.numeric(dist_mat$V1)
  dist_mat$V2 <- as.numeric(dist_mat$V2)
  distCou1 <- dist_mat[dist_mat$V3==couName,1]
  distCou2 <- dist_mat[dist_mat$V3==couName,2]
  dist_mat <- mutate(dist_mat, dist = sqrt((V1-distCou1)^2+(V2-distCou2)^2))
  # order by less distance to selected country
  dist_mat <- arrange(dist_mat, dist)[,c(3,4)]
  return(dist_mat)
}  

.hexagon_allocation <- function(num_rings, filter){
  
  # Hexagon allocation algorithm
  iter <- 1
  d <- data.frame(x=rep(888,200),y=rep(888,200),ring=rep(0,200))
  d[iter,1] <- d[iter,2] <- 0
  iter <- iter + 1
  for (i in 2:num_rings){
    d[iter,3] <- i
    if (round(i/2)==i/2){ # i is even
      for (j in 1:(i/2)){
        d[iter,1] <- 2*j - 1
        d[iter,2] <- 2*(i-1)
        iter <- iter + 1
        d[iter,3] <- i
        d[iter,1] <- -2*j + 1
        d[iter,2] <- 2*(i-1)
        iter <- iter + 1
        d[iter,3] <- i
        d[iter,1] <- 2*j - 1
        d[iter,2] <- -2*(i-1)
        iter <- iter + 1
        d[iter,3] <- i
        d[iter,1] <- -2*j + 1
        d[iter,2] <- -2*(i-1)
        iter <- iter + 1
        d[iter,3] <- i
      }
    } else { # i is odd
      for (j in 1:(((i-1)/2)+1)){
        d[iter,1] <- 2*(j-1)
        d[iter,2] <- 2*(i-1)
        iter <- iter + 1
        d[iter,3] <- i
        d[iter,1] <- -2*(j-1)
        d[iter,2] <- 2*(i-1)
        iter <- iter + 1
        d[iter,3] <- i
        d[iter,1] <- 2*(j-1)
        d[iter,2] <- -2*(i-1)
        iter <- iter + 1
        d[iter,3] <- i
        d[iter,1] <- -2*(j-1)
        d[iter,2] <- -2*(i-1)
        iter <- iter + 1
        d[iter,3] <- i
      }
    }
    for (k in 1:(i-1)){
      d[iter,1] <- i+k-1
      d[iter,2] <- 2*(i-2) - 2*(k-1)
      iter <- iter + 1
      d[iter,3] <- i
      d[iter,1] <- i+k-1
      d[iter,2] <- -2*(i-2) + 2*(k-1)
      iter <- iter + 1
      d[iter,3] <- i
      d[iter,1] <- -(i+k-1)
      d[iter,2] <- 2*(i-2) - 2*(k-1)
      iter <- iter + 1
      d[iter,3] <- i
      d[iter,1] <- -(i+k-1)
      d[iter,2] <- -2*(i-2) + 2*(k-1)
      iter <- iter + 1
      d[iter,3] <- i
    }
  }
  d <- d[!duplicated(d),]# remove duplicates
  
  if (filter==TRUE){ # skip odd rings in the hexagon grid
    d <- filter(d, ring %in% seq(0,round(num_rings+4),2))
  }
  return(d)
}

# Hexamap chart ---------------------------------------------------------
.hexamaps <- function(couName, num_iter, max_num_neighbors, period, fact){
  
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

  set.seed(456) # reproducitility
  tSNE_plot <- tsne(TCMN_data[,-c(1,ncol(TCMN_data)-1,ncol(TCMN_data))],
                    max_iter=as.numeric(num_iter), 
                    perplexity=as.numeric(max_num_neighbors), 
                    epoch=num_iter)
  # calculate the euclidean distance between the selected country and the rest
  dist_mat <- cbind(tSNE_plot,TCMN_data$Country)
  dist_mat <- as.data.frame(dist_mat, stringsAsFactors=FALSE)
  dist_mat$V1 <- as.numeric(dist_mat$V1)
  dist_mat$V2 <- as.numeric(dist_mat$V2)
  distCou1 <- dist_mat[dist_mat$V3==couName,1]
  distCou2 <- dist_mat[dist_mat$V3==couName,2]
  dist_mat <- mutate(dist_mat, dist = sqrt((V1-distCou1)^2+(V2-distCou2)^2))
  # order by less distance to selected country
  dist_mat <- arrange(dist_mat, dist)[,c(3,4)]
  
  ## compute hexamaps
  # 1. create grid of hexagons
  d <- .hexagon_allocation(14, filter = FALSE) 
  
  # 2. allocate countries in hexagons
  dist <- mutate(dist_mat, allocate = round(fact*dist/max(dist)+2), x=0,y=0)
  dist$allocate[1] <- 0 # selected country uses the central ring
  
  alloc <- 2 # pointer to d
  i <- 2 # pointer to dist
  for (r in 2:max(d$ring)){
    
    if (nrow(dist[dist$allocate==r,]) > table(d$ring)[[r]]){
      new_alloc <- alloc + table(d$ring)[[r]]
      for (j in new_alloc:(new_alloc + nrow(dist[dist$allocate==r,]) - table(d$ring)[[r]] - 1)){
        dist$allocate[j] <- r+1
      }
    } 
    set.seed(789)
    random_k <- sample(table(d$ring)[[r]],
                       size = min(table(d$ring)[[r]],nrow(dist[dist$allocate==r,])))
    index_k <- 1
    for (k in random_k){
      dist$x[i+index_k-1] <- d$x[alloc+k-1]
      dist$y[i+index_k-1] <- d$y[alloc+k-1]
      index_k <- index_k + 1
    }
    alloc <- alloc + table(d$ring)[[r]]
    i <- i + index_k - 1
  }
  # merge dist and d
  z <- cbind(dist,d[1:nrow(dist),])
  names(z)[1] <- "id"
  # create hexamap data.frame
  zz <- hexamap(z)
  regions <- data.frame(id=TCMN_data$Country,region=TCMN_data$Region)
  zz <- merge(zz, regions, by.x="id")
  # use ISO3 codes instead of full country name
  zz <- merge(zz, countries[,c("Country","CountryCodeISO3")], by.x="id",by.y="Country")
  zz$id <- zz$CountryCodeISO3
  # color hexagons by region
  #brk<-levels(as.factor(zz$region))
  #ncol<-length(brk)
  colors <- rainbow(length(unique(TCMN_data$Region)))
  names(colors) <- unique(TCMN_data$Region)
  
  # 3. Plot hexamaps
  p <- ggplot(zz, aes(x, y, group=id)) +
    geom_polygon(aes(fill=region)) +
    coord_fixed(ratio = 1) +
    scale_fill_manual(values = colors) +
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    labs(x="",y="") 
  # Label hexagons
  hexalabel(zz,p)
  
  
}  



# Rtsne chart ---------------------------------------------------------
# .RtSNE_plot <- function(num_iter, max_num_neighbors){
#   
#   require(Rtsne)
#   require(plyr)
#   require(dplyr)
#   require(tidyr)
#   require(data.table)
#   
#   ### read the data
#   training <- fread("/Users/asanchez3/Desktop/Data Analysis/TauMu/training.csv")
#   training <- as.data.frame(training)
#   training$signal <- as.factor(training$signal)
#   set.seed(123)
#   trainReduced <- sample_n(training,250)
#   colors <- rainbow(length(unique(trainReduced$signal)))
#   names(colors) <- unique(trainReduced$signal)
#   
#   tsne_out <- Rtsne(as.matrix(trainReduced[,c(2:48,50)]),
#                     perplexity = max_num_neighbors,
#                     max_iter = num_iter)
#   
#   plot(tsne_out$Y, t='n')
#   graphics::text(tsne_out$Y, col=colors[trainReduced$signal], labels=as.character(trainReduced$signal))
#   
#   
# }  

