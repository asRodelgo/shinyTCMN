# tsne chart ---------------------------------------------------------
.tSNE_plot <- function(num_iter, max_num_neighbors){
  
  require(tsne)
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
  ecb2 <- function(x,y){ plot(x,t='n'); text(x,labels=trainReduced$signal, col=colors[trainReduced$signal]) }
  tsne_TauMu <- tsne(trainReduced[,c(2:48,50)], epoch_callback = ecb2, max_iter=as.numeric(num_iter), 
                                 perplexity=as.numeric(max_num_neighbors), epoch=10)
  
}  
