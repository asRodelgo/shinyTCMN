require(dplyr)
require(ggplot2)

# Line chart ---------------------------------------------------------
.ExpImp_HF <- function(cou){
  
  data3 <- read.csv("/Users/asanchez3/Desktop/Work/TCMN/TCMN_ExpImpHF.csv")
  data <- select(data3, Indicator, Year, Tunisia)
  data$Year <- as.character(data$Year)
  
  ggplot(data, aes(x=Year, y=Tunisia, group=Indicator)) +
    geom_line(aes(linetype=Indicator,colour=Indicator),size=1.5,stat="identity") +
    scale_linetype_manual(values = c(1,2))+
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank(),plot.title = element_text(lineheight=.5),
          axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(x="",y=""#,title="Goods Export and Import volume growth, 2012-2015"
    ) + 
    scale_x_discrete(breaks = unique(data$Year)[seq(1,length(unique(data$Year)),3)]) 
}  
