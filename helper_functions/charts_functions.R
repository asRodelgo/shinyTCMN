require(dplyr)
require(ggplot2)

# Line chart ---------------------------------------------------------
.ExpImp_HF <- function(couName){
  
  cou <- .getCountryCode(couName)
  TCMN_data <- fread("/Users/asanchez3/Desktop/Work/TCMN/data/TCMN_data.csv")
  TCMN_data <- as.data.frame(TCMN_data)
  
  data <- filter(TCMN_data, CountryCode==cou, Subsection=="chart1")
  
  if (nrow(data)>0){
    ggplot(data, aes(x=Period, y=Observation, group=IndicatorShort)) +
      geom_line(aes(linetype=IndicatorShort,colour=IndicatorShort),size=1.5,stat="identity") +
      scale_linetype_manual(values = c(1,2))+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(lineheight=.5),
            axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(x="",y=""#,title="Goods Export and Import volume growth, 2012-2015"
      ) +
      scale_x_discrete(breaks = unique(data$Period)[seq(1,length(unique(data$Period)),3)])
    
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  }

}  
