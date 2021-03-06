# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_All <- function(colRegion,colPeriod,colCountry,colIndicator,centralMeasure="mean",showLabels=FALSE){
  # tsne_points contains pairs of coordinate points to plot
  # Parameters -----------
  # 
  #colPeriod <- "All"
  #colCountry <- "All" 
  #colRegion <- "MENA"
  #colIndicator <- "All" 
  # ----------------------
  #
  # ------------------------------------
  if (colCountry=="All" || is.null(colCountry)) colCountry <- countries_list
  if (colRegion=="All" || is.null(colRegion)) colRegion <- regions_list
  if (colPeriod=="All" || is.null(colPeriod)) colPeriod <- periods_list
  #
  if (length(tsne_ready)>0){ # if data do stuff
    par(mar=c(0,0,0,0))
    
    tsne_ready_plot <- tsne_ready %>% # by default all colored grey
      mutate(color = "lightgrey", colorDots = "grey")
    
    # General Filters
    tsne_points_filter <- tsne_ready_plot %>%
      filter(CountryShort %in% colCountry & RegionShort %in% colRegion 
             & Period %in% colPeriod) %>%
      group_by(CountryShort,Period) %>%
      mutate(group = ifelse(length(colRegion)>2,
                        ifelse(length(colPeriod) == 2,
                               ifelse(length(colCountry)>2,Period,paste0(CountryShort," (",Period,")")),
                               ifelse(length(colCountry)>2,RegionShort,
                                      ifelse(length(colPeriod)==1,paste0(CountryShort," (",Period,")"),CountryShort))),
                     ifelse(length(colPeriod)>2,ifelse(length(colCountry)>2,RegionShort,CountryShort),
                            ifelse(length(colCountry)>2,paste0(RegionShort," (",Period,")"),paste0(CountryShort," (",Period,")")))))
    
    
    centroid <- data.frame(x=(mean(tsne_points_filter$x)),y=mean(tsne_points_filter$y))
    
    tsne_points_filter_out <- tsne_ready_plot %>%
      filter(!(CountryShort %in% colCountry & RegionShort %in% colRegion 
               & Period %in% colPeriod))
    # Skills filter
    if (!(colIndicator=="All")){
      
      if (centralMeasure=="mean"){
        ggplot(NULL, aes(x,y)) +
          geom_point(data=tsne_points_filter,aes(color = eval(parse(text=colIndicator))),size=2) +
          scale_color_gradient2(midpoint=mean(eval(parse(text=paste0("tsne_points_filter$",colIndicator)))), low="blue", mid="white",high="red")+
          geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) + 
          theme(legend.key=element_blank(),
                legend.title=element_blank(),
                #legend.text = element_blank(),
                legend.position = "top",
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank())
      } else{
        ggplot(NULL, aes(x,y)) +
          geom_point(data=tsne_points_filter,aes(color = eval(parse(text=colIndicator))),size=2) +
          scale_color_gradient2(midpoint=median(eval(parse(text=paste0("tsne_points_filter$",colIndicator)))), low="blue", mid="white",high="red")+
          geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) + 
          theme(legend.key=element_blank(),
                legend.title=element_blank(),
                #legend.text = element_blank(),
                legend.position = "top",
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank())
      }
      
    } else {
      
      if (showLabels){ # show names and year of countries
        ggplot(NULL, aes(x,y)) +  
          #geom_point(data=tsne_points_filter,aes(group=CountryShort,color = CountryShort),size=2) +
          geom_point(data=tsne_points_filter,aes(group=group,color = group),size=2) +
          geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) +
          geom_point(data=centroid,color="red",size=3) + 
          geom_text(data=tsne_points_filter,aes(label=str_wrap(paste0(CountryShort," (",Period,")"))),color="grey",nudge_y=0.1)+
          theme(legend.key=element_blank(),
                legend.title=element_blank(),
                #legend.text = element_blank(),
                legend.position = "top",
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank())
      } else {
        ggplot(NULL, aes(x,y)) +  
          geom_point(data=tsne_points_filter,aes(group=group,color = group),size=2) +
          geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) +
          geom_point(data=centroid,color="red",size=3) + 
          theme(legend.key=element_blank(),
                legend.title=element_blank(),
                #legend.text = element_blank(),
                legend.position = "top",
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.text.x = element_blank(),
                axis.text.y = element_blank(),
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                axis.ticks = element_blank())
      }
    }
    
    #plot(tsne_points,type = "p", pch = 19, axes=FALSE, frame.plot = FALSE, xlab = "",ylab = "",col = tsne_ready$colorDots); 
    #    graphics::text(tsne_points_filter[,c("x","y")],
    #                   labels=paste0(as.character(tsne_points_filter$Player)," (",tsne_points_filter$Season,")"),
    #                   col=tsne_points_filter$color)
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  }
  
}

# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_filter <- function(colRegion,colPeriod,colCountry,selected_indicators){
  #
  if (colCountry=="All" || is.null(colCountry)) colCountry <- countries_list
  if (colRegion=="All" || is.null(colRegion)) colRegion <- regions_list
  if (colPeriod=="All" || is.null(colPeriod)) colPeriod <- periods_list
    
    
    if (length(tsne_ready)>0){ # if data do stuff
      if (!is.null(selected_indicators)){  # if at least 1 selected indicator
        tsne_ready_select <- tsne_ready %>%
            dplyr::select(CountryCode, Period, RegionShort, 
                          RegionShortIncome, CountryShort, x, y, 
                          one_of(selected_indicators))
      } else { # no selected indicators    
        tsne_ready_select <- tsne_ready %>%
          dplyr::select(CountryCode, Period, RegionShort, 
                        RegionShortIncome, CountryShort, x, y)
      }  
      # General Filters
      tsne_points_filter <- tsne_ready_select %>%
        filter(CountryShort %in% colCountry & RegionShort %in% colRegion & 
                 Period %in% colPeriod) %>%
        group_by(CountryShort,Period) %>%
        mutate(group = ifelse(length(colRegion)>2,
                              ifelse(length(colPeriod) == 2,
                                     ifelse(length(colCountry)>2,Period,paste0(CountryShort," (",Period,")")),
                                     ifelse(length(colCountry)>2,RegionShort,
                                            ifelse(length(colPeriod)==1,paste0(CountryShort," (",Period,")"),CountryShort))),
                              ifelse(length(colPeriod)>2,ifelse(length(colCountry)>2,RegionShort,CountryShort),
                                     ifelse(length(colCountry)>2,paste0(RegionShort," (",Period,")"),paste0(CountryShort," (",Period,")")))))
      tsne_points_filter_out <- tsne_ready_select %>%
        filter(!(CountryShort %in% colCountry & RegionShort %in% colRegion & Period %in% colPeriod))
      
    } else{ return()}
    #plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    #graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  
  return(tsne_points_filter)
}

# Filters for hover over tooltips ---------------------------------------------------------
.tSNE_plot_filter_hover <- function(colRegion,colPeriod,colCountry,selected_indicators){
  #
  if (colCountry=="All" || is.null(colCountry)) colCountry <- countries_list
  if (colRegion=="All" || is.null(colRegion)) colRegion <- regions_list
  if (colPeriod=="All" || is.null(colPeriod)) colPeriod <- periods_list
  
  if (length(tsne_ready)>0){ # if data do stuff
    tsne_ready_select <- tsne_ready %>%
      dplyr::select(CountryCode, Period, RegionShort, 
                    RegionShortIncome, CountryShort, x, y, 
                    one_of(selected_indicators))
    
    # General Filters
    tsne_points_filter <- tsne_ready_select %>%
      filter(CountryShort %in% colCountry & RegionShort %in% colRegion & Period %in% colPeriod)
    tsne_points_filter_out <- tsne_ready_select %>%
      filter(!(CountryShort %in% colCountry & RegionShort %in% colRegion & Period %in% colPeriod))
    
  } else{ return()}
  #plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
  #graphics::text(1.5, 1,"Not enough data", col="red", cex=2)
  
  return(tsne_points_filter)
}


# Density plots
.densityPlots <- function(colRegion,colPeriod,colCountry,colIndicator,clickCountry,clickPeriod,selected_indicators){
  
  tsne_points_filter <- .tSNE_plot_filter(colRegion,colPeriod,colCountry,colIndicator)
  tsne_points_filter <- gather(tsne_points_filter, indicator, value, -CountryCode,-CountryShort,-RegionShortIncome,-RegionShort,-Period,-x,-y)
  tsne_ready_gather <- gather(tsne_ready, indicator, value, -CountryCode,-CountryShort,-RegionShortIncome,-RegionShort,-Period,-x,-y)
  
  tsne_points_filter <- filter(tsne_points_filter, indicator %in% selected_indicators)
  tsne_ready_gather <- filter(tsne_ready_gather, indicator %in% selected_indicators)
  
  if (is.null(clickCountry)){
    
    ggplot(data=tsne_ready_gather,aes(value)) + 
      geom_density(data=tsne_ready_gather,aes(y=..density..),alpha=.8, fill="grey") +  
      geom_histogram(data=tsne_points_filter,aes(y=..density..),alpha=.6, fill="lightblue") +  
      facet_wrap(~indicator, nrow=1, scales="free_x") +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(lineheight=.5),
            #axis.text.x = element_blank(),
            #axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
            #axis.ticks = element_blank()
      )
    
  } else {
    
    verticalLine <- tsne_points_filter %>%
      filter(CountryShort == clickCountry, Period == clickPeriod) %>%
      dplyr::select(indicator, value)
    
    ggplot(data=tsne_ready_gather,aes(value)) + 
      geom_density(data=tsne_ready_gather,aes(y=..density..),alpha=.8, fill="grey") +  
      geom_histogram(data=tsne_points_filter,aes(y=..density..),alpha=.6, fill="lightblue") +  
      facet_wrap(~indicator, nrow=1, scales="free_x") +
      geom_vline(data=verticalLine, aes(xintercept = value), colour="red", size = 1) +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(lineheight=.5),
            #axis.text.x = element_blank(),
            #axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
            #axis.ticks = element_blank()
      )
    
  }
  
}

.radarPlot <- function(brushPoints,selected_indicators){
  
  tsne_radar <- tsne_ready %>%
    dplyr::select(one_of(selected_indicators), CountryShort, Period) %>%
    mutate_at(selected_indicators, funs(max,mean)) %>%
    #filter(Season == colPeriod) %>%
    dplyr::select(-Period)
  
  #brushPoints <- filter(tsne_ready, Tm == "CHI")
  brushPoints <- as.data.frame(brushPoints)
  
  if (nrow(brushPoints)>0){
    #brushPoints <- merge(tsne_ready, brushPoints, by = c("Player","Season"))
    tsne_mean <- brushPoints %>%
      dplyr::select(one_of(selected_indicators), CountryShort, Period) %>%
      mutate_at(selected_indicators, funs(mean)) %>%
      #dplyr::select(ends_with("_mean")) %>%
      mutate(CountryShort = "mean of selected") %>%
      distinct(.keep_all=TRUE) %>%
      dplyr::select(CountryShort, everything())
    
    names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
    
  } else {
    tsne_mean <- tsne_radar %>%
      dplyr::select(ends_with("_mean")) %>%
      distinct(.keep_all=TRUE) %>%
      mutate(CountryShort = "mean of selected") %>%
      dplyr::select(CountryShort, everything())
    
    names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
  }
  
  tsne_max <- tsne_radar %>%
    dplyr::select(ends_with("_max")) %>%
    distinct(.keep_all=TRUE) %>%
    mutate(CountryShort = "max") %>%
    dplyr::select(CountryShort, everything())
  
  names(tsne_max) <- gsub("_max","",names(tsne_max))
  
  tsne_radar <- bind_rows(tsne_mean,tsne_max)
  tsne_radar <- dplyr::select(tsne_radar, CountryShort, one_of(selected_indicators))
  # shorter names to display
  names(tsne_radar) <- c("CountryShort",indicator_selection_plots_short)
  #ez.radarmap(df, "model", stats="mean", lwd=1, angle=0, fontsize=0.6, facet=T, facetfontsize=1, color=id, linetype=NULL)
  ez.radarmap(tsne_radar, "CountryShort", stats="none", lwd=1, angle=0, fontsize=1.5, facet=F, facetfontsize=1, color=id, linetype=NULL) +
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position = "bottom",
          #panel.border = element_blank(),
          #panel.background = element_blank(),
          plot.title = element_text(lineheight=.5),
          #axis.text.x = element_blank(),
          #axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank()
          #axis.ticks = element_blank()
    )  
  
}

.radarPlot_base <- function(brushPoints,selected_indicators){
  
  tsne_ready_select <- as.data.frame(.tSNE_plot_filter(colRegion,colPeriod,colCountry,selected_indicators))
  
  if (length(selected_indicators)>1){
    tsne_radar <- tsne_ready_select %>%
      group_by(group) %>%
      dplyr::select(one_of(selected_indicators), CountryShort, Period,group) %>%
      mutate_at(selected_indicators, funs(max,mean)) %>%
      #filter(Season == colPeriod) %>%
      dplyr::select(-Period)
  } else { # introduce fictitious variable x to keep the _mean, _max structure when 
    # only 1 indicator is selected
    tsne_radar <- tsne_ready_select %>%
      dplyr::select(one_of(selected_indicators),x, CountryShort, Period,group) %>%
      mutate_at(c(selected_indicators,"x"), funs(mean)) #%>%
    #filter(Season == colPeriod) %>%
    dplyr::select(-Period)
  }
  #brushPoints <- filter(tsne_ready, CountryCode == "ALB")
  brushPoints <- as.data.frame(brushPoints)
  
  if (nrow(brushPoints)>0){
    #brushPoints <- merge(tsne_ready, brushPoints, by = c("Player","Season"))
    tsne_mean <- brushPoints %>%
      dplyr::select(one_of(selected_indicators), CountryShort, Period,RegionShort) %>%
      mutate_at(selected_indicators, funs(mean)) %>%
      #dplyr::select(ends_with("_mean")) %>%
      mutate(group = "mean of selected") %>%
      distinct(group, .keep_all=TRUE) %>%
      dplyr::select(group, everything())
    
    names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
    
  } else {
    
    tsne_mean <- tsne_radar %>%
      group_by(group) %>%
      dplyr::select(ends_with("_mean"),group) %>%
      distinct(.keep_all=TRUE) %>%
      #mutate(CountryShort = "mean of selected") %>%
      dplyr::select(group, everything())
    
    names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
    
  }
  
  tsne_max <- tsne_ready %>%
    #group_by(group) %>%
    dplyr::select(one_of(selected_indicators)) %>%
    mutate_at(selected_indicators, funs(max)) %>%
    #dplyr::select(-Period) %>%
    #dplyr::select(ends_with("_max")) %>%
    mutate(group = "max") %>%
    distinct(.keep_all=TRUE) %>%
    dplyr::select(group,everything())
  
  #names(tsne_max) <- gsub("_max","",names(tsne_max))
  
  tsne_radar <- bind_rows(tsne_mean,tsne_max)
  tsne_radar <- dplyr::select(tsne_radar, group, one_of(selected_indicators))
  
  require(stringr) # to wrap label text
  names(tsne_radar) <- gsub("_"," ",names(tsne_radar))
  names(tsne_radar) <- str_wrap(names(tsne_radar), width = 20)  
  
  # add the min column and transpose
  tsne_radar <- t(tsne_radar)
  tsne_radar_labels <- row.names(tsne_radar)[-1]
  tsne_radar_groups <- tsne_radar[1,]
  tsne_radar <- as.data.frame(tsne_radar)
  tsne_radar <- mutate_all(tsne_radar, funs(as.numeric(as.character(.))))
  tsne_radar <- tsne_radar %>%
    mutate(Observation = V1*10, min = 1, max = 10) %>%
    dplyr::select(max, min, Observation)
  tsne_radar <- as.data.frame(t(tsne_radar))
  # plot
  radarchart(tsne_radar, axistype=1, caxislabels=c(" "," ",".5"," "," "), centerzero = FALSE,seg=4,
             plty=c(1),plwd=c(5),pcol=c("green"),pdensity=c(0),
             cglwd=2,axislabcol="red", vlabels=tsne_radar_labels, cex.main=1,cex=2.5)  
  
}


.brushTable <- function(brushPoints,selected_indicators){
    
  if (!is.null(selected_indicators)){  
    # brushed points
    brushPoints <- dplyr::select(brushPoints,Country=CountryShort, CountryCode, Period, one_of(selected_indicators))
    #names(brushPoints) <- c("Country","Period",indicator_selection_plots_short)
    # actual data filter
    selected_TCMN_data <- .filter_TCMN_data()
    # merge
    brushPoints_actual <- merge(selected_TCMN_data,brushPoints[,c("Country","Period")], 
                                by.x = c("CountryShort","Period"), by.y = c("Country","Period"))
    brushPoints_actual <- brushPoints_actual %>%
      dplyr::select(Country=CountryShort, CountryCode,Period, one_of(selected_indicators)) %>%
      mutate(Country = paste0('<a href=',country_url,CountryCode,' target="_blank" >',Country,'</a>')) %>%
      dplyr::select(-CountryCode)
    
    require(stringr) # to wrap label text
    names(brushPoints_actual) <- gsub("_"," ",names(brushPoints_actual))
    names(brushPoints_actual) <- str_wrap(names(brushPoints_actual), width = 25)  
    
    #names(brushPoints_actual) <- c("Country","Period",indicator_selection_plots_short)
    brushPoints_actual[,c(3:ncol(brushPoints_actual))] <- round(brushPoints_actual[,c(3:ncol(brushPoints_actual))],2)
    brushPoints_actual[is.na(brushPoints_actual)] <- "..."
    
    ## Testing hrefs for indicators
#     count_rows <- nrow(brushPoints_actual)
#     thisRow <- 1
#     while (thisRow <= count_rows){
#       brushPoints_actual[thisRow,3] <- paste0('<a href=',indicator_url,'corr.scr',' target="_blank" >',brushPoints_actual[thisRow,3],'</a>')
#       thisRow <- thisRow + 1
#     }
    ## end of testing
    
      #return(str(brushPoints))
    brushPoints <- brushPoints_actual
  } else {
    brushPoints <- dplyr::select(brushPoints,Country=CountryShort, Period)
  }  
  return(brushPoints)  
}

# Bar chart plot
.bar_chart <- function(brushPoints,colRegion,colPeriod,colCountry,selected_indicators){      
  
  if (!(is.null(selected_indicators))){
    
    tsne_ready_select <- as.data.frame(.tSNE_plot_filter(colRegion,colPeriod,colCountry,selected_indicators))
    
    if (length(selected_indicators)>1){
      tsne_radar <- tsne_ready_select %>%
        group_by(group) %>%
        dplyr::select(one_of(selected_indicators), CountryShort, Period,group) %>%
        mutate_at(selected_indicators, funs(max,mean)) %>%
        #filter(Season == colPeriod) %>%
        dplyr::select(-Period)
    } else { # introduce fictitious variable x to keep the _mean, _max structure when 
      # only 1 indicator is selected
      tsne_radar <- tsne_ready_select %>%
        dplyr::select(one_of(selected_indicators),x, CountryShort, Period,group) %>%
        mutate_at(c(selected_indicators,"x"), funs(mean)) #%>%
        #filter(Season == colPeriod) %>%
        dplyr::select(-Period)
    }
    #brushPoints <- filter(tsne_ready, CountryCode == "ALB")
    brushPoints <- as.data.frame(brushPoints)
    
    if (nrow(brushPoints)>0){
      #brushPoints <- merge(tsne_ready, brushPoints, by = c("Player","Season"))
      tsne_mean <- brushPoints %>%
        dplyr::select(one_of(selected_indicators), CountryShort, Period,RegionShort) %>%
        mutate_at(selected_indicators, funs(mean)) %>%
        #dplyr::select(ends_with("_mean")) %>%
        mutate(group = "mean of selected") %>%
        distinct(group, .keep_all=TRUE) %>%
        dplyr::select(group, everything())
      
      names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
      
    } else {
  
        tsne_mean <- tsne_radar %>%
          group_by(group) %>%
          dplyr::select(ends_with("_mean"),group) %>%
          distinct(.keep_all=TRUE) %>%
          #mutate(CountryShort = "mean of selected") %>%
          dplyr::select(group, everything())
        
        names(tsne_mean) <- gsub("_mean","",names(tsne_mean))
    
    }
      
    tsne_max <- tsne_ready %>%
      #group_by(group) %>%
      dplyr::select(one_of(selected_indicators)) %>%
      mutate_at(selected_indicators, funs(max)) %>%
      #dplyr::select(-Period) %>%
      #dplyr::select(ends_with("_max")) %>%
      mutate(group = "max") %>%
      distinct(.keep_all=TRUE) %>%
      dplyr::select(group,everything())
    
    #names(tsne_max) <- gsub("_max","",names(tsne_max))
    
    tsne_radar <- bind_rows(tsne_mean,tsne_max)
    tsne_radar <- dplyr::select(tsne_radar, group, one_of(selected_indicators))
    
    require(stringr) # to wrap label text
    names(tsne_radar) <- gsub("_"," ",names(tsne_radar))
    names(tsne_radar) <- str_wrap(names(tsne_radar), width = 20)  
    
    tsne_barchart <- gather(tsne_radar,Indicator,Observation,-group)
      
    data_color <- filter(tsne_barchart,!(group=="max"))
    data_color$group <- str_wrap(data_color$group,width=10)
    data_grey <- filter(tsne_barchart,group=="max")
    
    #data <- mutate(data, id = seq(1,nrow(data),1))
    ggplot(NULL,aes(x=Indicator,y=Observation)) +
      geom_bar(data=data_grey,color="#f1f3f3",fill = "#f1f3f3",stat="identity") +
      geom_bar(data=data_color,aes(color=group,fill=group),stat="identity",position = "dodge") +
      geom_text(data=data_color, aes(label=round(Observation,2),y=Observation + .05,group=group),
                size=4,color="darkblue",position = position_dodge(width = .9)) + 
      coord_flip()+
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.position="top",
            legend.key.size = unit(0.5, "cm"),
            panel.border = element_blank(),
            panel.background = element_blank(),plot.title = element_text(lineheight=.5),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 12)) + 
      labs(x="",y=""#,title="Top 5 constraints according to 2013 Enterprise Survey (in percent)"
      )
  } else{
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"No indicators selected", col="red", cex=1)
  }
}

# Box plots
.boxPlots <- function(brushPoints,colRegion,colPeriod,colCountry,selected_indicators,clickCountry,clickPeriod){      
  
  #set.seed(123) # to fix the jitter across different plots
  # cloud of points on top of boxplots
  tsne_points_filter <- as.data.frame(.tSNE_plot_filter(colRegion,colPeriod,colCountry,selected_indicators))
  tsne_points_filter <- gather(tsne_points_filter, indicator, value, -CountryCode,-CountryShort,-RegionShortIncome,-RegionShort,-Period,-x,-y,-group)
  tsne_points_filter$indicator <- gsub("_"," ",tsne_points_filter$indicator)
  tsne_points_filter$indicator <- str_wrap(tsne_points_filter$indicator, width = 20)  
  tsne_points_filter$group <- str_wrap(tsne_points_filter$group,width=12)
  
  # boxplots
  tsne_ready_gather <- gather(tsne_ready, indicator, value, -CountryCode,-CountryShort,-RegionShortIncome,-RegionShort,-Period,-x,-y)
  #tsne_points_filter <- filter(tsne_points_filter, indicator %in% selected_indicators)
  tsne_ready_gather <- filter(tsne_ready_gather, indicator %in% selected_indicators)
  tsne_ready_gather$indicator <- gsub("_"," ",tsne_ready_gather$indicator)
  tsne_ready_gather$indicator <- str_wrap(tsne_ready_gather$indicator, width = 20)
  extremes_high <- tsne_ready_gather %>%
    group_by(indicator) %>%
    filter(value==max(value)) %>%
    distinct(value,.keep_all=TRUE)
  extremes_low <- tsne_ready_gather %>%
    group_by(indicator) %>%
    filter(value==min(value)) %>%
    distinct(value,.keep_all=TRUE)
  #tsne_ready_gather$group <- str_wrap(tsne_ready_gather$group,width=10)
  
  #plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
  ##graphics::text(1.5, 1,nrow(brushPoints), col="red", cex=1)
  #graphics::text(1.5, 0.8,paste(clickCountry,"Ubabu"), col="red", cex=1)
  
  if (paste0(clickPeriod," ")==" "){ #no click
    
    brushPoints <- as.data.frame(brushPoints)
    
    if (nrow(brushPoints)>0){ #brush
      
      brushPoints <- dplyr::select(brushPoints,group,one_of(selected_indicators))
      brushPoints <- gather(brushPoints, indicator, value, -group)
      brushPoints$indicator <- gsub("_"," ",brushPoints$indicator)
      brushPoints$indicator <- str_wrap(brushPoints$indicator, width = 20)
      brushPoints$group <- str_wrap(brushPoints$group,width=12)
      
      ggplot(data=tsne_ready_gather,aes(indicator,value)) + 
        geom_boxplot(color="darkgrey") +  
        geom_jitter(data=tsne_points_filter,aes(group=group,color=group),alpha=0.01,width=0.7) +  
        geom_jitter(data=brushPoints,aes(group=group,color=group),width=0.7) +  
        geom_text(data=extremes_high,aes(label=str_wrap(paste0(CountryShort," (",Period,")"),width=12)),color="darkgrey",size=3,nudge_x = 0.35,nudge_y=-0.05,show.legend = FALSE) +
        geom_text(data=extremes_low,aes(label=str_wrap(paste0(CountryShort," (",Period,")"),width=12)),color="darkgrey",size=3,nudge_x = 0.35,nudge_y=0.05,show.legend = FALSE) +           
        coord_flip() +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position="top",
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(lineheight=.5),
              #axis.text.x = element_blank(),
              #axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
              #axis.ticks = element_blank()
              )
    } else{ # no brush, no click
      
      ggplot(data=tsne_ready_gather,aes(indicator,value)) + 
        geom_boxplot(color="darkgrey") +  
        geom_jitter(data=tsne_points_filter,aes(group=group,color=group),width=0.7) +
        geom_text(data=extremes_high,aes(label=str_wrap(paste0(CountryShort," (",Period,")"),width=12)),color="darkgrey",size=3,nudge_x = 0.35,nudge_y=-0.05,show.legend = FALSE) +
        geom_text(data=extremes_low,aes(label=str_wrap(paste0(CountryShort," (",Period,")"),width=12)),color="darkgrey",size=3,nudge_x = 0.35,nudge_y=0.05,show.legend = FALSE) +      
        coord_flip() +
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position="top",
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(lineheight=.5),
              #axis.text.x = element_blank(),
              #axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank()
              #axis.ticks = element_blank()
        )
      
    }  
    
  } else { #click
    
    selectedPoint <- tsne_points_filter %>%
      filter(CountryShort == clickCountry, Period == clickPeriod) %>%
      dplyr::select(CountryShort,Period,indicator, value)
    
    ggplot(data=tsne_ready_gather,aes(indicator,value)) + 
      geom_boxplot(color="darkgrey") +  
      geom_jitter(data=tsne_points_filter,aes(group=group,color=group),alpha=0.5,width=0.7) + 
      geom_point(data=selectedPoint,aes(fill=paste0(CountryShort," (",Period,")")),color="blue",size=4) +
      geom_text(data=extremes_high,aes(label=str_wrap(paste0(CountryShort," (",Period,")"),width=12)),color="darkgrey",size=3,nudge_x = 0.35,nudge_y=-0.05,show.legend = FALSE) +
      geom_text(data=extremes_low,aes(label=str_wrap(paste0(CountryShort," (",Period,")"),width=12)),color="darkgrey",size=3,nudge_x = 0.35,nudge_y=0.05,show.legend = FALSE) +      
      coord_flip() +
      theme(legend.key=element_blank(),
            legend.title=element_blank(),
            legend.position="top",
            panel.border = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(lineheight=.5),
            #axis.text.x = element_blank(),
            #axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank()
            #axis.ticks = element_blank()
      )
    
  }
  
  
}
