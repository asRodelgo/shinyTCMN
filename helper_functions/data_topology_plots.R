# Plot tsne chart ---------------------------------------------------------
.tSNE_plot_All <- function(colRegion,colPeriod,colCountry,colIndicator){
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
  if (colCountry=="All") colCountry <- countries_list
  if (colRegion=="All") colRegion <- regions_list
  if (colPeriod=="All") colPeriod <- periods_list
  #
  if (length(tsne_ready)>0){ # if data do stuff
    par(mar=c(0,0,0,0))
    
    tsne_ready_plot <- tsne_ready %>% # by default all colored grey
      mutate(color = "lightgrey", colorDots = "grey")
    
    # General Filters
    tsne_points_filter <- tsne_ready_plot %>%
      filter(CountryShort %in% colCountry & RegionShort %in% colRegion 
             & Period %in% colPeriod)
    centroid <- data.frame(x=(mean(tsne_points_filter$x)),y=mean(tsne_points_filter$y))
    
    tsne_points_filter_out <- tsne_ready_plot %>%
      filter(!(CountryShort %in% colCountry & RegionShort %in% colRegion 
               & Period %in% colPeriod))
    # Skills filter
    if (!(colIndicator=="All")){
      
      ggplot(NULL, aes(x,y)) +
        geom_point(data=tsne_points_filter,aes(color = eval(parse(text=colIndicator))),size=2) +
        scale_color_gradient2(midpoint=mean(eval(parse(text=paste0("tsne_points_filter$",colIndicator)))), low="blue", mid="white",high="red")+
        geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) + 
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.text = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank())
      
    } else {
      
      ggplot(NULL, aes(x,y)) +  
        geom_point(data=tsne_points_filter,color = "blue",size=2) +
        geom_point(data=tsne_points_filter_out,color=alpha("lightgrey",0.1)) +
        geom_point(data=centroid,color="red",size=3) + 
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.text = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              axis.ticks = element_blank())
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
.tSNE_plot_filter <- function(colRegion,colPeriod,colCountry,colIndicator){
  #
  if (colCountry=="All") colCountry <- countries_list
  if (colRegion=="All") colRegion <- regions_list
  if (colPeriod=="All") colPeriod <- periods_list
  
  if (length(tsne_ready)>0){ # if data do stuff
    tsne_ready_select <- dplyr::select(tsne_ready, CountryCode, Period, RegionShort, 
                                       RegionShortIncome, CountryShort, x, y, one_of(indicator_selection_plots))
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
.densityPlots <- function(colRegion,colPeriod,colCountry,colIndicator,clickCountry,clickPeriod){
  
  tsne_points_filter <- .tSNE_plot_filter(colRegion,colPeriod,colCountry,colIndicator)
  tsne_points_filter <- gather(tsne_points_filter, indicator, value, -CountryCode,-CountryShort,-RegionShortIncome,-RegionShort,-Period,-x,-y)
  tsne_ready_gather <- gather(tsne_ready, indicator, value, -CountryCode,-CountryShort,-RegionShortIncome,-RegionShort,-Period,-x,-y)
  
  tsne_points_filter <- filter(tsne_points_filter, indicator %in% indicator_selection_plots)
  tsne_ready_gather <- filter(tsne_ready_gather, indicator %in% indicator_selection_plots)
  
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

.radarPlot <- function(brushPoints){#,Off_Deff="All"){
  
  #   if (Off_Deff == "Offense"){
  #     list_skills <- c("effPTS","eff2PM","eff2PA","eff3PM","eff3PA","effFTA","effFTM","effAST")
  #   } else if (Off_Deff == "Defense"){
  #     list_skills <- c("effBLK","effDRB","effORB","effSTL","effTOV","effPF")
  #   } else {
  list_indicators <- c()
  #     }
  
  tsne_radar <- tsne_ready %>%
    dplyr::select(one_of(indicator_selection_plots), CountryShort, Period) %>%
    mutate_at(indicator_selection_plots, funs(max,mean)) %>%
    #filter(Season == colPeriod) %>%
    dplyr::select(-Period)
  
  #brushPoints <- filter(tsne_ready, Tm == "CHI")
  brushPoints <- as.data.frame(brushPoints)
  
  if (nrow(brushPoints)>0){
    #brushPoints <- merge(tsne_ready, brushPoints, by = c("Player","Season"))
    tsne_mean <- brushPoints %>%
      dplyr::select(one_of(indicator_selection_plots), CountryShort, Period) %>%
      mutate_at(indicator_selection_plots, funs(mean)) %>%
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
  tsne_radar <- dplyr::select(tsne_radar, CountryShort, one_of(indicator_selection_plots))
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

.brushTable <- function(brushPoints){
  
  brushPoints <- dplyr::select(brushPoints,Country=CountryShort, Period, one_of(indicator_selection_plots))
  #return(str(brushPoints))
}