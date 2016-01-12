# TCMN projects portfolio and country representatives ----------------------------

# Country projects table ----------------
.projectsTable <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- filter(TCprojects, tolower(WBG_CNTRY_KEY)==couISO2) #select country
  dataTC <- select(dataTC, PROJ_ID, Prod_Line = PROD_LINE_TYPE_NME, Project_Name = PROJ_SHORT_NME,
                           Approval_Date = BD_APPRVL_DATE, Project_Status = PROJECT_STATUS_NME,
                           IBRD_CMT_USD_AMT, GRANT_USD_AMT, IDA_CMT_USD_AMT, Major_Sector = MAJORSECTOR_NAME1,
                           Major_Theme = MAJORTHEME_NAME1)
  # calculate total amount per project
  dataTC <- dataTC %>%
    group_by(PROJ_ID) %>%
    mutate(Project_Amount = IBRD_CMT_USD_AMT + GRANT_USD_AMT + IDA_CMT_USD_AMT,
           Prod_Line = paste("IBRD",Prod_Line)) %>%
    select(-IBRD_CMT_USD_AMT, -GRANT_USD_AMT, -IDA_CMT_USD_AMT) %>%
    filter(Project_Status %in% c("Closed","Active","Pipeline"))
  # remove duplicates
  dataTC <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line))
  
  ### IFC projects ----------
  dataIFC <- filter(IFCprojects, COUNTRY_CODE==cou) #select country
  # projects in active, pipeline or closed status
  dataIFC <- filter(dataIFC, (PROJECT_STAGE=="PIPELINE") | (PROJECT_STATUS %in% c("ACTIVE", "PIPELINE", "CLOSED")))
  dataIFC <- mutate(dataIFC, Prod_Line = "IFC",
                    Project_Status = ifelse(PROJECT_STAGE=="PIPELINE","Pipeline",ifelse(PROJECT_STATUS=="CLOSED","Closed","Active")))
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Prod_Line, Project_Name = PROJECT_NAME,
                   Approval_Date = ASIP_APPROVAL_DATE, Project_Status, Project_Amount = TOTAL_PROJECT_SIZE
                   )
  # remove duplicates
  dataIFC <- dataIFC[!duplicated(dataIFC$PROJ_ID),]
  # projects within 3 fiscal years in the past
  dataIFC <- filter(dataIFC, Approval_Date >= "2013-07-01") #select country
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  
  # Append both
  data <- rbind_list(dataTC, dataIFC)
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  return(data)
}

#############

.projectsStatus <- function(couName, count_type){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- filter(TCprojects, tolower(WBG_CNTRY_KEY)==couISO2) #select country
  dataTC <- select(dataTC, PROJ_ID, Prod_Line = PROD_LINE_TYPE_NME,
                   Project_Status = PROJECT_STATUS_NME,
                   IBRD_CMT_USD_AMT, GRANT_USD_AMT, IDA_CMT_USD_AMT)
  # calculate total amount per project
  dataTC <- dataTC %>%
    group_by(PROJ_ID) %>%
    mutate(Project_Amount = IBRD_CMT_USD_AMT + GRANT_USD_AMT + IDA_CMT_USD_AMT,
           Prod_Line = paste("IBRD",Prod_Line)) %>%
    select(-IBRD_CMT_USD_AMT, -GRANT_USD_AMT, -IDA_CMT_USD_AMT) %>%
    filter(Project_Status %in% c("Closed","Active","Pipeline"))
  # remove duplicates
  dataTC <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line))
  
  ### IFC projects ----------
  dataIFC <- filter(IFCprojects, COUNTRY_CODE==cou) #select country
  # projects in active, pipeline or closed status
  dataIFC <- filter(dataIFC, (PROJECT_STAGE=="PIPELINE") | (PROJECT_STATUS %in% c("ACTIVE", "PIPELINE", "CLOSED")))
  dataIFC <- mutate(dataIFC, Prod_Line = "IFC",
                    Project_Status = ifelse(PROJECT_STAGE=="PIPELINE","Pipeline",ifelse(PROJECT_STATUS=="CLOSED","Closed","Active")))
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Prod_Line, Project_Name = PROJECT_NAME,
                    Approval_Date = ASIP_APPROVAL_DATE, Project_Status, Project_Amount = TOTAL_PROJECT_SIZE
  )
  # remove duplicates
  dataIFC <- dataIFC[!duplicated(dataIFC$PROJ_ID),]
  # projects within 3 fiscal years in the past
  dataIFC <- filter(dataIFC, Approval_Date >= "2013-07-01") #select country
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  
  # Append both
  data <- rbind_list(dataTC, dataIFC)
  
  # generate plot
  if (nrow(data)>0){
    # Faceted chart
    # order the factors
    data$Prod_Line <- as.factor(data$Prod_Line)
    data$Project_Status <- as.factor(data$Project_Status)
    data$Prod_Line <- substr(data$Prod_Line,1,9)
    
    if (count_type=="count"){ # plot number of projects
      
      ggplot(data, aes(x=Project_Status, fill=Project_Status)) +
        geom_bar(stat="bin")+ #stat="identity") +
        facet_wrap(~ Prod_Line)+
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position="top",
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(lineheight=.5)) + 
        labs(x="",y="")
      
    } else{ # plot $ amount of projects
      
      ggplot(data, aes(x=Project_Status, y=Project_Amount, fill=Project_Status)) +
        geom_bar(stat="identity") +
        facet_wrap(~ Prod_Line)+
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position="top",
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(lineheight=.5)) + 
        labs(x="",y="")
    }
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  }
  
}

######################

.projectsSectors <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- filter(TCprojects, tolower(WBG_CNTRY_KEY)==couISO2) #select country
  dataTC <- select(dataTC, PROJ_ID, Prod_Line = PROD_LINE_TYPE_NME, 
                   MAJORSECTOR_NAME1, SECTOR_PCT1, 
                   MAJORSECTOR_NAME2, SECTOR_PCT2, MAJORSECTOR_NAME3, SECTOR_PCT3, MAJORSECTOR_NAME4, SECTOR_PCT4,
                   MAJORSECTOR_NAME5, SECTOR_PCT5)
  # calculate total percentage per sector
  # first, put them in the same column
  dataTC2 <- gather(dataTC, sectorOrder, sectorName, -PROJ_ID,-Prod_Line,-contains("PCT"))
  dataTC2 <- gather(dataTC2, sectorPctOrder, sectorPct, -PROJ_ID,-Prod_Line,-sectorOrder,-sectorName)
  
  dataTC2 <- dataTC2 %>%
    group_by(sectorName) %>%
    mutate(sectorPctTotal = sum(sectorPct,na.rm=TRUE))
  
  # remove duplicates
  dataTC2 <- select(dataTC2, sectorName,sectorPctTotal)
  sectors <- as.data.frame(dataTC2[!duplicated(dataTC2),])
  
  # aggregate sectors
  sectors <- sectors[!duplicated(sectors),]
  sectors <- sectors %>%
    filter(!is.na(sectorName)) %>%
    mutate(sectorPct = sectorPctTotal/sum(sectorPctTotal)) %>%
    arrange(desc(sectorPct))
  
  return(sectors)
  
}

#############

.projectsThemes <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- filter(TCprojects, tolower(WBG_CNTRY_KEY)==couISO2) #select country
  dataTC <- select(dataTC, PROJ_ID, Prod_Line = PROD_LINE_TYPE_NME, 
                   MAJORTHEME_NAME1, THEME_PCT1, 
                   MAJORTHEME_NAME2, THEME_PCT2, MAJORTHEME_NAME3, THEME_PCT3, MAJORTHEME_NAME4, THEME_PCT4,
                   MAJORTHEME_NAME5, THEME_PCT5)
  # calculate total percentage per sector
  # first, put them in the same column
  dataTC2 <- gather(dataTC, themeOrder, themeName, -PROJ_ID,-Prod_Line,-contains("PCT"))
  dataTC2 <- gather(dataTC2, themePctOrder, themePct, -PROJ_ID,-Prod_Line,-themeOrder,-themeName)

  dataTC2 <- dataTC2 %>%
    group_by(themeName) %>%
    mutate(themePctTotal = sum(themePct,na.rm=TRUE))
  
  # remove duplicates
  dataTC2 <- select(dataTC2, themeName,themePctTotal)
  themes <- as.data.frame(dataTC2[!duplicated(dataTC2),])
  
  # aggregate themes
  themes <- themes[!duplicated(themes),]
  themes <- themes %>%
    filter(!is.na(themeName)) %>%
    mutate(themePct = themePctTotal/sum(themePctTotal)) %>%
    arrange(desc(themePct))
  
  return(themes)
  
}

#############

.projectsTreemap <- function(couName, sectTheme){
  
  if (sectTheme=="sector"){
    data <- .projectsSectors(couName) 
  
  } else {
    data <- .projectsThemes(couName) 
  }
  
  if (nrow(data)>0){
    
    names(data) <- c("Name","PctTotal","Pct")
    #data$color <- terrain.colors(length(data$Name)) # add the color
    data$color <- rainbow(length(data$Name)) # add the color
    # format numbers
    data$PctTotal <- format(data$PctTotal, digits=0, decimal.mark=".",
                              big.mark=",",small.mark=".", small.interval=3)
    data$Pct <- format(data$Pct, digits=2, decimal.mark=".",
                                  big.mark=",",small.mark=".", small.interval=3)
    data$Pct <- as.numeric(data$Pct)*100
    
    #data <- select(data, -Period)
    
    treemap(data,
            index=c("Name","Pct"),
            vSize="Pct",
            fontsize.labels=c(24, 24), 
            align.labels=list(c("left", "top"), c("right","bottom")),
            lowerbound.cex.labels=0.5,
            vColor="color",
            type="color",
            title="")
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"Data not available", col="red", cex=2)
  }
  
}

######################

# Country projects table ----------------
.projectsPeople <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- filter(TCprojects, tolower(WBG_CNTRY_KEY)==couISO2) #select country
  dataTC <- select(dataTC, PROJ_ID, Prod_Line = PROD_LINE_TYPE_NME, Project_Name = PROJ_SHORT_NME,
                   Project_Status = PROJECT_STATUS_NME, Staff_Name = FULL_NAME, City = CITY_NAME,
                   position_type, grade, duty_country, practice, emplyment_type)
  # apply filters
  dataTC <- dataTC %>%
    group_by(PROJ_ID) %>%
    mutate(Prod_Line = paste("IBRD",Prod_Line)) %>%
    filter(Project_Status %in% c("Closed","Active","Pipeline"))
  dataTC <- filter(dataTC, practice == "TAC") #, position_type == "PRIMARY"
  dataTC <- select(dataTC, -grade,-duty_country,-practice,-emplyment_type)
  dataTC <- dataTC[!duplicated(dataTC),]
  
  ### IFC projects ----------
  dataIFC <- filter(IFCprojects, COUNTRY_CODE==cou) #select country
  # projects in active, pipeline or closed status
  dataIFC <- filter(dataIFC, (PROJECT_STAGE=="PIPELINE") | (PROJECT_STATUS %in% c("ACTIVE", "PIPELINE", "CLOSED")))
  dataIFC <- mutate(dataIFC, Prod_Line = "IFC",
                    Project_Status = ifelse(PROJECT_STAGE=="PIPELINE","Pipeline",ifelse(PROJECT_STATUS=="CLOSED","Closed","Active")))
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Prod_Line, Project_Name = PROJECT_NAME,
                   Project_Status, Staff_Name = FULL_NAME, City = CITY_NAME,
                   position_type, grade, duty_country, practice, emplyment_type, 
                   Approval_Date = ASIP_APPROVAL_DATE)
  # projects within 3 fiscal years in the past
  dataIFC <- filter(dataIFC, Approval_Date >= "2013-07-01") #select country
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  # apply filters
  dataIFC <- filter(dataIFC, practice == "TAC") #, position_type == "PRIMARY"
  dataIFC <- select(dataIFC, -grade,-duty_country,-practice,-emplyment_type,-Approval_Date)
  dataIFC <- dataIFC[!duplicated(dataIFC),]
  
  # Append both --------------------------
  data <- rbind_list(dataTC, dataIFC)
  # arrange
  data <- arrange(as.data.frame(data), Prod_Line, PROJ_ID, position_type, Staff_Name)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  return(data)
}

#############

# Country projects table ----------------
.projectsStaffStats <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- filter(TCprojects, tolower(WBG_CNTRY_KEY)==couISO2) #select country
  dataTC <- select(dataTC, PROJ_ID, Project_Status = PROJECT_STATUS_NME, Staff_Name = FULL_NAME, City = CITY_NAME,
                   position_type, grade, duty_country, practice, emplyment_type)
  # apply filters
  dataTC <- dataTC %>%
    group_by(PROJ_ID) %>%
    filter(Project_Status %in% c("Closed","Active","Pipeline")) %>%
    filter(practice == "TAC")
    
  dataTC <- dataTC %>%
    group_by(grade) %>%
    mutate(grade_count = length(grade)) %>%
    select(grade,grade_count)
  
  dataTC <- dataTC[!duplicated(dataTC),]
  
  ### IFC projects ----------
  dataIFC <- filter(IFCprojects, COUNTRY_CODE==cou) #select country
  # projects in active, pipeline or closed status
  dataIFC <- filter(dataIFC, (PROJECT_STAGE=="PIPELINE") | (PROJECT_STATUS %in% c("ACTIVE", "PIPELINE", "CLOSED")))
  dataIFC <- mutate(dataIFC, Prod_Line = "IFC",
                    Project_Status = ifelse(PROJECT_STAGE=="PIPELINE","Pipeline",ifelse(PROJECT_STATUS=="CLOSED","Closed","Active")))
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Staff_Name = FULL_NAME, City = CITY_NAME,
                    position_type, grade, duty_country, practice, emplyment_type, 
                    Approval_Date = ASIP_APPROVAL_DATE)
  # projects within 3 fiscal years in the past
  dataIFC <- filter(dataIFC, Approval_Date >= "2013-07-01") #select country
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  # apply filters
  dataIFC <- filter(dataIFC, practice == "TAC") #, position_type == "PRIMARY"
  
  dataIFC <- dataIFC %>%
    group_by(grade) %>%
    mutate(grade_count = length(grade)) %>%
    select(grade,grade_count)
  
  dataIFC <- dataIFC[!duplicated(dataIFC),]
  # Append both --------------------------
  data <- rbind_list(dataTC, dataIFC)
  data <- data %>%
    group_by(grade) %>%
    mutate(grade_count = sum(grade_count))
  data <- data[!duplicated(data),]
  # arrange
  data <- arrange(as.data.frame(data), grade)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  # bar chart
  ggplot(data, aes(x=grade,y=grade_count)) +
    geom_bar(position="dodge",stat="identity") +
    coord_flip() +
    theme(legend.key=element_blank(),
          legend.title=element_blank(),
          legend.position="top",
          panel.border = element_blank(),
          panel.background = element_blank(),plot.title = element_text(lineheight=.5),
          axis.text.y = element_text(size=15)#, axis.text.x = element_blank()
    ) + 
    labs(x="",y="")
  
}
