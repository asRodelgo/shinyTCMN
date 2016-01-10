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
