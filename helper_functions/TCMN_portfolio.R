# TCMN projects portfolio and country representatives ----------------------------

# filter IBRD T&C relevant projects ---------------
.filterTCProjects <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  dataTC <- filter(TCprojects, tolower(WBG_CNTRY_KEY)==couISO2) #select country
  # calculate total amount per project
  dataTC <- dataTC %>%
    group_by(PROJ_ID) %>%
    mutate(Project_Amount = (IBRD_CMT_USD_AMT + GRANT_USD_AMT + IDA_CMT_USD_AMT)/1000000,
           Prod_Line = ifelse(tolower(substr(PROD_LINE_TYPE_NME,1,4))=="lend","Financing",
                              ifelse(tolower(substr(PROD_LINE_TYPE_NME,1,3))=="aaa",
                                     "Advisory Services and Analytics (ASA) IBRD",PROD_LINE_TYPE_NME)),
           ProjectOrder = ifelse(PROJECT_STATUS_NME=="Active",1,ifelse(PROJECT_STATUS_NME=="Pipeline",2,3)),
           url = paste0("http://operationsportal2.worldbank.org/wb/opsportal/ttw/about?projId=",PROJ_ID),
           RAS = ifelse(is.na(FEE_BASED_FLAG),"N","Y")) %>%
    select(-IBRD_CMT_USD_AMT, -GRANT_USD_AMT, -IDA_CMT_USD_AMT) %>%
    filter(PROJECT_STATUS_NME %in% c("Closed","Active","Pipeline")) #%>%
    #filter(sequence == max(sequence) & rate_code == "ORR") # latest SORT
    #filter(!(tolower(substr(Prod_Line,1,8))=="standard"))
  
  return(dataTC)
}

# filter IFC T&C relevant projects ---------------
.filterIFCProjects <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  dataIFC <- filter(IFCprojects, COUNTRY_CODE==cou) #select country
  # projects in active, pipeline or closed status
  dataIFC <- filter(dataIFC, (PROJECT_STAGE %in% c("PIPELINE","PORTFOLIO")) | (PROJECT_STATUS %in% c("ACTIVE", "HOLD", "CLOSED")),
                    PROJECT_TYPE == "AS PROJECTS WITH CLIENT(S)")
  dataIFC <- mutate(dataIFC, Prod_Line = "Advisory Services and Analytics (ASA) IFC",
                    Project_Status = ifelse(PROJECT_STAGE=="PIPELINE","Pipeline",ifelse(PROJECT_STATUS=="CLOSED","Closed","Active")))
  dataIFC <- mutate(dataIFC, ProjectOrder = ifelse(Project_Status=="Active",1,ifelse(Project_Status=="Pipeline",2,3)),
                    url = paste0("http://ifcext.ifc.org/ifcext/spiwebsite1.nsf/%20AllDocsAdvisory?SearchView&Query=(FIELD ProjectId=",PROJ_ID))
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  
  return(dataIFC)
}

##################

# WB Lending Pipeline  ----------------
.projectsTableLendingPipeline <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  #fromDate <- as.character(dateRange[[1]])
  #toDate <- as.character(dateRange[[2]])
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME,
                   Team_Leader = FULL_NME, Approval_Date = BD_APPRVL_DATE, 
                   Lending_Inst_Type = LENDING_INSTR_TYPE_NME,
                   Begin_Appraisal = BEGIN_APPRAISAL_DATE,Project_Amount,
                   Latest_Sort = rate_indicator, FY_Expenses = CURRENT_FY_COST,
                   Cum_Expenses = CUMULATIVE_FY_COST,FY_Prob = FY_PROB_TYPE_CODE,
                   ProjectOrder,url)
  # Financing products in Pipeline (ProjectOrder==2)
  dataTC <- filter(dataTC, Prod_Line == "Financing" & ProjectOrder==2)
  # filter by date range
  #dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line), ProjectOrder)
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # Attach a link to Project ID
  data <- mutate(data, PROJ_ID = 
                     paste0('<a href=',url,'>',PROJ_ID,'</a>'))
  data <- select(data, -url)
  
  # scale Expenses to thousands 
  data$FY_Expenses <- data$FY_Expenses/1000
  data$Cum_Expenses <- data$Cum_Expenses/1000
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  data$FY_Expenses <- format(data$FY_Expenses, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  data$Cum_Expenses <- format(data$Cum_Expenses, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  names(data) <- c("Project ID", "Project Name", "Team Leader", "Approval Date", "Lending Inst. Type",
                   "Begin Appraisal", "Commitment (US$M)","Latest Sort Overall Risk Rating","FY Expenses (US$K)",
                   "Cum Expenses (US$K)","FY Prob")
  
  return(data)
}

#############

# WB Portfolio Active  ----------------
.projectsTablePortfolioActive <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  #fromDate <- as.character(dateRange[[1]])
  #toDate <- as.character(dateRange[[2]])
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME,
                   Team_Leader = FULL_NME, Approval_Date = BD_APPRVL_DATE, 
                   Lending_Inst_Type = LENDING_INSTR_TYPE_NME,
                   Closing_Date = REVISED_CLS_DATE,Project_Amount,
                   Undisb_Bal = total_undis_balance,DO_RATING, IP_RATING,
                   Months_Problem = No_of_Months_in_problem_status,
                   ProjectOrder,url)
  # Financing products in Active (ProjectOrder==1)
  dataTC <- filter(dataTC, Prod_Line == "Financing" & ProjectOrder==1)
  # filter by date range
  #dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line), ProjectOrder)
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # Attach a link to Project ID
  data <- mutate(data, PROJ_ID = 
                   paste0('<a href=',url,'>',PROJ_ID,'</a>'))
  data <- select(data, -url)
  
  # scale Expenses to thousands 
  data$Undisb_Bal <- data$Undisb_Bal/1000000
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  data$Undisb_Bal <- format(data$Undisb_Bal, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  names(data) <- c("Project ID", "Project Name", "Team Leader", "Approval Date", "Lending Inst. Type",
                   "Closing Date", "Commitment (US$M)","Undisbursed Balance (US$M)",
                   "Project Rating DO", "Project Rating IP",
                   "Months in Problem Status")
  
  return(data)
}

#############

# WB Portfolio Closed  ----------------
.projectsTablePortfolioClosed <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  #fromDate <- as.character(dateRange[[1]])
  #toDate <- as.character(dateRange[[2]])
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME,
                   Team_Leader = FULL_NME, Approval_Date = BD_APPRVL_DATE, 
                   Lending_Inst_Type = LENDING_INSTR_TYPE_NME,
                   Closing_Date = REVISED_CLS_DATE,Project_Amount,
                   DO_RATING, IP_RATING,
                   ProjectOrder,url)
  # Financing products in Closed (ProjectOrder==3)
  dataTC <- filter(dataTC, Prod_Line == "Financing" & ProjectOrder==3)
  # filter by date range. Last 2 years
  dataTC <- filter(dataTC, (Closing_Date >= (Sys.Date() - 730)) | (is.na(Closing_Date)))
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Closing_Date))
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # Attach a link to Project ID
  data <- mutate(data, PROJ_ID = 
                   paste0('<a href=',url,'>',PROJ_ID,'</a>'))
  data <- select(data, -url)
  
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  names(data) <- c("Project ID", "Project Name", "Team Leader", "Approval Date", "Lending Inst. Type",
                   "Closing Date", "Commitment (US$M)",
                   "Project Rating DO", "Project Rating IP")
  
  return(data)
}

#############

# WB ASA Active  ----------------
.projectsTableASAActive <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  #fromDate <- as.character(dateRange[[1]])
  #toDate <- as.character(dateRange[[2]])
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Project_Name = PROJ_SHORT_NME,
                   Team_Leader = FULL_NME, Approval_Date = BD_APPRVL_DATE, 
                   Prod_Line, PROD_LINE_CODE, RAS, Current_ExpBB = CURRENT_BB_COST,
                   Cum_ExpBB = CUMULATIVE_BB_COST,FY_Expenses = CURRENT_FY_COST,
                   Cum_Expenses = CUMULATIVE_FY_COST,
                   ProjectOrder,url)
  # AAA in Active (ProjectOrder==1)
  dataTC <- filter(dataTC, Prod_Line == "Advisory Services and Analytics (ASA) IBRD" 
                   & ProjectOrder==1)
  # filter by date range
  #dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # arrange
  #dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line), ProjectOrder)
  dataTC <- select(dataTC,-ProjectOrder,-Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # Attach a link to Project ID
  data <- as.data.frame(data)
  data <- mutate(data, PROJ_ID = 
                   paste0('<a href=',url,'>',PROJ_ID,'</a>'))
  data <- select(data, -url)
  
  # scale Expenses to thousands 
  data$Current_ExpBB <- data$Current_ExpBB/1000
  data$Cum_ExpBB <- data$Cum_ExpBB/1000
  data$FY_Expenses <- data$FY_Expenses/1000
  data$Cum_Expenses <- data$Cum_Expenses/1000
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  # format Amount
  # format Amount
  data$FY_Expenses <- format(data$FY_Expenses, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  data$Cum_Expenses <- format(data$Cum_Expenses, digits=0, decimal.mark=".",
                              big.mark=",",small.mark=".", small.interval=3)
  data$Current_ExpBB <- format(data$Current_ExpBB, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  data$Cum_ExpBB <- format(data$Cum_ExpBB, digits=0, decimal.mark=".",
                              big.mark=",",small.mark=".", small.interval=3)
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  names(data) <- c("Task ID", "Task Name", "Team Leader", "Management Approval Date", 
                   "Product Line","RAS (Y/N)","Current Expenditure BB (US$K)", "Current Expenditure Total (US$K)",
                   "Lifetime Expenditure BB (US$K)","Lifetime Expenditure Total (US$K)")
  
  return(data)
}

#############

# WB ASA Closed  ----------------
.projectsTableASAClosed <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  #fromDate <- as.character(dateRange[[1]])
  #toDate <- as.character(dateRange[[2]])
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  # select relevant variables
  dataTC <- select(dataTC, PROJ_ID, Project_Name = PROJ_SHORT_NME,
                   Team_Leader = FULL_NME, Approval_Date = BD_APPRVL_DATE, 
                   Prod_Line, PROD_LINE_CODE, RAS, Current_ExpBB = CURRENT_BB_COST,
                   Cum_ExpBB = CUMULATIVE_BB_COST,FY_Expenses = CURRENT_FY_COST,
                   Cum_Expenses = CUMULATIVE_FY_COST,
                   ProjectOrder,url)
  # AAA in Active (ProjectOrder==1)
  dataTC <- filter(dataTC, Prod_Line == "Advisory Services and Analytics (ASA) IBRD" 
                   & ProjectOrder==3)
  # filter by date range
  #dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # arrange
  #dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line), ProjectOrder)
  dataTC <- select(dataTC,-ProjectOrder, -Prod_Line)
  # remove duplicates
  data <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # Attach a link to Project ID
  data <- as.data.frame(data)
  data <- mutate(data, PROJ_ID = 
                   paste0('<a href=',url,'>',PROJ_ID,'</a>'))
  data <- select(data, -url)
  
  # scale Expenses to thousands 
  data$Current_ExpBB <- data$Current_ExpBB/1000
  data$Cum_ExpBB <- data$Cum_ExpBB/1000
  data$FY_Expenses <- data$FY_Expenses/1000
  data$Cum_Expenses <- data$Cum_Expenses/1000
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  # format Amount
  # format Amount
  data$FY_Expenses <- format(data$FY_Expenses, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  data$Cum_Expenses <- format(data$Cum_Expenses, digits=0, decimal.mark=".",
                              big.mark=",",small.mark=".", small.interval=3)
  data$Current_ExpBB <- format(data$Current_ExpBB, digits=0, decimal.mark=".",
                               big.mark=",",small.mark=".", small.interval=3)
  data$Cum_ExpBB <- format(data$Cum_ExpBB, digits=0, decimal.mark=".",
                           big.mark=",",small.mark=".", small.interval=3)
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataTC)-2)))
  }
  names(data) <- c("Task ID", "Task Name", "Team Leader", "Management Approval Date", 
                   "Product Line","RAS (Y/N)","Current Expenditure BB (US$K)", "Current Expenditure Total (US$K)",
                   "Lifetime Expenditure BB (US$K)","Lifetime Expenditure Total (US$K)")
  
  return(data)
}

#############

# Country projects table ASA IFC ----------------
.projectsTableASA_IFC <- function(couName, status){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  #fromDate <- as.character(dateRange[[1]])
  #toDate <- as.character(dateRange[[2]])
  
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects(couName)
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Project_Name = PROJECT_NAME, Team_Leader = PROJECT_LEADER,
                    Approval_Date = ASIP_APPROVAL_DATE, Closing_Date = IMPLEMENTATION_END_DATE,
                    Project_Status, Project_Amount = TOTAL_FUNDING,
                    Current_Exp = PRORATED_TOTAL_FYTD_EXPENSE, ProjectOrder)
  dataIFC <- filter(dataIFC, Project_Status == status)
  #dataIFC <- filter(dataIFC, (Approval_Date >= fromDate) & (Approval_Date <= toDate)) #select country
  # arrange
  dataIFC <- arrange(as.data.frame(dataIFC), ProjectOrder)
  dataIFC <- select(dataIFC,-ProjectOrder, -Project_Status) # drop ProjectOrder
  # remove duplicates
  data <- dataIFC[!duplicated(dataIFC$PROJ_ID),]
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  # Scale amounts
  data$Project_Amount <- data$Project_Amount/1000
  data$Current_Exp <- data$Current_Exp/1000
  # format Amount
  data$Project_Amount <- format(data$Project_Amount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  data$Current_Exp <- format(data$Current_Exp, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  
  # If table is empty show "None"
  if (nrow(data)==0){
    data <- rbind(data,c("None",rep("",ncol(dataIFC)-1)))
  }
  names(data) <- c("Project ID", "Project Name", "Team Leader","IP Approval Date", 
                   "Expected End Date","Approval Value (in US$K)", "Current Expenditure (in US$K)")
  
  return(data)
}

#############

.projectsStatusData <- function(couName, count_type, dateRange){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  fromDate <- as.character(dateRange[[1]])
  toDate <- as.character(dateRange[[2]])
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Status=PROJECT_STATUS_NME, 
                   Approval_Date = BD_APPRVL_DATE, Project_Amount, ProjectOrder)
  # filter by date range
  dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # remove duplicates
  dataTC <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line), ProjectOrder)
  dataTC <- select(dataTC,-ProjectOrder,-Approval_Date) # drop ProjectOrder
  
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects(couName)
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Prod_Line, Project_Name = PROJECT_NAME,
                    Approval_Date = ASIP_APPROVAL_DATE, Project_Status, Project_Amount = TOTAL_FUNDS_MANAGED_BY_IFC,
                    ProjectOrder)
  # remove duplicates
  dataIFC <- dataIFC[!duplicated(dataIFC$PROJ_ID),]
  # filter by date range
  dataIFC <- filter(dataIFC, (Approval_Date >= fromDate) & (Approval_Date <= toDate)) #select country
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  # arrange
  dataIFC <- arrange(as.data.frame(dataIFC), ProjectOrder)
  dataIFC <- select(dataIFC,-ProjectOrder,-Project_Name, -Approval_Date) # drop ProjectOrder
  
  # Append both -----------------------
  data <- rbind_list(dataTC, dataIFC)
  
  data[is.na(data)] <- "0"
  # remove duplicates
  data <- data[!duplicated(data$PROJ_ID),]
  
  data$Prod_Line <- as.character(data$Prod_Line)
  data <- mutate(data, Prod_Line=ifelse(substr(Prod_Line,nchar(Prod_Line)-2,nchar(Prod_Line))=="IFC",
                                        "ASA IFC","ASA IBRD"))
  data$Prod_Line <- as.factor(data$Prod_Line)
  
  data <- data %>%
    group_by(Prod_Line,Project_Status) %>%
    mutate(totalAmount = sum(as.numeric(Project_Amount), na.rm=TRUE)/1000, 
           countProjects = n_distinct(PROJ_ID)) %>%
    select(-PROJ_ID,-Project_Amount)
  data <- data[!duplicated(data),]
  # format Amount
  data$totalAmount <- format(data$totalAmount, digits=0, decimal.mark=".",
                             big.mark=",",small.mark=".", small.interval=3)
  
  return(data)
  
}

######################


.projectsStatus <- function(couName, count_type, dateRange){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  fromDate <- as.character(dateRange[[1]])
  toDate <- as.character(dateRange[[2]])
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, Project_Status=PROJECT_STATUS_NME, 
                   Approval_Date = BD_APPRVL_DATE, Project_Amount, ProjectOrder)
  # filter by date range
  dataTC <- filter(dataTC, (Approval_Date >= fromDate) & (Approval_Date <= toDate))
  # remove duplicates
  dataTC <- dataTC[!duplicated(dataTC$PROJ_ID),]
  # arrange
  dataTC <- arrange(as.data.frame(dataTC), desc(Prod_Line), ProjectOrder)
  dataTC <- select(dataTC,-ProjectOrder,-Approval_Date) # drop ProjectOrder
  
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects(couName)
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Prod_Line, Project_Name = PROJECT_NAME,
                    Approval_Date = ASIP_APPROVAL_DATE, Project_Status, Project_Amount = TOTAL_FUNDS_MANAGED_BY_IFC,
                    ProjectOrder)
  # remove duplicates
  dataIFC <- dataIFC[!duplicated(dataIFC$PROJ_ID),]
  # filter by date range
  dataIFC <- filter(dataIFC, (Approval_Date >= fromDate) & (Approval_Date <= toDate)) #select country
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  # arrange
  dataIFC <- arrange(as.data.frame(dataIFC), ProjectOrder)
  dataIFC <- select(dataIFC,-ProjectOrder,-Project_Name, -Approval_Date) # drop ProjectOrder
  
  # Append both -----------------------
  data <- rbind_list(dataTC, dataIFC)
  
  data[is.na(data)] <- "0"
  # remove duplicates
  data <- data[!duplicated(data$PROJ_ID),]
  
  data$Prod_Line <- as.character(data$Prod_Line)
  data <- mutate(data, Prod_Line=ifelse(substr(Prod_Line,nchar(Prod_Line)-2,nchar(Prod_Line))=="IFC",
                                          "ASA IFC","ASA IBRD"))
  data$Prod_Line <- as.factor(data$Prod_Line)
  
  data <- data %>%
    group_by(Prod_Line,Project_Status) %>%
    mutate(totalAmount = sum(as.numeric(Project_Amount), na.rm=TRUE)/1000, 
           countProjects = n_distinct(PROJ_ID)) %>%
    select(-PROJ_ID,-Project_Amount)
  data <- data[!duplicated(data),]
  # format Amount
  data$totalAmount <- format(data$totalAmount, digits=0, decimal.mark=".",
                                big.mark=",",small.mark=".", small.interval=3)
  # generate plot
  if (nrow(data)>0){
    # Faceted chart
    if (count_type=="count"){ # plot number of projects
      
      ggplot(data, aes(x=Project_Status, y=countProjects,fill=Project_Status)) +
        geom_bar(stat="identity")+ #stat="identity") +
        facet_grid(~ Prod_Line)+
        theme(legend.key=element_blank(),
              legend.title=element_blank(),
              legend.position="top",
              panel.border = element_blank(),
              panel.background = element_blank(),
              plot.title = element_text(lineheight=.5)) + 
        labs(x="",y="")
      
    } else{ # plot $ amount of projects
      
      data <- filter(data, !(trimws(totalAmount)=="0"))
      if (nrow(data)>0){
        ggplot(data, aes(x=Project_Status, y=totalAmount, fill=Project_Status)) +
          geom_bar(stat="identity") +
          facet_wrap(~ Prod_Line)+
          theme(legend.key=element_blank(),
                legend.title=element_blank(),
                legend.position="top",
                panel.border = element_blank(),
                panel.background = element_blank(),
                plot.title = element_text(lineheight=.5)) + 
          labs(x="",y="")
      } else {
          plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
          graphics::text(1.5, 1,"No data available", col="red", cex=1.5)    
      }
    }
  } else {
    plot(c(1,1),type="n", frame.plot = FALSE, axes=FALSE, ann=FALSE)
    graphics::text(1.5, 1,"No data available", col="red", cex=1.5)
  }
  
}

######################

.projectsSectors <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, 
                   MAJORSECTOR_NAME1, SECTOR_PCT1, MAJORSECTOR_NAME2, SECTOR_PCT2, 
                   MAJORSECTOR_NAME3, SECTOR_PCT3, MAJORSECTOR_NAME4, SECTOR_PCT4,
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
  dataTC <- .filterTCProjects(couName)
  dataTC <- select(dataTC, PROJ_ID, Prod_Line, 
                   MAJORTHEME_NAME1, THEME_PCT1, MAJORTHEME_NAME2, THEME_PCT2, 
                   MAJORTHEME_NAME3, THEME_PCT3, MAJORTHEME_NAME4, THEME_PCT4,
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
    data$Pct <- data$Pct*100
    data$PctTotal <- format(data$PctTotal, digits=0, decimal.mark=".",
                              big.mark=",",small.mark=".", small.interval=3)
    data$Pct <- format(data$Pct, digits=1, decimal.mark=".",
                                  big.mark=",",small.mark=".", small.interval=3)
    data$Pct <- as.numeric(data$Pct)
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

.sectThemesTable <- function(couName,sectTheme){
  
  if (sectTheme=="sector"){
    data <- .projectsSectors(couName) 
    
  } else {
    data <- .projectsThemes(couName) 
  }
  
  names(data) <- c("Name","PctTotal","Pct")
  data <- select(data, -PctTotal)
  # format numbers
  data$Pct <- data$Pct*100
  data$Pct <- format(data$Pct, digits=1, decimal.mark=".",
                     big.mark=",",small.mark=".", small.interval=3)
  data$Pct <- as.numeric(data$Pct)
  
  return(data)
  
}

######################

# Country projects table ----------------
.projectsPeople <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  dataTC <- select(dataTC, Staff_Name = FULL_NAME, job_title, 
                   Location = CITY_NAME, Role = UPI_ROLE_DESC, PROJ_ID, Prod_Line, Project_Name = PROJ_SHORT_NME,
                   Project_Status=PROJECT_STATUS_NME, grade, duty_country, practice,
                   ProjectOrder,WORK_ALPHA)

  dataTC <- filter(dataTC, practice == "TAC", grade %in% c("GF","GG","GH","EC1","EC2","EC3")) #, position_type == "PRIMARY"
  dataTC <- select(dataTC, -grade,-duty_country,-practice, -Prod_Line, -Project_Status)
  dataTC <- dataTC[!duplicated(dataTC),]
  
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects(couName)
  # keep relevant columns
  dataIFC <- select(dataIFC, Staff_Name = FULL_NAME,job_title, 
                    Location = CITY_NAME, Role = UPI_ROLE_DESC, PROJ_ID, Prod_Line, Project_Name = PROJECT_NAME,
                    Project_Status, grade, duty_country, practice, emplyment_type,
                    Approval_Date = ASIP_APPROVAL_DATE,ProjectOrder,WORK_ALPHA)
  # projects within 3 fiscal years in the past
  dataIFC <- filter(dataIFC, Approval_Date >= "2013-07-01") #select country
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  # apply filters
  dataIFC <- filter(dataIFC, practice == "TAC",grade %in% c("GF","GG","GH","EC1","EC2","EC3")) #, position_type == "PRIMARY"
  dataIFC <- select(dataIFC, -duty_country,-practice,-emplyment_type,-Approval_Date, -Prod_Line, -Project_Status)
  dataIFC <- dataIFC[!duplicated(dataIFC),]
  
  # Append both --------------------------
  data <- rbind_list(dataTC, dataIFC)
  data <- filter(data, tolower(substr(WORK_ALPHA,1,3))=="gtc")
  # arrange
  data <- arrange(as.data.frame(data), grade, Staff_Name, PROJ_ID, ProjectOrder)
  data <- select(as.data.frame(data), -ProjectOrder, -grade, -WORK_ALPHA)
  # substitute NAs for "---" em-dash
  data[is.na(data)] <- "---"
  
  # make sure staff info appear only once
  if (nrow(data)>1){ # makes no sense to remove duplicate staff if nrow(data) < 2
    i <- 1
    while (i <= nrow(data)){
      j <- i + 1
      while(data$Staff_Name[j]==data$Staff_Name[i]){
        data$Staff_Name[j]<-""
        data$job_title[j]<-""
        data$Location[j]<-""
        j <- j + 1
        if (j > nrow(data)) j <- j - 1
      }
      i <- ifelse(j == nrow(data),j+1,j)
    }
  }
  return(data)
}

#############

# Country projects table ----------------
.projectsStaffStats <- function(couName){
  
  cou <- .getCountryCode(couName)
  couISO2 <- .getISO2(couName)
  
  ### IBRD T&C projects -----------------
  dataTC <- .filterTCProjects(couName)
  dataTC <- select(dataTC, PROJ_ID, Project_Status=PROJECT_STATUS_NME, Staff_Name = FULL_NAME, grade,
                   practice)
  # apply filters
  dataTC <- filter(dataTC, practice == "TAC")
    
  dataTC <- dataTC %>%
    filter(grade %in% c("GF","GG","GH","EC1","EC2","EC3")) %>%
    group_by(grade) %>%
    mutate(grade_count = length(grade)) %>%
    select(grade,grade_count)
  
  dataTC <- dataTC[!duplicated(dataTC),]
  
  ### IFC projects ----------
  dataIFC <- .filterIFCProjects(couName)
  # keep relevant columns
  dataIFC <- select(dataIFC, PROJ_ID, Staff_Name = FULL_NAME, grade, 
                    Approval_Date = ASIP_APPROVAL_DATE, practice)
  # projects within 3 fiscal years in the past
  dataIFC <- filter(dataIFC, Approval_Date >= "2013-07-01") #select country
  # make PROJ_ID character
  dataIFC$PROJ_ID <- as.character(dataIFC$PROJ_ID)
  # apply filters
  dataIFC <- filter(dataIFC, practice == "TAC") #, position_type == "PRIMARY"
  
  dataIFC <- dataIFC %>%
    filter(grade %in% c("GF","GG","GH","EC1","EC2","EC3")) %>%
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
  ggplot(data, aes(x=grade,y=grade_count,fill=as.factor(grade))) +
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
