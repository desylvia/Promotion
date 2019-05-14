load_data <- function(str,flag=0){ #== Load data
  setwd("~/POS_Transmart/Promotion/Data")
    
  raw <- fread(str) #Load data based on string file name saved in str
  raw <- raw[,.(SITECODE,TRANSACTIONDATE,ITEM_CODE,ITM_ID,SKU,PRODUCT_NAME,SALES_QTY_UNIT,TOTAL_DISC_AMOUNT,
                SALES_METRIC_TYPE,SCCID,PROMO_PromoID,PROMO_StartDate,PROMO_EndDate,
                CM_AMT,NET_SALES)] #Get only specified columns
  
  #Convert SALES_QTY_UNIT, TOTAL_DISC_AMOUNT, CM_AMT, and NET_SALES to number
  strTmp = c('SALES_QTY_UNIT','TOTAL_DISC_AMOUNT','CM_AMT','NET_SALES')
  raw[, (strTmp) := lapply(.SD, as.numeric), .SDcols = strTmp]
  #Remove data with NET_SALES = 0, SALES_QTY_UNIT = 0, CM_AMT = 0 and data with NAs (empty)
  raw <- raw[NET_SALES != 0]
  raw <- raw[SALES_QTY_UNIT != 0]
  raw <- raw[CM_AMT != 0]
  raw <- raw[complete.cases(raw)]
  #Extract date from TRANSACTIONDATE
  raw$Date <- as.Date(raw$TRANSACTIONDATE)
  
  raw_data <- list() #Define variabel to save data
  raw_data[[1]] <- raw[SALES_METRIC_TYPE %in% c(1,2)] #Filter normal data
  raw_data[[2]] <- raw[SALES_METRIC_TYPE %in% c(3,4)] #Filter promo data
  
  return(raw_data) #Return data
}

baseline_comput <- function(normal_period_comput,start_date){
  #Get baseline function. Basically, this function will return normal data start from a day
  #before start date to 90 days, if available.
  #normal_period_comput -> Aggregated normal data, start_date -> start date of promotion ID from data
  idx <- match(start_date,normal_period_comput$Date) #Get start_date index in normal_period_comput
  
  if(is.na(idx)){ #If index is null / start_date is not available in data
    stp <- TRUE #Variabel for checking condition
    while(stp){
      #While index is still null, jump to a day before start_date (start_date-1) 
      #until index is not null (new start_date is available in data) 
      start_date <- start_date-1
      idx <- match(start_date,normal_period_comput$Date)
      if(!is.na(idx)){
        stp <- FALSE #Set to FALSE to exit While loop
      }
    }
  }else idx <- idx-1 #IF index is not null / start_date is available,
                     #start position from a day before start_date
  check <- idx-90+1 #Define check position at 90 days before start_date - 1 day
  if(check > 0) {baseline <- normal_period_comput[check:idx,] #If available in data, get it
  }else baseline <- normal_period_comput[1:idx,] #If it is not available, just return as it is.
  
  return(baseline) #Return baseline
}

category_matrix <- function(inp){
  #-- Confidential computation
  
  return(cat) #Return category
}

category_comput <- function(promo_comp,baseline){
  #Function to compute sales rating and margin rating
  #-- Confidential computation
  
  return(temp) #Return data
}
