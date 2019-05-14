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
  #This function return category based on inputted sales rating and margin rating
  #index 1: sales rating, index 2: marging rating
  if(inp[[1]] > 1){ #If sales rating > 1
    if(inp[[2]] > 0){cat <- "VERY GOOD"} #And if margin rating > 0, then out
    else if(inp[[2]] == 0){cat <- "GOOD"} #And if margin rating = 0, then out
    else cat <- "QUESTION" #If none of those logic above 
  }else if (inp[[1]] == 1){ #If sales rating = 1
    if(inp[[2]] > 0){cat <- "GOOD"} #And if margin rating > 0, then out
    else if(inp[[2]] == 0){cat <- "QUESTION"} #And if margin rating = 0, then out
    else cat <- "REVIEW" #If none of those logic above
  }else{ #If sales rating < 1
    if(inp[[2]] > 0){cat <- "QUESTION"} #And if margin rating > 0, then out
    else if(inp[[2]] == 0){cat <- "REVIEW"} #And if margin rating = 0, then out
    else cat <- "REVIEW" #If none of those logic above
  }
  
  return(cat) #Return category
}

category_comput <- function(promo_comp,baseline){
  #Function to compute sales rating and margin rating
  if(is.null(baseline)){ #If no baseline -> full uplift
    #Aggregated baseline (normal_comp variable) is set to 0 
    normal_comp <- data.table(NORMAL_Days=0,NORMAL_NET_SALES=0,NORMAL_SALES_QTY=0,NORMAL_CM_AMT=0,
                              NORMAL_MARGIN=0,AVG_QtySold=0,PRICE=0)
    rating <- data.table(BASE_SALES=0, BASE_MARGIN=0) #BASE_SALES and BASE_MARGIN are set to 0
    
    #Compute SALES_UPLIFT = Promo NetSales
    #MARGIN_UPLIFT = Promo Margin
    rating[,':='(SALES_UPLIFT = promo_comp$PROMO_NET_SALES,
                 MARGIN_UPLIFT = promo_comp$PROMO_CM_AMT)][]
    
    #SALES_RATING = round(SALES_UPLIFT/TOTAL_DISC_AMOUNT)
    #MARGIN_RATING = round(MARGIN_UPLIFT/TOTAL_DISC_AMOUNT)
    rating[,':='(SALES_RATING = round((SALES_UPLIFT/promo_comp$TOTAL_DISC_AMOUNT)),
                 MARGIN_RATING = round((MARGIN_UPLIFT/promo_comp$TOTAL_DISC_AMOUNT)) )][]
    
    #Call category_matrix function with SALES_RATING and MARGIN_RATING as parameters
    #and save the result in CATEGORY column
    rating[,':='(CATEGORY = category_matrix(list(rating$SALES_RATING,rating$MARGIN_RATING)) )]
  }else{ #If baseline is not null
    if(nrow(baseline) >= 30 ){ #If baseline >= 30 days
      
      #Aggregated baseline data based on number of rows/number of days
      #normal NET_SALES, normal SALES_QTY_UNIT, normal CM_AMT
      #Days = number of baseline rows, NET_SALES = sum of all normal NET_SALES
      #QTY = sum of all normal SALES_QTY_UNIT, MARGIN = sum of all normal CM_AMT
      normal_comp <- baseline[,.(NORMAL_Days = nrow(baseline), NORMAL_NET_SALES = sum(NET_SALES),
                                 NORMAL_SALES_QTY = sum(SALES_QTY_UNIT), NORMAL_CM_AMT = sum(CM_AMT))]
      
      #Compute NORMAL_MARGIN = MARGIN/NET_SALES
      #Average qty sold -> AVG_QtySold = (SALES_QTY_UNIT/Days), PRICE = (NET_SALES/SALES_QTY_UNIT)
      normal_comp[,':='(NORMAL_MARGIN = (NORMAL_CM_AMT/NORMAL_NET_SALES), AVG_QtySold = (NORMAL_SALES_QTY/NORMAL_Days),
                        PRICE = (NORMAL_NET_SALES/NORMAL_SALES_QTY))][]
      
      #Compute BASE_SALES = AVG_QtySold*Days*PRICE
      #BASE_MARGIN = NORMAL_MARGIN*BASE_SALES
      #SALES_UPLIFT = Promo NET_SALES - BASE_SALES
      #MARGIN_UPLIFT = Promo MARGIN - BASE_MARGIN
      #SALES_RATING = round(SALES_UPLIFT/TOTAL_DISC_AMOUNT)
      #MARGIN_RATING = round(MARGIN_UPLIFT/TOTAL_DISC_AMOUNT)
      rating <- data.table(BASE_SALES = (normal_comp$AVG_QtySold*normal_comp$PRICE)) #Ask, promo days removed because computation is everyday (promo days = 1 day) 
      rating[,':='(BASE_MARGIN = (normal_comp$NORMAL_MARGIN*BASE_SALES))]
      rating[,':='(SALES_UPLIFT = (promo_comp$PROMO_NET_SALES - rating$BASE_SALES),
                   MARGIN_UPLIFT = (promo_comp$PROMO_CM_AMT - rating$BASE_MARGIN))][]
      rating[,':='(SALES_RATING = round((SALES_UPLIFT/promo_comp$TOTAL_DISC_AMOUNT)),
                   MARGIN_RATING = round((MARGIN_UPLIFT/promo_comp$TOTAL_DISC_AMOUNT)) )][]
      
      #Call category_matrix function with SALES_RATING and MARGIN_RATING as parameters
      #and save the result in CATEGORY column
      rating[,':='(CATEGORY = category_matrix(list(rating$SALES_RATING,rating$MARGIN_RATING)) )]
    }else{ #If baseline < 30 days
      def_uplift = 0.3 #Define Default uplift = 30%
      
      #Aggregated baseline data based on number of rows/number of days
      #normal NET_SALES, normal SALES_QTY_UNIT, normal CM_AMT
      #Days = number of baseline rows, NET_SALES = sum of all normal NET_SALES
      #QTY = sum of all normal SALES_QTY_UNIT, MARGIN = sum of all normal CM_AMT
      normal_comp <- baseline[,.(NORMAL_Days = nrow(baseline), NORMAL_NET_SALES = sum(NET_SALES),
                                 NORMAL_SALES_QTY = sum(SALES_QTY_UNIT), NORMAL_CM_AMT = sum(CM_AMT))]
      
      #Compute NORMAL_MARGIN = MARGIN/NET_SALES
      #Average qty sold -> AVG_QtySold = (SALES_QTY_UNIT/Days), PRICE = (NET_SALES/SALES_QTY_UNIT)
      normal_comp[,':='(NORMAL_MARGIN = (NORMAL_CM_AMT/NORMAL_NET_SALES), AVG_QtySold = (NORMAL_SALES_QTY/NORMAL_Days),
                        PRICE = (NORMAL_NET_SALES/NORMAL_SALES_QTY))][]
      
      #Compute BASE_SALES = Promo NET_SALES*30%
      #BASE_MARGIN = 0
      #SALES_UPLIFT = Promo NET_SALES - BASE_SALES
      #MARGIN_UPLIFT = 0
      #SALES_RATING = round(SALES_UPLIFT/TOTAL_DISC_AMOUNT)
      #MARGIN_RATING = 0
      rating <- data.table(BASE_SALES = promo_comp$PROMO_NET_SALES*def_uplift, BASE_MARGIN=0)
      rating[,':='(SALES_UPLIFT = (promo_comp$PROMO_NET_SALES - rating$BASE_SALES), MARGIN_UPLIFT = 0)][]
      rating[,':='(SALES_RATING = round((SALES_UPLIFT/promo_comp$TOTAL_DISC_AMOUNT)), MARGIN_RATING = 0 )][] 
      
      #Call category_matrix function with SALES_RATING and MARGIN_RATING as parameters
      #and save the result in CATEGORY column
      rating[,':='(CATEGORY = category_matrix(list(rating$SALES_RATING,rating$MARGIN_RATING)) )]
    }
  }
  
  #Merge all results
  temp <- cbind(normal_comp,promo_comp,rating)
  return(temp) #Return data
}