#Last edited 2019-04-09 (Desylvia)
#Code to filter all promotion on certain range

#Load functions
setwd("~/POS_Transmart/Promotion/Code/Dev")
source("Functions.r")

library(data.table) #Load data.table library


raw <- load_data("DAT_PROMOTION_2018.txt",1) #id 1 -> normal data, id 2 -> promo data, load data and save to raw
raw_normal <- raw[[1]] #Get normal data
raw_promo <- raw[[2]] #Get promo data
rm(raw) #Remove original variable (raw)

#Set data range
start_date <- "2018-07-01"
end_date <- "2018-07-07"
raw_promo <- raw_promo[TRANSACTIONDATE %between% c(start_date,end_date)]

#Aggregated promo data based on NET_SALES, SALES_QTY_UNIT, CM_AMT group by ITM_ID,PROMO_PromoID,
#Date,SKU,PRODUCT_NAME
dat_promo <- raw_promo[,.(PROMO_NET_SALES = sum(NET_SALES),PROMO_SALES_QTY = sum(SALES_QTY_UNIT), 
                          PROMO_CM_AMT = sum(CM_AMT),TOTAL_DISC_AMOUNT = sum(TOTAL_DISC_AMOUNT)),
                       by=.(ITM_ID,PROMO_PromoID,Date,SKU,PRODUCT_NAME)]

raw_normal <- raw_normal[!(Date %like% "2017-")] #Remove 2017 data

tmp <- list() #Define list to save result
for(idx in 1:nrow(dat_promo)){ #Loop All promo data (for each row)
#for(idx in 1:100){
  #Get normal data, for each SKU in promo data, based on certain index
  dat_normal <- raw_normal[SKU == dat_promo$SKU[idx]]
  
  #If normal data is empty or when normal data day 1 is later than promo date
  if(nrow(dat_normal) == 0 || (dat_promo$Date[1] < as.Date(dat_normal$Date[1]))){
    baseline <- NULL #Set baseline to null
  }else{
    #Aggregated normal data based on NET_SALES, SALES_QTY_UNIT, CM_AMT group by date
    dat_normal <- dat_normal[,.(NET_SALES = sum(NET_SALES),SALES_QTY_UNIT = sum(SALES_QTY_UNIT),
                                CM_AMT = sum(CM_AMT)),by=Date]
    baseline <- baseline_comput(dat_normal, dat_promo$Date[idx]) #Call function to get baseline
  }
  
  #Get promo data for each SKU based on certain index
  period_tmp <- dat_promo[,.(PROMO_NET_SALES,PROMO_SALES_QTY,PROMO_CM_AMT,TOTAL_DISC_AMOUNT)][idx]
  #Get ITM_ID,PROMO_PromoID,Date,SKU,PRODUCT_NAME in promo data for each SKU
  prop_tmp <- dat_promo[,.(ITM_ID,PROMO_PromoID,Date,SKU,PRODUCT_NAME)][idx]
  tryCatch( #Exception handling
    expr = { #If no errors or warnings, call function to compute promotion category (Very Good, Good, etc)
      res <- category_comput(period_tmp,baseline)
      tmp[[idx]] <- cbind(prop_tmp,res) #Merge computation result with the rest promo data columns
    },
    #If error or warning, print SKU index (for checking)
    error = function(e){
      message('Caught an error!')
      print(idx)
    },
    warning = function(w){
      message('Caught an warning!')
      print(idx)
    }
  )
}
res <- do.call(rbind, tmp) #Convert list to data table

dim_promo <- fread("DIM_PROMOTION.txt") #Load DIM_PROMOTION
#Get only Promo_ID, Promo_Media, Promo_Type
tmp <- data.table(Promo_ID=dim_promo$Promo_ID,Promo_Media=dim_promo$Promo_Media,
                  Promo_Type=dim_promo$Promo_Type)
#Inner join computation result with some columns from DIM_PROMOTION to conclude information
final_result <- tmp[res, on=c(Promo_ID="PROMO_PromoID")]
final_result[is.na(final_result)] <- "" #Remove NAs

dim_items <- fread("DIM_ITEMS_VIEW.txt") #Load DIM_ITEMS_VIEW
#Get only ITM_ID, DIV_CODE, DEPT_CODE, GF_CODE, F_Code, SF_Code, BRAND
tmp <- data.table(ITM_ID=dim_items$ITM_ID,DIV_CODE=dim_items$DIV_CODE,
                  DEPT_CODE=dim_items$DEPT_CODE,GF_CODE=dim_items$GF_CODE,F_Code=dim_items$F_Code,
                  SF_Code=dim_items$SF_Code,BRAND=dim_items$BRAND)
#Inner join computation result with some columns from DIM_ITEMS_VIEW to complete information
final_result <- tmp[final_result, on="ITM_ID"]

#Save result
write.table(final_result, paste(paste(start_date,end_date,"final_result",sep = "_"),"txt",sep = "."),
            sep="|",row.names=FALSE)

rm(list=ls()) #Remove all data in memory
gc() #Clear memory cache
