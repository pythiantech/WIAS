library(tidyverse)
library(lubridate)
library(RODBC)
library(odbc)
library(dbplyr)
library(stringi)
library(DBI)

#Function to extract tables and combine
#them from databases

getData <- function(dbname){
  # con <- dbConnect(odbc(), 
  #                  Driver = "Simba SQL Server ODBC Driver", 
  #                  Server = "localhost", 
  #                  UID = 'SA',
  #                  PWD  = 'Dhiraj@0401',
  #                  database = dbname)

  # On Linux
  con <- dbConnect(odbc(),
                  Driver =  "Simba SQL Server ODBC Driver",
                  Server =   "localhost",
                  Database = dbname,
                  UID =      "SA",
                  PWD =      "Dhiraj@0401")
################################################################################
#This SQL query is to be used only once as we are doing conversion of data
#data types in the main SQL database. If not done, only single letters will
#be returned in various columns (20 Jan 20)
################################################################################  
  # dbGetQuery(con, '
  #            ALTER TABLE dbo.tbl_LMS_TBLINVENTORYMASTER
  #            ALTER COLUMN ITEMCODE NVARCHAR (1000)
  #            ALTER TABLE dbo.tbl_LMS_TBLINVENTORYMASTER
  #            ALTER COLUMN ITEMDESC NVARCHAR (1000)
  #            ')
##############################################################################
  demands <- tbl(con, 'tbl_LMS_TBLDEMANDMASTER') %>% 
    select(-c(DTGNO, EARMARKNO)) %>% collect()
  demands$CLOSED <- as.factor(demands$CLOSED)
  dem_type <- tbl(con, 'tbl_LMS_TBLLOOKUPDEMANDTYPES') %>% collect()
  inventory <- tbl(con, 'tbl_LMS_TBLINVENTORYMASTER') %>% collect()
  inventory$ITEMCODE <- as.character(inventory$ITEMCODE)
  inventory$ITEMDESC <- as.character(inventory$ITEMDESC)
  
  ship_master <- tbl(con, 'tbl_LMS_TBLSHIPMASTER') %>% collect()
  eqpt <- tbl(con, 'tbl_LMS_TBLEQPTMASTER') %>% collect()
  currency <- tbl(con, 'tbl_LMS_TBLLOOKUPCURRENCY') %>% collect()
  denom <- tbl(con, 'tbl_LMS_TBLLOOKUPDENOMINATION') %>% collect()
  depot <- tbl(con, 'tbl_LMS_TBLDEPOTMASTER') %>% collect()
  gatepass <- tbl(con, 'TBL_LMS_TBLiSSUEGATEPAS') %>% collect()
  gate_issued <- gatepass %>% group_by(DEMANDID) %>% summarise(IssuedQty = sum(ISSUEDQTY, na.rm=TRUE))
  direct_indent <- tbl(con, 'tbl_LMS_TBL_DIRECTINDENT') %>% collect()
  roles <- tbl(con, 'tbl_LMS_TBLROLES') %>% collect()
  ard_dem_master <- tbl(con, 'tbl_LMS_TBLARD_DEMANDMASTER') %>% collect()
  ard_dem_process_master <- tbl(con, 'tbl_LMS_TBL_ARD_DEMANDPROCESSMASTER') %>% collect()
  ard_dem_process <- tbl(con, 'tbl_LMS_TBL_ARD_DEMANDPROCESS') %>% collect()
  survey <- tbl(con, 'tbl_LMS_TBLSURVEYRECEIPTS') %>% collect()
  survey_type <- tbl(con, 'tbl_LMS_TBLLOOKUPSURVEYTYPE') %>% collect()
  refit_info <- tbl(con, 'tbl_LMS_TBLSHIPREFITINFO') %>% collect()
  refit_type <- tbl(con, 'tbl_LMS_TBLREFITTYPE') %>% collect()
  survey$SURVEYTYPE <- survey_type$SURVEYTYPE[match(survey$SURVEYTYPEID, survey_type$SURVEYTYPEID)]
  #Close the connection
  dbDisconnect(con)
  
  inventory$UNITCOST <- as.double(inventory$UNITCOST)
  demands$ItemCode <- inventory$ITEMCODE[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$ItemDesc <- inventory$ITEMDESC[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$EQPTID <- inventory$EQPTID[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$MSL <- inventory$MSL[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$LATESTQTY <- inventory$LATESTQTY[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$UNITCOST <- inventory$UNITCOST[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$CURRENCYID <- inventory$CURRENCYID[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$CurrencyType <- currency$CURRENCYNAME[match(demands$CURRENCYID, currency$CURRENCYID)]
  demands$Eqpt <- eqpt$EQPTNAME[match(demands$EQPTID, eqpt$EQPTID)]
  demands$CATEGORY <- inventory$CATEGORY[match(demands$INVENTORYID, inventory$INVENTORYID)]
  # demands$Ship <- substr(demands$DEMANDNO, 1, 3)
  # demands$ShipName <- as.character(ship_master$SHIPNAME[match(demands$Ship, ship_master$SHORTNAME)])
  demands$ShipName <- as.character(ship_master$SHIPNAME[match(demands$CLIENTID, ship_master$SHIPID)])
  demands$DENOMID <- inventory$DENOMID[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$DENOMDESC <- denom$DENOMDESC[match(demands$DENOMID, denom$DENOMID)]
  demands$IssuedQty <- gate_issued$IssuedQty[match(demands$DEMANDID, gate_issued$DEMANDID)]
  demands <- demands %>% mutate(
    DEMANDSTATUS = case_when(
      DEMANDSTATUS == 'C' ~ 'Closed',
      DEMANDSTATUS == 'P' ~ 'Pending',
      DEMANDSTATUS == 'R' ~ 'Raised',
      DEMANDSTATUS == 'A' ~ 'Approved',
      DEMANDSTATUS == 'F' ~ 'Forwarded'
    ),
    DepotID = case_when(
      DepotID == 1 ~ 'IHQ',
      DepotID == 2 ~ 'VIZAG',
      DepotID == 3 ~ 'PORT BLAIR',
      DepotID == 4 ~ 'MUMBAI',
      DepotID == 5 ~ 'KARWAR',
      DepotID == 6 ~ 'KOCHI'
    ),
    IssueDiff = REQQTY - IssuedQty
  )
  demands$ARDID <- ard_dem_master$ARDID[match(demands$DEMANDID, ard_dem_master$DEMANDID)]
  demands$DUESIN <- ard_dem_process$DUESIN[match(demands$ARDID, ard_dem_process$A_PID)]
  demands$DUES_OUT <- ard_dem_process$DUES_OUT[match(demands$ARDID, ard_dem_process$A_PID)]
  demands$STOCK_HELD <- ard_dem_process$STOCK_HELD[match(demands$ARDID, ard_dem_process$A_PID)]
  demands$MSL <- ard_dem_process$MSL[match(demands$ARDID, ard_dem_process$A_PID)]
  demands$PQFINAL <- ard_dem_process$PQFINAL[match(demands$ARDID, ard_dem_process$A_PID)]
  df <- demands %>% select(ShipName, Eqpt, ItemCode, ItemDesc, DepotID, CATEGORY,DEMANDID, ARDID,
                           DUESIN, DUES_OUT, STOCK_HELD, MSL, PQFINAL,
                           DEMANDDATE, DEMANDNO, DEMANDTYPE, REQQTY, DENOMDESC,DEMANDSTATUS,
                           MSL, LATESTQTY, UNITCOST, CurrencyType, IssuedQty, IssueDiff)
  inventory$Eqpt <- eqpt$EQPTNAME[match(inventory$EQPTID, eqpt$EQPTID)]
  survey$ShipName <- df$ShipName[match(survey$DEMANDNO, df$DEMANDNO)]
  survey$Eqpt <- df$Eqpt[match(survey$DEMANDNO, df$DEMANDNO)]
  survey$ItemCode <- df$ItemCode[match(survey$DEMANDNO, df$DEMANDNO)]
  survey$ItemDesc <- df$ItemDesc[match(survey$DEMANDNO, df$DEMANDNO)]
  
  wed_data <- list(df, inventory, survey)
  return(wed_data)
}

vizag <- getData('WLMSNG_VIZAG')
mumbai <- getData('MUMBAI')
kochi <- getData('KOCHI')
karwar <- getData('KARWAR')



df <- bind_rows(vizag[1], mumbai[1], kochi[1], karwar[1])
ship_class <- read_csv('data/ship_class.csv')
df$ShipClass <- ship_class$SHIPCLASS[match(df$ShipName,ship_class$SHIPNAME)]
df <- df %>% mutate(
  Conversion = case_when(
    CurrencyType == 'EUROS' ~ 79.61,
    CurrencyType == 'POUND' ~ 87.02,
    CurrencyType == 'Rouble' ~ 1.09,
    CurrencyType == 'Rupees' ~ 1,
    CurrencyType == 'USD' ~ 71.68
  ),
  INR = as.double(Conversion*UNITCOST)
)
inventories <- bind_rows(vizag[2], mumbai[2], kochi[2], karwar[2]) %>% 
  select(Eqpt, ItemCode = ITEMCODE, ItemDesc = ITEMDESC, DENOMID, CATEGORY, DEPOTID,LATESTQTY, UNITCOST)
survey <- bind_rows(vizag[3], mumbai[3], kochi[3], karwar[3]) %>% 
  select(ShipName, Eqpt, ItemCode, ItemDesc, SURVEYTYPE, DEMANDNO, SURVEYNO, SURVEYDATE, RECQTY, TYPE)
rm(karwar, kochi, mumbai, vizag, ship_class)
# df$UNITCOST <- as.double(df$UNITCOST)
# df$INR <- as.double(df$INR)
saveRDS(df, 'data/new_df.Rds')
saveRDS(inventories, 'data/inventories.Rds')
saveRDS(survey, 'data/survey.Rds')


###########################
#Info Schema
#https://db.rstudio.com/best-practices/run-queries-safely/
info_schema <- dbGetQuery(con, 'SELECT * FROM INFORMATION_SCHEMA.COLUMNS') 
