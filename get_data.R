library(readr)
library(dplyr)
library(lubridate)

getData <- function(folder_name) {
  path <- paste0('data_ext/', folder_name)
  files <- list.files(path = path, pattern = '.csv$', full.names = T)
  filenames <- gsub(paste0('data_ext/', folder_name, '/'), '', gsub("\\.csv$", '', files))
  #
  for (i in filenames) {
    assign(i, read_csv(paste0('data_ext/', folder_name, '/', i, '.csv')))
  }
  assign('demands', tbl_LMS_TBLDEMANDMASTER)
  assign('dem_type', tbl_LMS_TBLLOOKUPDEMANDTYPES)
  assign('inventory', tbl_LMS_TBLINVENTORYMASTER)
  assign('ship_master', tbl_LMS_TBLSHIPMASTER)
  assign('eqpt', tbl_LMS_TBLEQPTMASTER)
  assign('currency', tbl_LMS_TBLLOOKUPCURRENCY)
  assign('denom', tbl_LMS_TBLLOOKUPDENOMINATION)
  assign('depot', tbl_LMS_TBLDEPOTMASTER)
  assign('gatepass', TBL_LMS_TBLiSSUEGATEPAS)
  assign('direct_indent', tbl_LMS_TBL_DIRECTINDENT)
  assign('roles', tbl_LMS_TBLROLES)
  assign('ard_dem_master', tbl_LMS_TBLARD_DEMANDMASTER)
  assign('ard_dem_process_master', tbl_LMS_TBL_ARD_DEMANDPROCESSMASTER)
  assign('ard_dem_process', tbl_LMS_TBL_ARD_DEMANDPROCESS)
  assign('survey', tbl_LMS_TBLSURVEYRECEIPTS)
  assign('survey_type', tbl_LMS_TBLLOOKUPSURVEYTYPE)
  assign('refit_info', tbl_LMS_TBLSHIPREFITINFO)
  assign('refit_type', tbl_LMS_TBLREFITTYPE)
  
  rm(tbl_LMS_TBL_ARD_DEMANDPROCESS, tbl_LMS_TBL_ARD_DEMANDPROCESSMASTER, tbl_LMS_TBL_DIRECTINDENT, tbl_LMS_TBLARD_DEMANDMASTER,
     tbl_LMS_TBLDEMANDMASTER, tbl_LMS_TBLDEPOTMASTER, tbl_LMS_TBLEQPTMASTER, tbl_LMS_TBLINVENTORYMASTER, tbl_LMS_TBLLOOKUPCURRENCY,
     tbl_LMS_TBLLOOKUPDEMANDTYPES, tbl_LMS_TBLLOOKUPDENOMINATION, tbl_LMS_TBLLOOKUPSURVEYTYPE, tbl_LMS_TBLREFITTYPE,
     tbl_LMS_TBLROLES, tbl_LMS_TBLSHIPMASTER, tbl_LMS_TBLSHIPREFITINFO, tbl_LMS_TBLSURVEYRECEIPTS, TBL_LMS_TBLiSSUEGATEPAS, 
     files, filenames,i, path)
  
  demands <- demands %>%  select(-c(DTGNO, EARMARKNO))
  demands$CLOSED <- as.factor(demands$CLOSED)
  inventory$ITEMCODE <- as.character(inventory$ITEMCODE)
  inventory$ITEMDESC <- as.character(inventory$ITEMDESC)
  gatepass$ISSUEDQTY <- as.numeric(gatepass$ISSUEDQTY)
  gate_issued <- gatepass %>% group_by(DEMANDID) %>% summarise(IssuedQty = sum(ISSUEDQTY, na.rm=TRUE))
  survey$SURVEYTYPE <- survey_type$SURVEYTYPE[match(survey$SURVEYTYPEID, survey_type$SURVEYTYPEID)]
  
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
  demands$ShipName <- as.character(ship_master$SHIPNAME[match(demands$CLIENTID, ship_master$SHIPID)])
  demands$DENOMID <- inventory$DENOMID[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$DENOMDESC <- denom$DENOMDESC[match(demands$DENOMID, denom$DENOMID)]
  demands$IssuedQty <- gate_issued$IssuedQty[match(demands$DEMANDID, gate_issued$DEMANDID)]
  demands$REQQTY <- as.numeric(demands$REQQTY)
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
  
  if (nrow(ard_dem_master) > 0) {
    demands$ARDID <- ard_dem_master$ARDID[match(demands$DEMANDID, ard_dem_master$DEMANDID)]
    demands$DUESIN <- ard_dem_process$DUESIN[match(demands$ARDID, ard_dem_process$A_PID)]
    demands$DUES_OUT <- ard_dem_process$DUES_OUT[match(demands$ARDID, ard_dem_process$A_PID)]
    demands$STOCK_HELD <- ard_dem_process$STOCK_HELD[match(demands$ARDID, ard_dem_process$A_PID)]
    demands$MSL <- ard_dem_process$MSL[match(demands$ARDID, ard_dem_process$A_PID)]
    demands$PQFINAL <- ard_dem_process$PQFINAL[match(demands$ARDID, ard_dem_process$A_PID)]
  } else {
    demands$ARDID <- NA
    demands$DUESIN <- NA
    demands$DUES_OUT <- NA
    demands$STOCK_HELD <- NA
    demands$MSL <- NA
    demands$PQFINAL <- NA
  }
  
  df <- demands %>% select(ShipName, Eqpt, ItemCode, ItemDesc, DepotID, CATEGORY,DEMANDID, ARDID,
                           DUESIN, DUES_OUT, STOCK_HELD, MSL, PQFINAL,
                           DEMANDDATE, DEMANDNO, DEMANDTYPE, REQQTY, DENOMDESC,DEMANDSTATUS,
                           MSL, LATESTQTY, UNITCOST, CurrencyType, IssuedQty, IssueDiff)
  inventory$Eqpt <- eqpt$EQPTNAME[match(inventory$EQPTID, eqpt$EQPTID)]
  survey$ShipName <- df$ShipName[match(survey$DEMANDNO, df$DEMANDNO)]
  survey$Eqpt <- df$Eqpt[match(survey$DEMANDNO, df$DEMANDNO)]
  survey$ItemCode <- df$ItemCode[match(survey$DEMANDNO, df$DEMANDNO)]
  survey$ItemDesc <- df$ItemDesc[match(survey$DEMANDNO, df$DEMANDNO)]
 
  df$ShipName <- as.character(df$ShipName)
  df$Eqpt <- as.character(df$Eqpt)
  df$ItemCode <- as.character(df$ItemCode)
  df$ItemDesc <- as.character(df$ItemDesc)
  df$DepotID <- as.character(df$DepotID)
  df$CATEGORY <- as.character(df$CATEGORY)
  df$DEMANDID <- as.integer(df$DEMANDID)
  df$ARDID <- as.integer(df$ARDID)
  df$DUESIN <- as.integer(df$DUESIN)
  df$DUES_OUT <- as.integer(df$DUES_OUT)
  df$STOCK_HELD <- as.integer(df$DUES_OUT)
  df$MSL <- as.integer(df$MSL)
  df$PQFINAL <- as.integer(df$PQFINAL)
  df$REQQTY <- as.integer(df$REQQTY)
  df$LATESTQTY <- as.integer(df$LATESTQTY)
  
  inventory <- inventory %>% 
    select(Eqpt, ItemCode = ITEMCODE, ItemDesc = ITEMDESC, DENOMID, CATEGORY, DEPOTID,LATESTQTY, UNITCOST)
  
  survey <- survey %>% 
    select(ShipName, Eqpt, ItemCode, ItemDesc, SURVEYTYPE, DEMANDNO, SURVEYNO, SURVEYDATE, RECQTY, TYPE)
  
  wed_data <- list(df, inventory, survey)
  return(wed_data)
}

vizag <- getData('vizag')
mumbai <- getData('mumbai')
kochi <- getData('kochi')
karwar <- getData('karwar')



df <- bind_rows(vizag[1], mumbai[1], kochi[1], karwar[1])

#11 Apr 20: Added for "refresh button" functionality
ship_class <- read_csv('data/ship_class.csv')
# ship_class <- read_csv('/home/dwe/Documents/data/ship_class.csv')

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
inventories <- bind_rows(vizag[2], mumbai[2], kochi[2], karwar[2])
survey <- bind_rows(vizag[3], mumbai[3], kochi[3], karwar[3])
rm(karwar, kochi, mumbai, vizag, ship_class)
# saveRDS(df, '/home/dwe/Documents/data/df.Rds')
# saveRDS(inventories, '/home/dwe/Documents/data/inventories.Rds')
# saveRDS(survey, '/home/dwe/Documents/data/survey.Rds')

#11 Apr 20: Added for "refresh button" functionality
saveRDS(df, 'data/df.Rds')
saveRDS(inventories, 'data/inventories.Rds')
saveRDS(survey, 'data/survey.Rds')
