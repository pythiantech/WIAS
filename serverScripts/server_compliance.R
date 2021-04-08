getData <- function(dbname){
  con <- dbConnect(odbc(), 
                   Driver = "Simba SQL Server ODBC Driver", 
                   Server = "localhost", 
                   UID = 'SA',
                   PWD  = 'Dhiraj@0401',
                   database = dbname)
  
  demands <- tbl(con, 'tbl_LMS_TBLDEMANDMASTER') %>% 
    select(-c(DTGNO, EARMARKNO)) %>% collect()
  demands$CLOSED <- as.factor(demands$CLOSED)
  
  inventory <- tbl(con, 'tbl_LMS_TBLINVENTORYMASTER') %>% collect()
  inventory$ITEMCODE <- as.character(inventory$ITEMCODE)
  inventory$ITEMDESC <- as.character(inventory$ITEMDESC)
  
  ship_master <- tbl(con, 'tbl_LMS_TBLSHIPMASTER') %>% collect()
  eqpt <- tbl(con, 'tbl_LMS_TBLEQPTMASTER') %>% collect()
  currency <- tbl(con, 'tbl_LMS_TBLLOOKUPCURRENCY') %>% collect()
  
  denom <- tbl(con, 'tbl_LMS_TBLLOOKUPDENOMINATION') %>% collect()
  
  depot <- tbl(con, 'tbl_LMS_TBLDEPOTMASTER') %>% collect()
  
  gatepass <- tbl(con, 'TBL_LMS_TBLiSSUEGATEPAS') %>% collect()
  
  
  #Close the connection
  dbDisconnect(con)
  
  #Items not issues
  dem_noissue <- demands %>% filter(DEMANDID %!in% gatepass$DEMANDID)
  dem_issued <- demands %>% filter(DEMANDID %in% gatepass$DEMANDID) %>% 
    mutate(
      ISSUEDQTY = gatepass$ISSUEDQTY[match(DEMANDID, gatepass$DEMANDID)],
      Dem_Diff = REQQTY - ISSUEDQTY
    )
  
  
  demands$ItemCode <- inventory$ITEMCODE[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$ItemDesc <- inventory$ITEMDESC[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$EQPTID <- inventory$EQPTID[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$MSL <- inventory$MSL[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$LATESTQTY <- inventory$LATESTQTY[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$UNITCOST <- inventory$UNITCOST[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$CURRENCYID <- inventory$CURRENCYID[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$CurrencyType <- currency$CURRENCYNAME[match(demands$CURRENCYID, currency$CURRENCYID)]
  demands$Eqpt <- eqpt$EQPTNAME[match(demands$EQPTID, eqpt$EQPTID)]
  
  demands$Ship <- substr(demands$DEMANDNO, 1, 3)
  demands$ShipName <- as.character(ship_master$SHIPNAME[match(demands$Ship, ship_master$SHORTNAME)])
  demands$ShipName_Code <- as.character(ship_master$SHIPNAME[match(demands$CLIENTID, ship_master$SHIPID)])
  demands$DENOMID <- inventory$DENOMID[match(demands$INVENTORYID, inventory$INVENTORYID)]
  demands$DENOMDESC <- denom$DENOMDESC[match(demands$DENOMID, denom$DENOMID)]
  
  df <- demands %>% select(Ship, ShipName, ShipName_Code, Eqpt, ItemCode, ItemDesc, DepotID,
                           DEMANDDATE, DEMANDNO, DEMANDTYPE, REQQTY, DENOMDESC,DEMANDSTATUS,
                           MSL, LATESTQTY, UNITCOST, CurrencyType)
  
  return(df)
}