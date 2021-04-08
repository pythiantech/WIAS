
shinyServer(function(input, output, session) {
    Sys.sleep(1)
    waiter_hide()
    
    # new_rvs <- reactiveValues()
    makeReactiveBinding('df')
    makeReactiveBinding('inventories')
    makeReactiveBinding('survey')
    
    # Read in reactive datasources
    df1 <- reactiveFileReader(
        intervalMillis = 24*60*60*1000,
        session = session,
        filePath = 'data/df.Rds',
        readFunc = readRDS
    ) 
    
    df <- reactive({
        df2 <- df1() %>% filter(!is.na(Eqpt) & !is.na(ItemCode) & !is.na(ItemDesc))
        df2
    })
    
    inventories <- reactiveFileReader(
        intervalMillis = 24*60*60*1000,
        session = session,
        filePath = 'data/inventories.Rds',
        readFunc = readRDS
    )
    
    survey <- reactiveFileReader(
        intervalMillis = 24*60*60*1000,
        session = session,
        filePath = 'data/survey.Rds',
        readFunc = readRDS
    )
    
    
    # Inventory Cleanup
    inv_tbl <- reactive({
        inv_tbl <- inventories() %>% select(Eqpt, ItemCode, ItemDesc) %>% filter(!is.na(ItemCode)) %>% 
            mutate(ic_tr = stri_trans_general(ItemCode, "russian-latin/bgn"),
                   clean = tolower(str_replace_all(ic_tr, "[^[:alnum:]]", ''))) %>% 
            mutate(DEPOTID = case_when(
                DEPOTID == 1 ~ 'IHQ',
                DEPOTID == 2 ~ 'VIZAG',
                DEPOTID == 3 ~ 'PORT BLAIR',
                DEPOTID == 4 ~ 'MUMBAI',
                DEPOTID == 5 ~ 'KARWAR',
                DEPOTID == 6 ~ 'KOCHI'
            ))
        inv_tbl
    })
    
    # DF For ABC Analysis
    df_abc <- reactive({
        df_abc <- df() %>% filter(!is.na(UNITCOST), UNITCOST > 0) %>% 
            select(ShipName, DEMANDDATE, Eqpt, ItemCode, ItemDesc, REQQTY, DENOMDESC, UNITCOST, CurrencyType, INR) %>% 
            filter(!is.na(Eqpt) & !is.na(ItemCode) & !is.na(ItemDesc)) %>% 
            filter(INR > 0)
        df_abc
    })
    
    # MSL Available DF
    msl_df <- reactive({
        msl_df <- df() %>% filter(!is.na(MSL), !is.na(Eqpt)) %>% group_by(Eqpt, ItemCode, ItemDesc) %>% 
            distinct(Eqpt, ItemCode, ItemDesc, .keep_all = TRUE) %>% 
            select(Eqpt, ItemCode, ItemDesc, MSL, LATESTQTY)
        msl_df
    })
    
    
    
    # Source reamining scripts
    source("serverScripts/server_Demands.R", local = TRUE)
    source("serverScripts/server_spares_consumption.R", local = TRUE)
    source('serverScripts/server_predictive_modeling.R', local = TRUE)
    source('serverScripts/server_refresh_data.R', local = TRUE)

})
