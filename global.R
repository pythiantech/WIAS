#Load all the requisite libraries

library(dplyr)
library(ggplot2)
library(purrr)
library(tidyr)
library(highcharter)
library(plotly)
library(crosstalk)
library(DT)
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinydashboard)
library(ggthemes)
library(lubridate)
library(dygraphs)
library(xts)
library(shinythemes)
library(htmlwidgets)
library(forecast)
library(fpp2)
library(TTR)
library(shinyalert)
library(seasonal)
library(tsintermittent)
library(ABCanalysis)
library(stringdist)
library(stringi)
library(stringr)
library(shinymanager)
# library(dbplyr)
library(viridis)
library(waiter)
library(xgboost)
#############################################################
#For reactiveFileReader
# LoadToEnvironment <- function(Rdata, env = new.env()) {
#   load(Rdata, env)
#   return(env)
# }

#Not in function
'%!in%' <- function(x,y)!('%in%'(x,y))

#Read in the data

# df <- readRDS('data_ext/df.Rds') %>% mutate(DEMANDDATE = lubridate::date(DEMANDDATE)) %>% 
#   filter(!is.na(Eqpt) & !is.na(ItemCode) & !is.na(ItemDesc))
# inventories <- readRDS('data_ext/inventories.Rds')
# survey <- readRDS('data_ext/survey.Rds')

#====================================================

##############################################################
#Inventory Cleanup

###On the consolidated df table
# inv_tbl <- inventories %>% select(Eqpt, ItemCode, ItemDesc) %>% filter(!is.na(ItemCode)) %>% 
#   mutate(ic_tr = stri_trans_general(ItemCode, "russian-latin/bgn"),
#          clean = tolower(str_replace_all(ic_tr, "[^[:alnum:]]", ''))) 
# 
# #DF for ABC analysis
# df_abc <- df %>% filter(!is.na(UNITCOST), UNITCOST > 0) %>% 
#   select(ShipName, DEMANDDATE, Eqpt, ItemCode, ItemDesc, REQQTY, DENOMDESC, UNITCOST, CurrencyType, INR) %>% 
#   filter(!is.na(Eqpt) & !is.na(ItemCode) & !is.na(ItemDesc)) %>% 
#   filter(INR > 0)

xts_func <- function(df) {
  df_items <- df %>% 
    group_by(month = floor_date(DEMANDDATE, unit = "month")) %>%
    summarise(Count = n(), Total = sum(REQQTY)) %>%
    filter(!is.na(month)) %>% 
    mutate(month = ymd(month))
  df_items_ts <- data.frame(month = seq.Date(min(df_items$month), 
                                             max(df_items$month), by = 'month'))
  df_items_ts <- df_items_ts %>% left_join(df_items) %>% replace(is.na(.), 0)
  df_items_xts <- xts(df_items_ts$Count, df_items_ts$month)
}

xts_func_mod <- function(df) {
  df_items_xts <- xts(df$REQQTY, df$DEMANDDATE)
}

######################################
#Function for running Croston model
#Load the xgboost model(this is the new model saved on 26 Jun 2020)
bst <- readRDS('data/bst2.Rds')


#msl available df

# msl_df <- df %>% filter(!is.na(MSL), !is.na(Eqpt)) %>% group_by(Eqpt, ItemCode, ItemDesc) %>% 
#   distinct(Eqpt, ItemCode, ItemDesc, .keep_all = TRUE) %>% 
#   select(Eqpt, ItemCode, ItemDesc, MSL, LATESTQTY)
# 
# ship_spends <- df %>% select(ShipName, DEMANDDATE, Eqpt, INR, IssuedQty) %>% 
#   mutate(Total = INR * IssuedQty)
# x <- ship_spends %>% group_by(ShipName) %>% 
#   filter(DEMANDDATE > as.Date('2018-12-31')) %>% 
#   summarise(GrandTotal = sum(INR, na.rm = T)) %>% 
#   arrange(desc(GrandTotal))
