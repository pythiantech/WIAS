########################################################### Select equipment filter
output$equip_filter <- renderUI({
  pickerInput("equip_filter", "Select Equipment", 
              choices = levels(as.factor(df()$Eqpt)), multiple = TRUE, 
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$eqpt_cp_ui <- renderUI({
  pickerInput('eqpt_cp', 'Select Input',
              choices = levels(as.factor(df()$Eqpt)),
              multiple = TRUE,
              selected = c('LYNX U2'),
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$ship_cp_ui <- renderUI({
  pickerInput('ship_cp', 'Select Ship',
              choices = levels(as.factor(df()$ShipName)),
              multiple = TRUE,
              selected = 'KULISH',
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$op_ship_ui <- renderUI({
  pickerInput('op_ship', 'Select Ship',
              choices = levels(as.factor(df()$ShipName)),
              multiple = FALSE,
              selected = 'KOLKATA',
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$eqpt_analysis_ui <- renderUI({
  pickerInput('eqpt_analysis', 'Select Equipment',
              choices = levels(as.factor(df_abc()$Eqpt)),
              multiple = TRUE,
              selected = c('VARUNA', 'VARUNA ESM'),
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})
########################################################### Equipment - Ship Plot
rvs_spares <- reactiveValues()
output$equip_demands_ships <- renderPlotly({
  req(input$equip_filter)
  equip_filter <- df() %>% filter(Eqpt %in% input$equip_filter)
  rvs_spares$df <- equip_filter
  g <- equip_filter %>% filter(!is.na(ShipName)) %>% 
    count(ShipName) %>% arrange(desc(n)) %>% ggplot(aes(x = reorder(ShipName, 
                                                                    n), y = n, fill = n, text = paste("Count:", 
                                                                                                      n))) + geom_bar(stat = "identity") + coord_flip() + 
    scale_fill_gradient2_tableau() + # scale_fill_gradient(low = 'blue', high =
    # 'steelblue') +
    labs(title = paste("Demands Raised by Ships for ", 
                       paste(input$equip_filter, collapse = " ")), 
         x = NULL, y = "Count") + theme(legend.position = "none")
  x <- ggplot_build(g)
  rvs_spares$ship_name <- x$layout$panel_params[[1]]$y$breaks
  ggplotly(g, tooltip = c("text"), source = "equip_demands_ship")
})

########################################################### Item - Plot
output$item_plot2 <- renderPlotly({
  req(rvs_spares$ship_name)
  req(rvs_spares$df)
  req(input$equip_filter)
  d <- event_data("plotly_click", source = "equip_demands_ship")
  req(d)
  ship_equip_name <- rvs_spares$ship_name[d$y]
  x <- rvs_spares$df %>% filter(ShipName == ship_equip_name) %>% 
    count(ItemDesc) %>% arrange(desc(n))
  if (nrow(x) > 0) {
    g <- ggplot(x, aes(x = reorder(ItemDesc, n), 
                       y = n, fill = n, text = paste("Count:", 
                                                     n))) + geom_bar(stat = "identity") + 
      coord_flip() + scale_fill_gradient2_tableau() + 
      # scale_fill_gradient(low = 'blue', high =
      # 'steelblue') +
      labs(title = paste("Items demanded by ", ship_equip_name, 
                         " for ", input$equip_filter), x = NULL, 
           y = "Count") + theme(legend.position = "none")
    gg <- plotly_build(g)
    
    # rvs$donut_values <- gg$x$data[[1]]$labels d2 <-
    # event_data('plotly_click', source = 'donut')
    # rvs$ship_click <- rvs$donut_values[d2$pointNumber
    # + 1]
    ggplotly(g, tooltip = c("text"), source = "item_plot")
  }
})

output$item_plot <- renderHighchart({
  ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked3', event.point.name);}")
  req(rvs_spares$ship_name)
  req(rvs_spares$df)
  req(input$equip_filter)
  d <- event_data("plotly_click", source = "equip_demands_ship")
  req(d)
  ship_equip_name <- rvs_spares$ship_name[d$y]
  x <- rvs_spares$df %>% filter(ShipName == ship_equip_name) %>% 
    count(ItemDesc) %>% arrange(desc(n))
  rvs$treemap <- rvs_spares$df %>% filter(ShipName == ship_equip_name)
  if (nrow(x) > 0) {
    x %>% hchart(type = "treemap", hcaes(x = ItemDesc, 
                                         value = n, color = n)) %>% 
      hc_plotOptions(series = list(events = list(click = ClickFunction))) %>%
      hc_title(text = paste("Treemap of Items Demanded by ", ship_equip_name)) %>% 
      hc_legend(layout = "vertical", verticalAlign = "top", align = "right", valueDecimals = 0) %>% 
      hc_add_theme(hc_theme_smpl())
  }
  
})

observeEvent(input$Clicked3, {
  req(rvs$treemap)
  output$treemap_df <- renderDT({
    x <- rvs$treemap %>% filter(ItemDesc == input$Clicked3)
    datatable(x, extensions = "Buttons", rownames = FALSE, 
              filter = "top", options = list(deferRender = TRUE, 
                                             scroller = FALSE, scrollX = TRUE, dom = "Blfrtip", 
                                             buttons = c("copy", "csv", "excel", "print", 
                                                         I("colvis")), 
                                             pageLength = 10, 
                                             lengthMenu = list(c(10,20, 50, -1), 
                                                               list("10", "20", "50", "All")), paging = T))
  })
})

output$common_dem <- renderHighchart({
  req(input$equip_filter)
  x <- df() %>% filter(Eqpt == input$equip_filter) %>% count(ItemDesc) %>% arrange(desc(n))
  x %>% hchart(type = "treemap", hcaes(x = ItemDesc, 
                                       value = n, color = n)) %>% 
    # hc_plotOptions(series = list(events = list(click = ClickFunction))) %>%
    hc_title(text = paste("Demands Commonality - ", input$equip_filter)) %>% 
    hc_legend(layout = "vertical", verticalAlign = "top", align = "right", valueDecimals = 0) %>% 
    hc_add_theme(hc_theme_smpl())
})
# output$msl <- renderDT({
#   req(input$equip_filter)
#   df_equip <- df %>% filter(Eqpt %in% input$equip_filter) %>% 
#     select(ShipName, ItemDesc, MSL, LATESTQTY, 
#            UNITCOST, CurrencyType, INR)
#   
#   datatable(df_equip, extensions = "Buttons", rownames = FALSE, 
#             filter = "top", options = list(deferRender = TRUE, 
#                                            scroller = FALSE, scrollX = TRUE, dom = "Blfrtip", 
#                                            buttons = c("copy", "csv", "excel", "print", 
#                                                        I("colvis")), 
#                                            pageLength = 10, 
#                                            lengthMenu = list(c(10,20, 50, -1), 
#                                                              list("10", "20", "50", "All")), paging = T))
# })

##################################################################### Identical Spares

output$identical_spares <- renderDT({
  if (input$spare != "") {
    x <- inv_tbl() %>% filter(Reduce(`&`, lapply(strsplit(input$spare, 
                                                        " ")[[1]], grepl, clean, ignore.case = T))) %>% 
      select(Eqpt, ItemCode, ItemDesc)
  } else x <- inv_tbl() %>% select(Eqpt, ItemCode, ItemDesc)
  
  datatable(x, rownames = FALSE, extensions = 'Buttons', filter = 'top',
            options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                           dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'print',I('colvis')),
                           pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')),
                           paging = T))
})

##################################################################### 

############################################################################## Inventory Categorization

output$item_analysis <- renderUI({
  req(input$eqpt_analysis)
  req(input$abc_date)
  x <- df_abc() %>% filter(Eqpt %in% input$eqpt_analysis) %>% 
    filter(DEMANDDATE >= input$abc_date[1] & DEMANDDATE <= 
             input$abc_date[2])
  rvs$abc <- x
  pickerInput("item_analysis", "Select Item", choices = levels(as.factor(x$ItemDesc)), 
              multiple = TRUE, selected = levels(as.factor(x$ItemDesc)), 
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$abc_analysis <- renderDT({
  req(rvs$abc)
  req(input$item_analysis)
  req(input$fast_inv)
  req(input$slow_inv)
  x <- rvs$abc %>% filter(!is.na(INR), INR > 0) %>% 
    filter(ItemDesc %in% input$item_analysis) %>% 
    group_by(Eqpt, ItemCode, ItemDesc) %>% summarise(TotalQty = sum(REQQTY, 
                                                                    na.rm = TRUE), ItemCost = mean(INR, na.rm = TRUE), 
    )
  x$TotalCost <- x$TotalQty * x$ItemCost
  x <- x %>% arrange(desc(TotalCost)) %>% filter(TotalCost > 
                                                   0)
  abc <- ABCanalysis(as.numeric(x$TotalCost))
  x$ABC <- c(rep("A", length(abc$Aind)), rep("B", 
                                             length(abc$Bind)), rep("C", length(abc$Cind)))
  
  rvs$abc_plot <- x
  # No demand inventory
  no_dem <- inventories() %>% filter(Eqpt %in% input$eqpt_analysis) %>% 
    filter(ItemDesc %in% input$item_analysis) %>% 
    left_join(x) %>% filter(is.na(TotalQty)) %>% 
    select(Eqpt, ItemCode, ItemDesc) %>% mutate(TotalQty = 0, 
                                                ItemCost = 0, TotalCost = 0, ABC = "Not considered for ABC", 
                                                Count = 0, FSN = "Non Moving")
  x$ItemCost <- as.numeric(x$ItemCost)
  x$TotalCost <- as.numeric(x$TotalCost)
  # x <- bind_rows(x, no_dem) saveRDS(x, 'x.Rds')
  fsn_df <- df_abc() %>% filter(Eqpt %in% input$eqpt_analysis) %>% 
    filter(ItemDesc %in% input$item_analysis) %>% 
    select(-c(DENOMDESC, UNITCOST, CurrencyType, 
              INR)) %>% filter(DEMANDDATE >= input$abc_date[1] & 
                                 DEMANDDATE <= input$abc_date[2]) %>% group_by(Eqpt, 
                                                                               ItemCode, ItemDesc) %>% summarize(Count = n()) %>% 
    arrange(desc(Count)) %>% mutate(FSN = case_when(Count >= 
                                                      input$fast_inv ~ "Fast", 
                                                      Count >= input$slow_inv & 
                                                      Count < input$fast_inv ~ "Slow", 
                                                      Count < input$slow_inv ~ 
                                                      "Non Moving"))
  x <- x %>% left_join(fsn_df)
  x <- bind_rows(x, no_dem)
  x <- x %>% mutate(Category = case_when(((ABC == 
                                             "A" & FSN == "Fast") | (ABC == "A" & FSN == 
                                                                       "Slow") | (ABC == "A" & FSN == "Non Moving") | 
                                            (ABC == "B" & FSN == "Fast") | (ABC == "C" & 
                                                                              FSN == "Fast")) ~ "Category I", ((ABC == "B" & 
                                                                                                                  FSN == "Slow") | (ABC == "B" & FSN == "Non Moving") | 
                                                                                                                 (ABC == "C" & FSN == "Slow")) ~ "Category II", 
                                         ((ABC == "C" & FSN == "Non Moving")) ~ "Category III")) %>% 
    replace_na(list(Category = "Uncategorized"))
  rvs$fsn_abc <- x
  datatable(x, rownames = FALSE, extensions = 'Buttons', filter = 'top',
            options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                           dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'print',I('colvis')),
                           pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')),
                           paging = T)) %>% formatStyle("ABC", 
                                                 backgroundColor = styleEqual(c("A", "B", "C"), 
                                                                              c("#FFA500", "#6CA6CD", "#90EE90"))) %>% 
    formatStyle("FSN", backgroundColor = styleEqual(c("Fast", 
                                                      "Slow", "Non Moving"), c("#FFA500", "#6CA6CD", 
                                                                               "#90EE90"))) %>% formatCurrency("TotalCost", 
                                                                                                               currency = "", interval = 3, mark = ",") %>% 
    formatCurrency("ItemCost", currency = "", interval = 3, 
                   mark = ",")
})

output$abc_plot <- renderPlot({
  x <- req(rvs$abc_plot)
  y <- as.numeric(x$TotalCost)
  ABCanalysisPlot(y)
})

################################### For value boxes
output$a_num <- renderText({
  req(rvs$fsn_abc)
  x <- rvs$fsn_abc %>% group_by(ABC) %>% count()
  prettyNum(x$n[x$ABC == "A"])
})
output$b_num <- renderText({
  req(rvs$fsn_abc)
  x <- rvs$fsn_abc %>% group_by(ABC) %>% count()
  prettyNum(x$n[x$ABC == "B"])
})
output$c_num <- renderText({
  req(rvs$fsn_abc)
  x <- rvs$fsn_abc %>% group_by(ABC) %>% count()
  prettyNum(x$n[x$ABC == "C"])
})

output$fast_num <- renderText({
  req(rvs$fsn_abc)
  x <- rvs$fsn_abc %>% group_by(FSN) %>% count()
  num_dis <- prettyNum(x$n[x$FSN == "Fast"])
  if (length(num_dis) > 0) 
    num_dis else prettyNum(0)
})
output$slow_num <- renderText({
  req(rvs$fsn_abc)
  x <- rvs$fsn_abc %>% group_by(FSN) %>% count()
  num_dis <- prettyNum(x$n[x$FSN == "Slow"])
  if (length(num_dis) > 0) 
    num_dis else prettyNum(0)
})
output$non_num <- renderText({
  req(rvs$fsn_abc)
  x <- rvs$fsn_abc %>% group_by(FSN) %>% count()
  num_dis <- prettyNum(x$n[x$FSN == "Non Moving"])
  if (length(num_dis) > 0) 
    num_dis else prettyNum(0)
})
############################################### FSN Analysis output$fsn_df <- renderDT({
############################################### req(input$eqpt_analysis) req(input$item_analysis)
############################################### req(input$fast_inv) req(input$slow_inv)
############################################### req(input$non_inv) x <- df_abc() %>% filter(Eqpt
############################################### %in% input$eqpt_analysis) %>%
############################################### select(-c(DENOMDESC, UNITCOST, CurrencyType,
############################################### INR)) %>% filter(DEMANDDATE >= input$fsn_date[1]
############################################### & DEMANDDATE <= input$fsn_date[2]) %>%
############################################### group_by(Eqpt, ItemCode, ItemDesc) %>%
############################################### summarize(Count = n()) %>% arrange(desc(Count))
############################################### %>% mutate(FSN = case_when( Count >=
############################################### input$fast_inv ~ 'Fast', Count > input$non_inv &
############################################### Count < input$fast_inv ~ 'Slow', Count <=
############################################### input$slow_inv ~ 'Non Moving' )) datatable(x,
############################################### rownames = FALSE) %>% formatStyle('FSN',
############################################### backgroundColor = styleEqual(c('Fast','Slow','Non
############################################### Moving'), c('#FFA500', '#6CA6CD', '#90EE90'))) })
############################################### Consumption Patterns
output$items_cp <- renderUI({
  req(input$eqpt_cp)
  req(input$date_cp)
  x <- df() %>% filter(Eqpt %in% input$eqpt_cp) %>% 
    filter(DEMANDDATE >= input$date_cp[1] & DEMANDDATE <= 
             input$date_cp[2])
  pickerInput("items_cp", "Select Items", choices = levels(as.factor(x$ItemDesc)), 
              multiple = TRUE, selected = levels(as.factor(x$ItemDesc)), 
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$dygraph_cp <- renderDygraph({
  
  ########################## 
  if (input$selection_cp == "Equipment") {
    req(input$eqpt_cp)
    req(input$date_cp)
    req(input$items_cp)
    df_items <- df() %>% filter(Eqpt %in% input$eqpt_cp) %>% 
      filter(ItemDesc %in% input$items_cp) %>% 
      filter(DEMANDDATE >= input$date_cp[1] & 
               DEMANDDATE <= input$date_cp[2]) %>% 
      group_by(month = floor_date(DEMANDDATE, 
                                  unit = "month")) %>% summarise(Count = n(), 
                                                                 Total = sum(REQQTY)) %>% filter(!is.na(month)) %>% 
      mutate(month = ymd(month))
    ################ 
    split_df <- df() %>% filter(Eqpt %in% input$eqpt_cp) %>% 
      filter(ItemDesc %in% input$items_cp) %>% 
      filter(DEMANDDATE >= input$date_cp[1] & 
               DEMANDDATE <= input$date_cp[2]) %>% 
      select(Eqpt, DEMANDDATE, REQQTY)
    x <- split(split_df, split_df$Eqpt)
    
    test <- x %>% map(~xts_func(.))
    
    eqpt_series <- do.call(cbind, test) %>% replace(is.na(.), 
                                                    0)
    # saveRDS(eqpt_series, "es.Rds")
    dygraph(eqpt_series, main = "Count of Demands", 
            ylab = "Counts") %>% # dySeries('V1', label = 'Count') %>%
      dyRangeSelector() %>% dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.5)) %>% 
      dyOptions(fillGraph = TRUE, drawGrid = TRUE)
  } else {
    req(input$ship_cp)
    req(input$date_cp)
    
    df_items <- df() %>% filter(ShipName %in% input$ship_cp) %>% 
      # filter(ItemDesc %in% input$items_cp) %>%
      filter(DEMANDDATE >= input$date_cp[1] & DEMANDDATE <= 
               input$date_cp[2]) %>% group_by(month = floor_date(DEMANDDATE, 
                                                                 unit = "month")) %>% summarise(Count = n(), 
                                                                                                Total = sum(REQQTY)) %>% filter(!is.na(month)) %>% 
      mutate(month = ymd(month))
    
    
    split_df <- df() %>% filter(ShipName %in% input$ship_cp) %>% 
      filter(DEMANDDATE >= input$date_cp[1] & 
               DEMANDDATE <= input$date_cp[2]) %>% 
      select(ShipName, DEMANDDATE, REQQTY)
    x <- split(split_df, split_df$ShipName)
    
    
    test <- x %>% map(~xts_func(.))
    
    ship_series <- do.call(cbind, test) %>% replace(is.na(.), 
                                                    0)
    # saveRDS(ship_series, 'ss.Rds')
    dygraph(ship_series, main = "Count of Demands", 
            ylab = "Counts") %>% # dySeries('V1', label = 'Count') %>%
      dyRangeSelector() %>% dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
      dyOptions(fillGraph = TRUE, drawGrid = TRUE)
  }
})

################################################################## Abnormal Demands by Equipment
output$abnormal_dems_eqpt <- renderDT({
  req(input$selection_cp)
  req(input$date_cp)
  # req(input$items_cp)
  
  if(input$selection_cp == 'Equipment') {
    eqpt_abnormal <- df() %>% filter(Eqpt %in% input$eqpt_cp) %>% 
      filter(ItemDesc %in% input$items_cp) %>% filter(DEMANDDATE >= 
                                                        input$date_cp[1] & DEMANDDATE <= input$date_cp[2]) %>% 
      select(Eqpt, ItemCode, ItemDesc, DEMANDDATE, 
             ShipName, REQQTY) %>% filter(!is.na(ItemCode), 
                                          ItemCode %!in% c("-", " -", "_", "", " ", "--", 
                                                           "----", "  -", "NIL", "NA", "N/A")) %>% 
      arrange(Eqpt, ItemCode, ItemDesc, DEMANDDATE) %>% 
      group_by(Eqpt, ItemCode, ItemDesc) %>% mutate(DEMANDDATE = date(DEMANDDATE), 
                                                    lag_date = lag(DEMANDDATE, order_by = Eqpt), 
                                                    date_diff = difftime(DEMANDDATE, lag_date, 
                                                                         units = "days"))
    x <- eqpt_abnormal %>% select(-lag_date)
    x <- x %>% mutate(
      Demand_Type = case_when(
        date_diff <=30 ~ 'Abnormal'
      )
    )
    rvs$abnormal_df <- x
    
    datatable(x, extensions = "Buttons", rownames = FALSE, 
              filter = "top", options = list(deferRender = TRUE, 
                                             scroller = FALSE, scrollX = TRUE, dom = "Blfrtip", 
                                             buttons = c("copy", "csv", "excel", "print", 
                                                         I("colvis")), pageLength = 10, lengthMenu = list(c(10, 
                                                                                                            20, 50, -1), list("10", "20", "50", 
                                                                                                                              "All")), paging = T))
  }
  
  else {
    ship_dems <- df() %>% filter(ShipName %in% input$ship_cp) %>% 
    filter(DEMANDDATE >= 
             input$date_cp[1] & DEMANDDATE <= input$date_cp[2]) %>% 
      select(Eqpt, ItemCode, ItemDesc, DEMANDDATE, 
             ShipName, REQQTY) %>% filter(!is.na(ItemCode), 
                                          ItemCode %!in% c("-", " -", "_", "", " ", "--", 
                                                           "----", "  -", "NIL", "NA", "N/A")) %>% 
      arrange(Eqpt, ItemCode, ItemDesc, DEMANDDATE) %>% 
      group_by(Eqpt, ItemCode, ItemDesc) %>% mutate(DEMANDDATE = date(DEMANDDATE), 
                                                    lag_date = lag(DEMANDDATE, order_by = Eqpt), 
                                                    date_diff = difftime(DEMANDDATE, lag_date, 
                                                                         units = "days"))
    x <- ship_dems %>% select(-lag_date)
    x <- x %>% mutate(
      Demand_Type = case_when(
        date_diff <=30 ~ 'Abnormal'
      )
    )
    rvs$abnormal_df <- x
    
    datatable(x, extensions = "Buttons", rownames = FALSE, 
              filter = "top", options = list(deferRender = TRUE, 
                                             scroller = FALSE, scrollX = TRUE, dom = "Blfrtip", 
                                             buttons = c("copy", "csv", "excel", "print", 
                                                         I("colvis")), pageLength = 10, lengthMenu = list(c(10, 
                                                                                                            20, 50, -1), list("10", "20", "50", 
                                                                                                                              "All")), paging = T))
  }
  
})

output$abnormal_df <- renderDT({
  req(rvs$abnormal_df)
  datatable(rvs$abnormal_df, extensions = "Buttons", 
            rownames = FALSE, filter = "top", options = list(deferRender = TRUE, 
                                                             scroller = FALSE, scrollX = TRUE, dom = "Blfrtip", 
                                                             buttons = c("copy", "csv", "excel", "print", 
                                                                         I("colvis")), pageLength = 10, lengthMenu = list(c(10, 
                                                                                                                            20, 50, -1), list("10", "20", "50", 
                                                                                                                                              "All")), paging = T))
})

#########################################
#Op Costing
output$op_df <- renderDT({
  req(input$op_date)
  req(input$op_ship)
  
  time_period_days <- as.numeric(difftime(input$op_date[2], input$op_date[1], units = c('days')))
  op_df <- df() %>% filter(ShipName == input$op_ship, DEMANDDATE >= 
                           input$op_date[1] & DEMANDDATE <= input$op_date[2]) %>% 
    select(Eqpt, ItemCode, ItemDesc, REQQTY, INR) %>% 
    mutate(Total_Cost = REQQTY * INR)
  op_cost_day <- round(sum(op_df$Total_Cost, na.rm = TRUE)/time_period_days, 2)
  rvs$op_cost_day <- op_cost_day
  datatable(op_df, rownames = FALSE, extensions = 'Buttons', filter = 'top',
            options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                           dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'print',I('colvis')),
                           pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')),
                           paging = T))
})

output$op_cost_day <- renderText({
  req(rvs$op_cost_day)
  # x <- rvs$fsn_abc %>% group_by(ABC) %>% count()
  prettyNum(rvs$op_cost_day)
})

#################################
#MSL vs Stock

output$msl_stock <- renderDT({
  datatable(msl_df(), extensions = "Buttons", rownames = FALSE, 
            filter = "top", options = list(deferRender = TRUE, 
                                           scroller = FALSE, scrollX = TRUE, dom = "Blfrtip", 
                                           buttons = c("copy", "csv", "excel", "print", 
                                                       I("colvis")), pageLength = 10, 
                                           lengthMenu = list(c(10,20, 50, -1), list("10", "20", "50","All")), paging = T))
})
