observeEvent(input$generate_predictions, {
  req(input$ard_date)
  ard <- df() %>% filter(DEMANDDATE >= input$ard_date[1] & DEMANDDATE <= input$ard_date[2],
                       !is.na(ARDID)) %>%
    select(DEMANDDATE, ShipName, Eqpt, ItemCode, ItemDesc, DUESIN,
           DUES_OUT, STOCK_HELD, MSL, Cost= INR, PQFINAL)
  
  actual_consumption <- df() %>% filter(DEMANDDATE > input$ard_date[2] & DEMANDDATE <= (input$ard_date[2] + 365),
  ) %>%
    group_by(Eqpt, ItemCode, ItemDesc) %>% summarise(Actual_Consumption = sum(REQQTY))
  
  ard <- ard %>% left_join(actual_consumption)
  ard$Actual_Consumption[is.na(ard$Actual_Consumption)] <- 0
  waiter_show(
    tagList(
      spin_dual_ring(),
      'Predicting...'
    )
  )
  croston_predict <- function(eqpt, code, desc, max_month) {
    df_items <- df() %>% filter(Eqpt == eqpt, ItemCode == code, ItemDesc == desc) %>% 
      group_by(month = floor_date(DEMANDDATE, unit = "month")) %>%
      summarise(Count = n(), Total = sum(REQQTY)) %>%
      filter(!is.na(month)) %>% 
      mutate(month = ymd(month)) %>% filter(month <= max_month)
    
    df_items_ts <- data.frame(month = seq.Date(min(df_items$month), 
                                               max(df_items$month), by = 'month'))
    df_items_ts <- df_items_ts %>% left_join(df_items) %>% replace(is.na(.), 0)
    df_items_xts <- xts(df_items_ts$Total, df_items_ts$month)
    df_items_ts2 <- ts(df_items_xts, 12)
    
    interval_series <- which(df_items_ts$Total>0)
    interval_series <- interval_series - lag(interval_series)
    mean_interval <- mean(interval_series, na.rm = TRUE)
    sd_interval <- sd(interval_series, na.rm = TRUE)
    mean_qty <- mean(df_items_ts$Total, na.rm = TRUE)
    sd_qty <- sd(df_items_ts$Total, na.rm = TRUE)
    
    test_df <- data.frame(mean_interval = mean_interval,
                          sd_interval = sd_interval,
                          mean_qty = mean_qty,
                          sd_qty = sd_qty)
    alpha <- predict(bst, as.matrix(test_df, outputmargin = TRUE))
    if(alpha > 1) alpha <- 1 #Added on 26 Jun 2020
    model <- croston(df_items_ts2, h = 12,alpha = alpha)
    print(list(round(sum(as.numeric(model$mean)))))
    result <- list(round(sum(as.numeric(model$mean))), df_items_xts, df_items_ts)
    return(result)
  }
  ard <- ard %>% rowwise %>% 
    mutate(Predicted_Demand = unlist(croston_predict(Eqpt, 
                                                     ItemCode, 
                                                     ItemDesc,
                                                     floor_date(input$ard_date[2], unit = 'month'))[1]),
           Diff1 = abs(PQFINAL - Actual_Consumption), Diff2 = abs(Actual_Consumption - Predicted_Demand),
           Predicted_Budget = Predicted_Demand * Cost) #%>% 
  # arrange(Diff2)
  waiter_hide()
  rvs$ard <- ard
  
})

output$predicted_output <- renderDT({
  req(rvs$ard)
  # saveRDS(rvs$ard, 'ard.Rds')
  datatable(rvs$ard, rownames = FALSE, extensions = 'Buttons', filter = 'top',
            options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                           dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'print',I('colvis')),
                           pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')),
                           paging = T)) %>% 
    formatCurrency("Cost", currency = "", interval = 3, 
                   mark = ",") %>% 
    formatCurrency("Predicted_Budget", currency = "", interval = 3, 
                   mark = ",")
})

output$item_dygraph <- renderDygraph({
  x <- req(rvs$ard)
  req(input$ard_date)
  sel_row <- req(input$predicted_output_rows_selected)
  df_items <- df() %>% filter(Eqpt == x$Eqpt[sel_row], ItemCode == x$ItemCode[sel_row]) %>%
    filter(ItemDesc == x$ItemDesc[sel_row]) %>%
    group_by(month = floor_date(DEMANDDATE, unit = "month")) %>%
    summarise(Count = n(), Total = sum(REQQTY)) %>%
    filter(!is.na(month)) %>%
    mutate(month = ymd(month)) %>% filter(month <= floor_date(input$ard_date[2], unit = 'month'))

  df_items_ts <- data.frame(month = seq.Date(min(df_items$month),
                                             max(df_items$month), by = 'month'))
  df_items_ts <- df_items_ts %>% left_join(df_items) %>% replace(is.na(.), 0)
  df_items_xts <- xts(df_items_ts$Total, df_items_ts$month)
  
  # df_items_ts <- unlist(croston_predict(x$Eqpt[sel_row], x$ItemCode[sel_row], x$ItemDesc[sel_row])[3])
  # print(df_items_ts)
  #Time series object
  start_freq <- c(year(df_items_ts$month[1]), month(df_items_ts$month[1]))
  end_freq <- c(year(df_items_ts$month[nrow(df_items_ts)]), month(df_items_ts$month[nrow(df_items_ts)]))
  myts <- ts(df_items_ts$Total, start = start_freq, end = end_freq, frequency = 12)

  # rvs_model$ts <- myts
  dygraph(df_items_xts, main = 'Required Quantity in Demands', ylab = 'Req Qty') %>%
    dySeries("V1", label = "REQ_QTY") %>%
    dyRangeSelector() %>%
    dyHighlight(highlightSeriesOpts = list(strokeWidth = 1.5)) %>%
    dyOptions(fillGraph = TRUE, drawGrid = TRUE)
})

output$eqpt_ard <- renderHighchart({
  req(rvs$ard)
  x <- rvs$ard %>% group_by(Eqpt) %>% summarise(Demands = n()) %>% 
    arrange(desc(Demands)) %>% top_n(10)
  
  highchart() %>% 
    hc_chart(type = "column") %>% 
    hc_title(text = "Equipment-wise Summary") %>% 
    hc_subtitle(text = 'The plot shows you the top 10 equipment which have the 
                highest number of demands') %>% 
    hc_xAxis(categories = x$Eqpt) %>% 
    hc_add_series(data = x$Demands,
                  name = "Demands")
})

output$ship_ard <- renderHighchart({
  req(rvs$ard)
  x <- rvs$ard %>% group_by(ShipName) %>% summarise(Demands = n()) %>% 
    arrange(desc(Demands)) %>% top_n(10)
  
  highchart() %>% 
    hc_chart(type = "column") %>% 
    hc_title(text = "Ship-wise Summary") %>% 
    hc_subtitle(text = 'The plot shows you the top 10 ships which have the 
                highest number of demands') %>% 
    hc_xAxis(categories = x$ShipName) %>% 
    hc_add_series(data = x$Demands,
                  name = "Demands")
})