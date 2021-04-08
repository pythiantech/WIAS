
# For WIAS-II -------------------------------------------------------------

observeEvent(input$view_demads, {
  updateTabsetPanel(session, 'all', 'Demands')
  updateTabsetPanel(session, 'demands_tabsetpanel', 'Demands')
})
#Create UIs for Demands tab
##############################################################################
output$ship_class <- renderUI({
  pickerInput('ship_class', "Select Ship Class", choices = levels(as.factor(df()$ShipClass)), multiple = TRUE,
              selected = levels(as.factor(df()$ShipClass)),
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$ship_name <- renderUI({
  req(input$ship_class)
  pickerInput('ship_name', "Select Ship", choices = levels(as.factor(df()$ShipName[df()$ShipClass %in% input$ship_class])), 
              multiple = TRUE,
              selected = levels(as.factor(df()$ShipName[df()$ShipClass %in% input$ship_class])),
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$class_of_ship <- renderUI({
  pickerInput('ship_class_hm', 'Select class of ship',
              choices = levels(as.factor(df()$ShipClass)),
              multiple = TRUE,
              selected = levels(as.factor(df()$ShipClass[df()$ShipClass == 'K'])),
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$spares_category <- renderUI({
  pickerInput('category', 'Select Demand Category',
              choices = levels(as.factor(df()$CATEGORY)),
              selected = levels(as.factor(df()$CATEGORY)),
              multiple = TRUE)
})

output$equip_name <- renderUI({
  req(input$ship_name)
  pickerInput('equip_name', "Select Equipment", choices = levels(as.factor(df()$Eqpt[df()$ShipName %in% input$ship_name])),
              multiple = TRUE,
              selected = levels(as.factor(df()$Eqpt[df()$ShipName %in% input$ship_name])),
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$shipsummary <- renderUI({
  pickerInput('ship_summary','Select Ship', choices = levels(as.factor(df()$ShipName)),
              multiple = FALSE,
              selected = 'KULISH',
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})
##############################################################################
#Render table output

output$demands_table <- renderDT({
  req(input$ship_name)
  req(input$equip_name)
  # new_rvs$df <- df()
  # print(str(new_rvs$df))
  # print(new_rvs$df$CATEGORY[new_rvs$df$DEMANDID == 66456])
  # print(head(df()))
  dem_tbl <- df() %>% filter(ShipName %in% input$ship_name) %>% 
    filter(Eqpt %in% input$equip_name)
  
  datatable(dem_tbl, extensions = 'Buttons', rownames = FALSE, filter = 'top',
            options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                           dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'print',I('colvis')),
                           pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')),
                           paging = T))
  
})

##############################################################################
#Reactive values
rvs <- reactiveValues()
#Render ship plot
output$ship_demands <- renderPlotly({
  g <- df() %>% filter(!is.na(ShipClass)) %>% filter(!is.na(Eqpt)) %>% count(ShipClass) %>% arrange(desc(n)) %>%
    ggplot(aes(x=reorder(ShipClass,n), y = n, fill = n,
               text = paste("Count:", n))
    ) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    scale_fill_gradient2_tableau() +
    # scale_fill_gradient(low = "blue", high = "steelblue") +
    labs(
      title = "Demands Raised by Class of Ship",
      subtitle = "Use the slider to select the top ship classes with respect to the maximum number of demands raised",
      x = NULL,
      y = "Count"
    ) + 
    theme(legend.position = "none") 
    # theme_minimal(base_size=9, base_family="Roboto") +
    # theme(plot.subtitle = element_text(color="#666666"),
    #       plot.title = element_text(family="Roboto-Bold",size = 12),
    #       plot.caption = element_text(color="#AAAAAA", size=6))
  x <- ggplot_build(g)
 
  rvs$ship_values <- x$layout$panel_params[[1]]$y$breaks
  # print(x$layout$panel_params[[1]])
  ggplotly(
    g,
    tooltip = c("text"), source = "base"
  ) 
})

# output$click <- renderPrint({
#   d <- event_data("plotly_click", source = "base")
#   if (is.null(d)) "Click events appear here (double-click to clear)" else d
# })

#######################################################
#Donut plot
output$ship_donut <- renderPlotly({
  req(rvs$ship_values)
  d <- event_data("plotly_click", source = "base")
  req(d)
  ship_class_name <- rvs$ship_values[d$y]
  x <- df() %>% filter(ShipClass == ship_class_name) %>% filter(!is.na(Eqpt)) %>% 
    group_by(ShipName) %>% summarise(count=n()) %>% 
    plot_ly(labels = ~ShipName, values = ~count, source = 'donut') %>%
    add_pie(hole = 0.6) %>%
    layout(title = paste0("Distribution of Demands in ",ship_class_name),  showlegend = T,
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  gg <- plotly_build(x)
  
  rvs$donut_values <- gg$x$data[[1]]$labels
  d2 <- event_data("plotly_click", source = "donut")
  rvs$ship_click <- rvs$donut_values[d2$pointNumber + 1]
  x
})

#######################################################
#Equipment plot

output$equipment <- renderPlotly({
  ship_name <- req(rvs$ship_click)
  g <- df() %>% filter(ShipName == ship_name) %>% filter(!is.na(Eqpt)) %>% count(Eqpt) %>% arrange(desc(n)) %>%
    filter(!is.na(Eqpt)) %>% 
    ggplot(aes(x=reorder(Eqpt,n), y = n, fill = n,
               text = paste("Count:", n))
    ) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    # scale_fill_discrete() +
    scale_fill_gradient_tableau() +
    # scale_fill_gradient(low = "blue", high = "steelblue") +
    labs(
      title = paste("Demands Raised by ", ship_name),
      subtitle = "Use the slider to select the top ship classes with respect to the maximum number of demands raised",
      x = NULL,
      y = "Count"
    ) + 
    theme(legend.position = "none")
    # theme_minimal(base_size=9, base_family="Roboto") +
    # theme(plot.subtitle = element_text(color="#666666"),
    #       plot.title = element_text(family="Roboto-Bold",size = 12),
    #       plot.caption = element_text(color="#AAAAAA", size=6))
  x <- ggplot_build(g)
  rvs$equip_values <- x$layout$panel_params[[1]]$y$breaks
  ggplotly(
    g,
    tooltip = c("text"), source = "equip"
  ) 
})

#############################################################################
#Heatmaps
output$ship_hm <- renderUI({
  req(input$ship_class_hm)
  x <- df() %>% filter(ShipClass %in% input$ship_class_hm)
  pickerInput('ship_hm', 'Select Ships',
              choices = levels(as.factor(x$ShipName)),
              multiple = TRUE,
              selected = levels(as.factor(x$ShipName)),
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})
output$eqpt_hm <- renderUI({
  req(input$ship_hm)
  x <- df() %>% filter(ShipName %in% input$ship_hm)
  pickerInput('eqpt_hm', 'Select Equipment',
              choices = levels(as.factor(x$Eqpt)),
              multiple = TRUE,
              selected = c('GARPUN', 'GARPUN-BAL E1','GARPUN-E', 'LADOGA', 'KASU'),
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$heatmap <- renderHighchart({
  req(input$ship_hm)
  req(input$eqpt_hm)
  hmap_df <- df() %>% 
    filter(DEMANDDATE >= input$date_hm[1] & DEMANDDATE <= input$date_hm[2]) %>% 
    filter(ShipName %in% input$ship_hm) %>% 
    filter(Eqpt %in% input$eqpt_hm) %>% 
    group_by(ShipName, Eqpt) %>% summarise(count = n()) %>% ungroup()
  hmap_df <- hmap_df[complete.cases(hmap_df),]
  
  fntltp <- JS("function(){
  return this.series.xAxis.categories[this.point.x] + ' ' +  this.series.yAxis.categories[this.point.y] + ':<br>' +
  Highcharts.numberFormat(this.point.value);
}")
  
  hchart(hmap_df, "heatmap", hcaes(x = ShipName, y = Eqpt, value = count)) %>% 
    hc_colorAxis(stops = color_stops(20, rev(viridis(30))),
                 type = "logarithmic") %>%
    # hc_colorAxis(minColor  = "#E01C19", min = 1, max = 15,
    #              maxColor = "#1AED13", scale = 'logarithmic') %>%
    hc_yAxis(reversed = TRUE, offset = 0, tickLength = 0,
             gridLineWidth = 0, minorGridLineWidth = 0,
             labels = list(style = list(fontSize = "12px"))) %>%
    hc_tooltip(formatter = fntltp) %>%
    # hc_xAxis(plotLines = list(plotline)) %>%
    hc_title(text = "Ship-Class Health") %>% 
    hc_legend(layout = "vertical", verticalAlign = "top",
              align = "right", valueDecimals = 0) %>% 
    hc_size(height = 800) %>% 
    hc_add_theme(hc_theme_smpl())
})


####################################################################################
#####################################################################
#Compliance

output$comp_type <- renderUI({
  req(input$type)
  if(input$type == 'ShipName')
    pickerInput('ship_name_comp','Select Ship Name',
                choices = levels(as.factor(df()$ShipName)),
                selected = levels(as.factor(df()$ShipName)),
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search` = TRUE))
  else
    pickerInput('equip_name_comp','Select Equipment Name',
                choices = levels(as.factor(df()$Eqpt)),
                selected = levels(as.factor(df()$Eqpt)),
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$compliance <- renderHighchart({
  req(input$type)
  ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked', event.point.name);}")
  #The above is from https://stackoverflow.com/questions/48887731/highcharter-click-event-to-filter-data-from-graph
  compl_tbl <- df() %>% select(ShipName, Eqpt, ItemCode, ItemDesc, REQQTY, CATEGORY,
                             IssuedQty, IssueDiff, DEMANDDATE, DepotID) %>% 
    mutate(IsIssued = ifelse(is.na(IssuedQty), 'Not Complied', 
                             ifelse(IssueDiff > 0, 'Partially Complied', 'Complied')))
  if(input$type == 'ShipName') {
    req(input$ship_name_comp)
    x <- compl_tbl %>% filter(ShipName %in% input$ship_name_comp) %>% 
      filter(DEMANDDATE >= input$demand_date[1] & DEMANDDATE <= input$demand_date[2]) %>% 
      filter(CATEGORY %in% input$category) 
    y <- x %>% count(IsIssued) %>% filter(!is.na(n))
    rvs$clicked_ship <- x
    
    highchart() %>%
      hc_add_series(y, hcaes(x = 'IsIssued',y = "n"), type = "pie",
                    dataLabels = list(enabled = TRUE),innerSize = '40%', size = '80%',
                    tooltip = list(pointFormat = paste('{point.y} demands<br/><b>{point.percentage:.1f}%</b>'))) %>%
      hc_plotOptions(series = list(events = list(click = ClickFunction))) %>% 
      hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text = paste("Compliance Graph - ", input$type)) %>%
      hc_subtitle(text = paste("Hover over the pie chart to get the number of demands both
                               as a number as well as a percentage"))
  }
  
  else {
    req(input$equip_name_comp)
    x <- compl_tbl %>% filter(Eqpt %in% input$equip_name_comp) %>% 
      filter(DEMANDDATE >= input$demand_date[1] & DEMANDDATE <= input$demand_date[2]) %>% 
      filter(CATEGORY %in% input$category)
    y <- x %>% count(IsIssued) %>% filter(!is.na(n))
    rvs$clicked_eqpt <- x
    highchart() %>%
      hc_add_series(y, hcaes(x = 'IsIssued',y = "n"), type = "pie",
                    dataLabels = list(enabled = TRUE),innerSize = '40%', size = '80%',
                    tooltip = list(pointFormat = paste('{point.y} demands<br/><b>{point.percentage:.1f}%</b>'))) %>%
      hc_plotOptions(series = list(events = list(click = ClickFunction))) %>% 
      hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text = paste("Compliance Graph - ", input$type)) %>%
      hc_subtitle(text = paste("Hover over the pie chart to get the number of demands both
                               as a number as well as a percentage"))
  }
  
})

output$depot_compliance <- renderHighchart({
  req(input$demand_date)
  compl_tbl <- df() %>% select(ShipName, Eqpt, ItemCode, ItemDesc, REQQTY, CATEGORY,
                             IssuedQty, IssueDiff, DEMANDDATE, DepotID) %>% 
    mutate(IsIssued = ifelse(is.na(IssuedQty), 'Not Complied', 
                             ifelse(IssueDiff > 0, 'Partially Complied', 'Complied')))
  
  if (input$type == 'ShipName'){
    req(input$ship_name_comp)
    x <- compl_tbl %>% filter(ShipName %in% input$ship_name_comp) %>% 
      filter(DEMANDDATE >= input$demand_date[1] & DEMANDDATE <= input$demand_date[2]) %>%
      filter(CATEGORY %in% input$category) %>% 
      group_by(DepotID, IsIssued) %>% summarise(Count = n()) %>% 
      filter(!is.na(IsIssued), !is.na(DepotID))
    
    hchart(x, "column", hcaes(x = DepotID,y = Count, group = IsIssued)) %>%
      hc_title(text = "Compliance by Depot") %>%
      hc_subtitle(text = "The graph shows the compliance of demands issued
                with respect to depots.") %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = FALSE),
        stacking = "normal",
        enableMouseTracking = TRUE)
      ) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_add_theme(hc_theme_smpl())
  }
  
  else {
    req(input$equip_name_comp)
    x <- compl_tbl %>% filter(Eqpt %in% input$equip_name_comp) %>% 
      filter(DEMANDDATE >= input$demand_date[1] & DEMANDDATE <= input$demand_date[2]) %>%
      group_by(DepotID, IsIssued) %>% summarise(Count = n()) %>% 
      filter(!is.na(IsIssued), !is.na(DepotID))
    
    hchart(x, "column", hcaes(x = DepotID,y = Count, group = IsIssued)) %>%
      hc_title(text = "Compliance by Depot") %>%
      hc_subtitle(text = "The graph shows the compliance of demands issued
                with respect to depots.") %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = FALSE),
        stacking = "normal",
        enableMouseTracking = TRUE)
      ) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_add_theme(hc_theme_smpl())
  }
  
  
})

observeEvent(input$Clicked, {
  if(input$type == 'ShipName') {
    output$selected_DT <- renderDT({
      req(rvs$clicked_ship)
      sel_df <- rvs$clicked_ship %>% 
        filter(IsIssued == input$Clicked) 
      datatable(sel_df, rownames = FALSE, extensions = 'Buttons', filter = 'top',
                options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                               dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'print',I('colvis')),
                               pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')),
                               paging = T))
    })
  }
  else {
    output$selected_DT <- renderDT({
      req(rvs$clicked_eqpt)
      sel_df <- rvs$clicked_eqpt %>% 
        filter(IsIssued == input$Clicked) 
      datatable(sel_df, rownames = FALSE, extensions = 'Buttons', filter = 'top',
                options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                               dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'print',I('colvis')),
                               pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')),
                               paging = T))
    })
  }
  
})


####################################################################################
#####################################################################
#Survey Analysis

output$sur_comp_type <- renderUI({
  req(input$survey_type)
  if(input$survey_type == 'ShipName')
    pickerInput('ship_name_survey','Select Ship Name',
                choices = levels(as.factor(survey()$ShipName)),
                selected = levels(as.factor(survey()$ShipName)),
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search` = TRUE))
  else
    pickerInput('equip_name_survey','Select Equipment Name',
                choices = levels(as.factor(survey()$Eqpt)),
                selected = levels(as.factor(survey()$Eqpt)),
                multiple = TRUE,
                options = list(`actions-box` = TRUE, `live-search` = TRUE))
})

output$survey_type_chart <- renderHighchart({
  req(input$survey_type)
  ClickFunction <- JS("function(event) {Shiny.onInputChange('Clicked2', event.point.name);}")
  #The above is from https://stackoverflow.com/questions/48887731/highcharter-click-event-to-filter-data-from-graph
  # compl_tbl <- df %>% select(ShipName, Eqpt, ItemCode, ItemDesc, REQQTY, CATEGORY,
  #                            IssuedQty, IssueDiff, DEMANDDATE, DepotID) %>%
  #   mutate(IsIssued = ifelse(is.na(IssuedQty), 'Not Complied',
  #                            ifelse(IssueDiff > 0, 'Partially Complied', 'Complied')))
  if(input$survey_type == 'ShipName') {
    req(input$ship_name_survey)
    x <- survey() %>% filter(ShipName %in% input$ship_name_survey) %>%
      filter(SURVEYDATE >= input$survey_date[1] & SURVEYDATE <= input$survey_date[2])
    y <- x %>% count(SURVEYTYPE) %>% filter(!is.na(n))
    rvs$clicked_ship_survey <- x

    highchart() %>%
      hc_add_series(y, hcaes(x = 'SURVEYTYPE',y = "n"), type = "pie",
                    dataLabels = list(enabled = TRUE),innerSize = '40%', size = '80%',
                    tooltip = list(pointFormat = paste('{point.y} items<br/><b>{point.percentage:.1f}%</b>'))) %>%
      hc_plotOptions(series = list(events = list(click = ClickFunction))) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text = paste("Survey Type Graph - ", input$survey_type)) %>%
      hc_subtitle(text = paste("Hover over the pie chart to get the number of items surveyed both
                               as a number as well as a percentage"))
  }

  else {
    req(input$equip_name_survey)
    x <- survey() %>% filter(Eqpt %in% input$equip_name_survey) %>%
      filter(SURVEYDATE >= input$survey_date[1] & SURVEYDATE <= input$survey_date[2])
    y <- x %>% count(SURVEYTYPE) %>% filter(!is.na(n))
    rvs$clicked_eqpt_survey <- x
    highchart() %>%
      hc_add_series(y, hcaes(x = 'SURVEYTYPE',y = "n"), type = "pie",
                    dataLabels = list(enabled = TRUE),innerSize = '40%', size = '80%',
                    tooltip = list(pointFormat = paste('{point.y} items<br/><b>{point.percentage:.1f}%</b>'))) %>%
      hc_plotOptions(series = list(events = list(click = ClickFunction))) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_title(text = paste("Survey Type Graph - ", input$survey_type)) %>%
      hc_subtitle(text = paste("Hover over the pie chart to get the number of items surveyed both
                               as a number as well as a percentage"))
  }

})
 
output$pts <- renderHighchart({
  req(input$survey_date)
  # compl_tbl <- df %>% select(ShipName, Eqpt, ItemCode, ItemDesc, REQQTY, CATEGORY,
  #                            IssuedQty, IssueDiff, DEMANDDATE, DepotID) %>%
  #   mutate(IsIssued = ifelse(is.na(IssuedQty), 'Not Complied',
  #                            ifelse(IssueDiff > 0, 'Partially Complied', 'Complied')))

  if (input$survey_type == 'ShipName'){
    req(input$ship_name_survey)
    x <- survey() %>% filter(ShipName %in% input$ship_name_survey) %>%
      filter(SURVEYDATE >= input$survey_date[1] & SURVEYDATE  <= input$survey_date[2]) %>%
      # filter(CATEGORY %in% input$category) %>%
      group_by(SURVEYTYPE, TYPE) %>% summarise(Count = n()) %>%
      filter(!is.na(SURVEYTYPE), !is.na(TYPE))

    hchart(x, "column", hcaes(x = TYPE,y = Count, group = SURVEYTYPE)) %>%
      hc_title(text = "Surveyed Items by PTS/Normal/TYL") %>%
      hc_subtitle(text = "The graph shows the survey of items based on demand types.") %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = FALSE),
        stacking = "normal",
        enableMouseTracking = TRUE)
      ) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_add_theme(hc_theme_smpl())
  }

  else {
    req(input$equip_name_survey)
    x <- survey() %>% filter(Eqpt %in% input$equip_name_survey) %>%
      filter(SURVEYDATE >= input$survey_date[1] & SURVEYDATE  <= input$survey_date[2]) %>%
      # filter(CATEGORY %in% input$category) %>%
      group_by(SURVEYTYPE, TYPE) %>% summarise(Count = n()) %>%
      filter(!is.na(SURVEYTYPE), !is.na(TYPE))
    
    hchart(x, "column", hcaes(x = TYPE,y = Count, group = SURVEYTYPE)) %>%
      hc_title(text = "Surveyed Items by PTS/Normal/TYL") %>%
      hc_subtitle(text = "The graph shows the survey of items based on demand types.") %>%
      hc_plotOptions(column = list(
        dataLabels = list(enabled = FALSE),
        stacking = "normal",
        enableMouseTracking = TRUE)
      ) %>%
      hc_xAxis(title = list(text = "")) %>%
      hc_add_theme(hc_theme_smpl())
  }


})
# 
observeEvent(input$Clicked2, {
  if(input$survey_type == 'ShipName') {
    output$selected_DT_survey <- renderDT({
      req(rvs$clicked_ship_survey)
      sel_df <- rvs$clicked_ship_survey %>%
        filter(SURVEYTYPE == input$Clicked2)
      datatable(sel_df, rownames = FALSE, extensions = 'Buttons', filter = 'top',
                options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                               dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'print',I('colvis')),
                               pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')),
                               paging = T))
    })
  }
  else {
    output$selected_DT_survey <- renderDT({
      req(rvs$clicked_eqpt_survey)
      sel_df <- rvs$clicked_eqpt_survey %>%
        filter(SURVEYTYPE == input$Clicked2)
      datatable(sel_df, rownames = FALSE, extensions = 'Buttons', filter = 'top',
                options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                               dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'print',I('colvis')),
                               pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')),
                               paging = T))
    })
  }

})

##############################################################
#Unique demands
output$dem_analysis <- renderDT({
  req(input$demand_slider)
  x <- df() %>% group_by(Eqpt, ItemCode, ItemDesc) %>% count() %>% 
    filter(n == input$demand_slider)
  datatable(x, rownames = FALSE, extensions = 'Buttons', filter = 'top',
            options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                           dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'print',I('colvis')),
                           pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')),
                           paging = T))
})

#############################################################
#Ship Summary Section
#Info Boxes
output$most_demanded_eqpt <- renderText({
  req(input$ship_summary)
  x <- df() %>% filter(ShipName == input$ship_summary) %>% 
    group_by(Eqpt) %>% count() %>% arrange(desc(n))
  paste(x$Eqpt[1], '-', x$n[1])
})
output$most_demanded_item <- renderText({
  req(input$ship_summary)
  x <- df() %>% filter(ShipName == input$ship_summary) %>% 
    group_by(Eqpt, ItemCode, ItemDesc) %>% count() %>% arrange(desc(n))
  paste(x$Eqpt[1], x$ItemCode[1], x$ItemDesc[1], '-', x$n[1])
})
output$total_demands <- renderText({
  req(input$ship_summary)
  x <- df() %>% filter(ShipName == input$ship_summary)
  prettyNum(nrow(x))
})

#DT
output$ship_summary_abnormal <- renderDT({
  req(input$ship_summary)
  abnormal <- df() %>% filter(ShipName == input$ship_summary) %>%
    select(Eqpt, ItemCode, ItemDesc, DEMANDDATE, DEMANDNO,
           REQQTY) %>% filter(!is.na(ItemCode),
                                        ItemCode %!in% c("-", " -", "_", "", " ", "--",
                                                         "----", "  -", "NIL", "NA", "N/A")) %>%
    arrange(Eqpt, ItemCode, ItemDesc, DEMANDDATE) %>%
    group_by(Eqpt, ItemCode, ItemDesc) %>% mutate(DEMANDDATE = date(DEMANDDATE),
                                                  lag_date = lag(DEMANDDATE, order_by = Eqpt),
                                                  date_diff = difftime(DEMANDDATE, lag_date,
                                                                       units = "days"))
  abnormal$DEMTYPE <- ifelse(abnormal$date_diff <= 30, 'Abnormal', '')
  x <- abnormal %>% select(-lag_date, -date_diff)
  # # 
  Threshold1 <- 100
  Threshold2 <- 60
  # 
  datatable(x, extensions = "Buttons", rownames = FALSE,
            filter = "top", options = list(deferRender = TRUE,
                                           scroller = FALSE, scrollX = TRUE, dom = "Blfrtip",
                                           buttons = c("copy", "csv", "excel", "print",
                                                       I("colvis")), pageLength = 10,
                                           lengthMenu = list(c(10,20, 50, -1), list("10", "20", "50","All")), paging = T)) %>%
  formatStyle('DEMTYPE', fontWeight = 'Bold',
              backgroundColor = styleEqual(c('Abnormal'),
                                           c('red')
              )
  )
  # datatable(x)
})