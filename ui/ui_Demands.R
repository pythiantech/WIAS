tabPanel(
  title = "Demands",
  icon = icon('list'),
  tabsetPanel(
    id = 'demands_tabsetpanel',
    tabPanel(
      title = 'Demands',
      icon = icon('list'),
      fixedRow(
        column(
          4,
          div(
            class = "section_box",
            div(
              class = "section_box_title",
              "Demands Raised - Ship Class"
            ),
            div(
              class = "section_box_body",
              plotlyOutput('ship_demands',height = '800px')
            )
          )
        ),
        column(
          4,
          div(
            class = "section_box",
            div(
              class = "section_box_title",
              "Demands Raised - By Ship"
            ),
            div(
              class = "section_box_body",
              plotlyOutput('ship_donut',height = '800px')
            )
          )
        ),
        column(
          4,
          div(
            class = "section_box",
            div(
              class = "section_box_title",
              "Demands Raised - Equipment"
            ),
            div(
              class = "section_box_body",
              plotlyOutput('equipment',height = '800px')
            )
          )
        )
      ),
      ##################################
      fixedRow(
        column(
          9,
          div(
            class = "section_box",
            div(
              class = "section_box_title",
              "Demands Raised"
            ),
            div(
              class = "section_box_body",
              DTOutput('demands_table')
            )
          )
          
        ),
        ################
        column(
          3,
          div(
            class = "section_box",
            div(
              class = "section_box_title",
              "Filters"
            ),
            div(
              class = "section_box_body",
              uiOutput('ship_class'),
              uiOutput('ship_name'),
              uiOutput('equip_name')
            )
          )
        )
      )
    ),
    tabPanel(
      title = 'Demand Health',
      icon = icon('heartbeat'),
      fixedRow(
        column(
          3,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Ship Demands'
            ),
            div(
              class = 'section_box_body',
              p('Given the number of demands being raised by various ships on different equipment,
                a rough estimate on the health of a system on a particular ship can be estimated.
                this may find use in selection of the ideal platform suitable for a particular mission.
                You can use the filters below to filter the data as per your requirement.'),
              br(), br(),
              dateRangeInput('date_hm', 'Select date range',
                             start = Sys.Date() - 365,
                             end = Sys.Date()),
              hr(),
              uiOutput('class_of_ship'),
              
              hr(),
              uiOutput('ship_hm'),
              hr(),
              uiOutput('eqpt_hm')
            )
          )
        ),
        column(
          9,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              ''
            ),
            div(
              class = 'section_box_body',
              highchartOutput('heatmap',height = 800)
            )
          )
        )
      )
    ),
    
    
    tabPanel(
      title = 'Compliance',
      icon = icon('pie-chart'),
      fluidRow(
        column(
          3,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Compliance Overview',
            ),
            div(
              class = 'section_box_body',
              p("Please select the date selection inputs below
                to choose the time frame over which to view compliance of demands issued
                to various units."),
              br(), hr(),
              dateRangeInput('demand_date',
                             "Filter by Dates", 
                             start = (Sys.Date() - 365), 
                             end = Sys.Date() ),
              hr(),
              selectInput('type','Select by Ship/Equipment',
                          choices = c('ShipName','Eqpt')),
              hr(),
              uiOutput('comp_type'),
              hr(),
              uiOutput('spares_category')
              
            )
          )
        ), 
        column(
          9,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Compliance (Ship/Equipment)'
            ),
            div(
              class = 'section_box_body',
              
              fluidRow(
                column(6, highchartOutput('compliance')),
                column(6, highchartOutput('depot_compliance'))
              ),
              DTOutput('selected_DT')
            )
          )
        )
      )
      
      
    ),
    tabPanel(
      title = 'Survey Analysis',
      icon = icon('bar-chart'),
      fluidRow(
        column(
          3,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Survey Details Selection'
            ),
            div(
              class = 'section_box_body',
              p("Please select the date selection inputs below
                to choose the time frame over which to view survey of items undertaken
                by various units."),
              br(), hr(),
              dateRangeInput('survey_date',
                             "Filter by Dates", 
                             start = (Sys.Date() - 365), 
                             end = Sys.Date() ),
              hr(),
              selectInput('survey_type','Select by Ship/Equipment',
                          choices = c('ShipName','Eqpt')),
              hr(),
              uiOutput('sur_comp_type'),
              hr()
              # pickerInput('category', 'Select Demand Category',
              #             choices = levels(as.factor(df()$CATEGORY)),
              #             selected = levels(as.factor(df()$CATEGORY)),
              #             multiple = TRUE)
            )
          )
        ),
        column(
          9,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Survey Details'
            ),
            div(
              class = 'section_box_body',
              fluidRow(
                column(6, highchartOutput('survey_type_chart')),
                column(6, highchartOutput('pts'))
              ),
              DTOutput('selected_DT_survey')
            )
          )
        )
      )
    ),
    tabPanel(
      title = 'Demand Periodicity',
      icon = icon('clock-o'),
      fluidRow(
        column(
          3,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Periodictity Parameter'
            ),
            div(
              class = 'section_box_body',
              p('Select the slider to view the periodicity of demands. For example,
                if you are interested in viewing items that have only a single unique
                demand, select the slider value as 1.'),
              sliderInput('demand_slider', 'Select number of demands',
                          min = 1, max = 100, value = 2)
            )
          )
        ),
        column(
          9,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Demand Details'
            ),
            div(
              class = 'section_box_body',
              DTOutput('dem_analysis')
            )
          )
        )
      )
    ),
    tabPanel(
      title = "Ship Summary",
      icon = icon('ship'),
      fluidRow(
        column(
          3,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Ship Selection'
            ),
            div(
              class = 'section_box_body',
              p('Select ship name from the drop down below to get a ship-wise summary
                on the demands raised by the selected ship'),
              uiOutput('shipsummary')
              
            )
          )
        ),
        column(
          9,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Demands Breakdown'
            ),
            div(
              class = 'section_box_body',
              fluidRow(
                infoBox(
                  title = "Total Demands",
                  value = uiOutput('total_demands'),
                  color = 'green',
                  icon = icon('calendar'),
                  fill = TRUE
                ),
                infoBox(
                  title = "Most Demanded Equipment",
                  value = uiOutput('most_demanded_eqpt'),
                  color = 'yellow',
                  icon = icon('line-chart'),
                  fill = TRUE
                ),
                infoBox(
                  title = "Most Demanded Item",
                  value = uiOutput('most_demanded_item'),
                  color = 'blue',
                  icon = icon('line-chart'),
                  fill = TRUE
                )
              ),
              hr(),
              DTOutput('ship_summary_abnormal')
            )
          )
        )
      )
    )
  )
)