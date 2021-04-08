tabPanel(
  title = "Spares Consumption",
  icon = icon('cogs'),
  div(
    id = "logo_subheader",
    img(src = "img/IndianNavy.png", height = "100%")
  ),
  tabsetPanel(
    tabPanel(
      title = "Spares Consumption",
      icon = icon('cogs'),
      
      
      fluidRow(
        column(
          12,
          div(
            class = "section_box",
            div(
              class = "section_box_title",
              "Equipment Consumption"
            ),
            div(
              class = "section_box_body",
              # actionLink('test', label = "Guide d'utilisation", icon = icon('info')),
              uiOutput('equip_filter'),
              fluidRow(
                column(4, highchartOutput('common_dem', height = '600px')),
                column(4, plotlyOutput('equip_demands_ships', height = '600px')),
                column(4, highchartOutput('item_plot',height = '600px'))
              ),
              br(), hr(),
              # DTOutput('msl')
              DTOutput('treemap_df')
            )
          )
        )
      )
    ),

    
    
    tabPanel(
      title = 'Identical Spares',
      icon = icon('clone'),
      fluidRow(
        column(
          3,
         div(
           class = 'section_box',
           div(
             class = 'section_box_title'
           ),
           div(
             class = 'section_box_body',
             p("The pattern number of a number of spares being used in WEDs are of Russian origin
            and have Cyrillic characters. While some of these have been translated to English, a large
               number of them are still using Cyrillic charcaters along with English. This leads to duplication
               of spares."),
             p('The search box  below allows you to search for any spare by 
               entering the characters in English. Based on string distance metric calculations,
               the table on the right hand side will show you the identical spares.'),
             textInput("spare","Type in the pattern number in English to filter the table", NULL)
           )
         )
        ),
        column(
          9,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Matched Output'
            ),
            div(
              class = 'section_box_body',
              DTOutput('identical_spares')
            )
          )
        )
      )
    ),
    ##################################################
    tabPanel(
      title = 'Inventory Categorization',
      icon = icon('list'),
      fluidRow(
        column(
          3,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'ABC/FSN Analysis'
            ),
            div(
              class = 'section_box_body',
              p('This tab allows you to carry out inventory analysis based on ABC and FSN analysis. You 
                can use the drop down options below to select an appropriate date range, type 
                of equipment and the items therein.'),
              h5('Please note that these analyses depend largely on the accuracy and veracity of the 
                 underlying data.'),
              hr(),
              dateRangeInput('abc_date', 'Select Date  Range',
                             start = Sys.Date() - 365,
                             end = Sys.Date()),
              # selectInput('analysis_type', 'Select an analysis',
              #             choices = c('ABC', 'FSN'),
              #             selected = 'ABC'),
              # hr(),
              uiOutput('eqpt_analysis_ui'),
             
              hr(),
              uiOutput('item_analysis'),
              hr(),
              h5('Fast Moving'),
              numericInput('fast_inv', 'No. of Demands', min = 0,
                           max = 50, value = 10),
              hr(),
              h5('Slow Moving'),
              numericInput('slow_inv', 'No. of Demands', min = 0,
                           max = 50, value = 5),
              hr(),
              # h5('Non Moving'),
              # numericInput('non_inv', 'No. of Demands', min = 0,
              #              max = 50, value = 1),
              p('All counts of demands less than *Slow Moving* will be classified as non-moving'),
              hr(),
              h4('Inventory Categories'),
              p('Based on the ABC and FSN analysis, a further classification has been provided to get the
              importance of spares. By 
                default, the following is the classification:'),
              tags$ol(
                tags$li(tags$b('Category I : AF, AS, AN, BF, CF')),
                tags$li(tags$b('Category II : BS, CS')),
                tags$li(tags$b('Category III : CN'))
              )
              
              
            )
          )
        ),
        column(
          9,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Inventory Analysis'
            ),
            div(
              class = 'section_box_body',
              fluidRow(
                infoBox(
                  title = "Count of 'A' Inventory",
                  value = uiOutput('a_num'),
                  color = 'yellow',
                  icon = icon('battery'),
                  fill = TRUE
                ),
                infoBox(
                  title = "Count of 'B' Inventory",
                  value = uiOutput('b_num'),
                  color = 'light-blue',
                  icon = icon('battery-3'),
                  fill = TRUE
                ),
                infoBox(
                  title = "Count of 'C' Inventory",
                  value = uiOutput('c_num'),
                  color = 'green',
                  icon = icon('battery-1'),
                  fill = TRUE
                )
              ),
              fluidRow(
                infoBox(
                  title = "Count of Fast Inventory",
                  value = uiOutput('fast_num'),
                  color = 'yellow',
                  icon = icon('fighter-jet'),
                  fill = TRUE
                ),
                infoBox(
                  title = "Count of Slow Inventory",
                  value = uiOutput('slow_num'),
                  color = 'light-blue',
                  icon = icon('car'),
                  fill = TRUE
                ),
                infoBox(
                  title = "Count of Non Moving Inventory",
                  value = uiOutput('non_num'),
                  color = 'green',
                  icon = icon('bicycle'),
                  fill = TRUE
                )
              ),
                DTOutput('abc_analysis'),
              fluidRow(
                column(
                  6,
                  plotOutput('abc_plot',height = 600,width = 600)
                ),
                column(
                  6
                )
              )
                
              
            )
          )
          
        )
      )
    ),
    tabPanel(
      title = 'Consumption Patterns',
      icon = icon('bar-chart'),
      fluidRow(
        column(
          3,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Consumption'
            ),
            div(
              class = 'section_box_body',
              p('Please select  Ship/Equipment to observe the consumption
            pattern over a period of time'),
              br(), br(),
              selectInput('selection_cp', 'Select Ship/Equipment',
                          choices = c('Equipment', 'Ship'),
                          selected = 'Equipment'),
              hr(),
              dateRangeInput('date_cp', 'Select date range',
                             start = Sys.Date() - 365,
                             end = Sys.Date()),
              conditionalPanel(
                condition = "input.selection_cp == 'Equipment'",
                uiOutput('eqpt_cp_ui'),
                hr(),
                uiOutput('items_cp')
              ),
              conditionalPanel(
                condition = "input.selection_cp == 'Ship'",
                uiOutput('ship_cp_ui')
              )
            )
          )
        ),
        column(
          9,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Patterns'
            ),
            div(
              class = 'section_box_body',
              dygraphOutput('dygraph_cp',height = 600),
              br(), br(),
              DTOutput('abnormal_dems_eqpt'),
              br(),br(),
              DTOutput('msl_stock')
            )
          )
        )
      )
      
    ),
    tabPanel(
      title = 'Operational Costing',
      icon = icon('inr'),
      fluidRow(
        column(
          3,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Operational Running Costs'
            ),
            div(
              class = 'section_box_body',
              p('Operational cost of deployment of a ship takes into account
the cost of weapons/sensors being utilized during deployment. This is calculated based on
                 the cost of spare consumption on a per day basis'),
              br(),
              p('Please select a time frame to calculate operational cost'),
              dateRangeInput('op_date', 'Select Date Range',
                             start = Sys.Date() - 365,
                             end = Sys.Date()),
              br(),
              p('Select name of the ship to calculate op cost'),
              uiOutput('op_ship_ui')
              
            )
          )
        ),
        column(
          9,
          div(
            class = 'section_box',
            div(
              class = 'section_box_title',
              'Breakdown & Summary'
            ),
            div(
              class = 'section_box_body',
              fluidRow(
                infoBox(
                  title = "Op Cost per Day",
                  value = uiOutput('op_cost_day'),
                  color = 'blue',
                  icon = icon('inr'),
                  fill = TRUE
                )
              ),
              DTOutput('op_df')
            )
          )
        )
      )
    )
    
  )
)
