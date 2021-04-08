tabPanel(
  title = 'Predictive ARD Modeling',
  icon = icon('line-chart'),
  div(
    id = "logo_subheader",
    img(src = "img/IndianNavy.png", height = "100%")
  ),
  fluidRow(
    column(
      3,
      div(
        class = 'section_box',
        div(
          class = 'section_box_title',
          'ARD Analysis'
        ),
        div(
          class = 'section_box_body',
          # uiOutput('equip_filter_model'),
          # br(), 
          # uiOutput('items_model'),
          # br(), hr(),
          # radioButtons("method", h4( "Select Predictive Model"),
          #              c("Naive" = "naive",
          #                "Moving Average" = "ma",
          #                "ARIMA" = "ar",
          #                "Neural Network" = "nn",
          #                "Exponential Smoothing" = "es")),
          # br(), hr()
          p('Select a date range over which to generate ARD'),
          dateRangeInput('ard_date','Select Dates',
                         start = '2017-01-01',#Sys.Date() - 365,
                         end = '2017-12-31'),#Sys.Date()),
          hr(),
          p('For demands classified as ARD, we can now generate a prediction using a time 
            series model based on the Croston method. The model takes into account the count
            of demands and the inter-arrival time for the same.'),
          # sliderInput('alpha', 'Smoothing Parameter', min = 0, max = 1, value = 0.1),
          actionBttn('generate_predictions', 'Generate Predictions',
                     color = 'primary'),
          hr()
        )
      )
    ),
    column(
      9,
      div(
        class = 'section_box',
        div(
          class = 'section_box_title',
          'Time Series Analysis'
        ),
        div(
          class = 'section_box_body',
          # dygraphOutput('item_dygraph'),
          # br(), hr(),
          # highchartOutput('decomposition',height = 600),
          # br(), hr(),
          # plotlyOutput('seasonal')
          # DTOutput('ard_df'),
          DTOutput('predicted_output'),
          hr(), br(),
          dygraphOutput('item_dygraph'),
          hr(),
          fluidRow(
            column(6, highchartOutput('eqpt_ard')),
            column(6, highchartOutput('ship_ard'))
          )
        )
        
      )
    )
    
  )
  
  
  
)