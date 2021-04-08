observeEvent(input$refresh, {
  waiter_show(
    tagList(
      spin_facebook(),
      'Refreshing Data...'
    )
  )
  source('get_data.R')
  df <<- reactive(readRDS('data/df.Rds'))
  inventories <<- reactive('data/inventories.Rds') 
  survey <<- reactive('data/survey.Rds')
  waiter_hide()
})