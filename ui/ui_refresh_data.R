tabPanel(
  title = "Refresh Data",
  icon = icon('refresh'),
  div(
    id = "logo_subheader",
    img(src = "img/IndianNavy.png", height = "100%")
  ),
  div(
    class = 'section_box',
    div(
      class = 'section_box_title',
      'Refresh Data'
    ),
    div(
      class = 'section_box_body',
      
      actionBttn('refresh', 'Refresh Data')
    )
  )
)