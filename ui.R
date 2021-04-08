# secure_app(
tagList(
  useShinyalert(),
  useShinyjs(),
  # useShinyFeedback(),
  useShinydashboard(),
  use_waiter(), #include_js = FALSE
  waiter_show_on_load(html = spin_fading_circles()),
  
  tags$head(
    # tags$link(rel = "stylesheet",
    #           type = "text/css",
    #           href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
    tags$link(rel = "icon",
              type = "image/png",
              href = "images/logo_icon.png"),
    tags$link(rel = "stylesheet",
              type = "text/css",
              href = "plugins/carousel.css"),
    tags$script(src = "plugins/scripts.js"),
    tags$script(src = "plugins/holder.js")
  ),


  navbarPage(
    title = div(
      tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:left\"><img src=\"images/navy.jpeg\" alt=\"alt\" style=\"float:left;width:140px;height:120px;padding-top:1px;padding-right:50px;\"></div>');
    console.log(header)")),
# tags$script(HTML("var header = $('.navbar > .container-fluid');
# header.append('<div style=\"float:left\"><img src=\"images/dahra_latest.png\" alt=\"alt\" style=\"float:left;width:150px;height:120px;padding-top:1px;padding-right:50px;\"></div>');
#     console.log(header)")),
tags$script(HTML("var header = $('.navbar> .container-fluid');
                       header.append('<div style=\"padding-right:10px;\"><h4 class=\"navbar-title\">Weapons Inventory & Analytics System</h4><h5 class=\"navbar-title\">Data driven spares management</h5></div>');
                       console.log(header)"))
    ),
    id = 'all',
    windowTitle = 'WIAS',
    theme = "style/style.css",
    fluid = TRUE,
    # position = c("fixed-top"),
    collapsible = TRUE,
    source('ui/ui_home.R', local = TRUE)$value,
    source('ui/ui_Demands.R', local = TRUE)$value,
    source('ui/ui_spares_consumption.R', local = TRUE)$value,
    source('ui/ui_predictive_modeling.R', local = TRUE)$value
    # source('ui/ui_btp.R', local = TRUE)$value,
    # source('ui/ui_qp.R', local = TRUE)$value
  )


)
# add image on top ?
# tags_top = 
#   tags$div(
#     tags$h4("Welcome to the Training Management System of", style = "align:center"),
#     tags$h4("Qatar Emiri Navy’s Naval Academy", style = "align:center"),
#     #   div(style="display: inline-block;vertical-align:top;",
#     #       tags$img(
#     #         src = "img/dahralogo.png", width = 100
#     #       )),
#     #   div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
#     #   div(style="display: inline-block;vertical-align:top;",
#     #       tags$img(
#     #         src = "img/navaclogo.png", width = 100
#     #       )
#     # )
#   ),
# # add information on bottom ?
# tags_bottom = tags$div(
#   tags$p(
#     "For any questions, please  contact ",
#     tags$a(
#       href = "mailto:psinha@dahraglobal.com?Subject=Shiny%20aManager",
#       target="_top", "administrator"
#     )
#   )
# ),
# # change auth ui background ?
# # https://developer.mozilla.org/fr/docs/Web/CSS/background
# background  = "no-repeat center/100% url('images/t2.png')",  #"url('img/testbg.png') no-repeat;"
# 
# enable_admin = TRUE
# )


# shinyUI(navbarPage(title = "Training Management System",
#                    theme = "style/style.css",
#                    footer = includeHTML("footer.html"),
#                    fluid = TRUE,
#                    collapsible = TRUE,
# 
#                    # ----------------------------------
#                    # tab panel 1 - Home
#                    tabPanel("Home",
#                             includeHTML("home.html"),
#                             tags$script(src = "plugins/scripts.js"),
#                             tags$head(
#                               tags$link(rel = "stylesheet",
#                                         type = "text/css",
#                                         href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
#                               tags$link(rel = "icon",
#                                         type = "image/png",
#                                         href = "images/logo_icon.png")
#                             )
#                    ),
#                    tabPanel("About",
#                             includeHTML("about.html"),
#                             shinyjs::useShinyjs(),
#                             tags$head(
#                                 tags$link(rel = "stylesheet",
#                                           type = "text/css",
#                                           href = "plugins/carousel.css"),
#                                 tags$script(src = "plugins/holder.js")
#                             ),
#                             tags$style(type="text/css",
#                                        ".shiny-output-error { visibility: hidden; }",
#                                        ".shiny-output-error:before { visibility: hidden; }"
#                             )
#                    )
# 
# ))

