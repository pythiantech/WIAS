tabPanel(
  title = "Home",
  icon = icon("home"),
  div(
    id = "home_page",
    div(
      id = "home_intro_row",
      img(id = "home_logo", src = "img/IndianNavy.png"), 
      
      div(
        id = "home_intro_text",
        "Welcome to", strong("WIAS"),
        "- Weapon Inventory Analytics System is a user friendly web application for visualizing weapon inventory data from WLMS-NG",
        "to help you make more informed decisions."
        
      )
    ),

    fluidRow(
      column(
        12,
        div(
          class = "section_box",
          id = "section_faq",
          div(
            class = "section_box_title",
            "How does it work?"
          ),
          div(
            class = "section_box_body",
            tags$h3("Demands Section"),
            tags$p(style = "display: inline-block;vertical-align:top; width: 800px;","This section enables the user to view demands raised
                   by a class of ship. On clicking a particular class of ship on the graph, he would then be able to drill down further into
                   demands raised by individual ships in that particular class of ship selected in the previous graph. A further level of
                   granularity is provided by allowing the user to further drill down into the equipment for which a demand is raised.
                   
                   This section also enables the user to view records by smart filters available on the data being displayed in a tabular fashion.
                   Once the user has filtered the data, he can also save the data as a csvor Excel file."),
            div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
            tags$img(style = "display: inline-block;vertical-align:top; width: 500px;",src = "img/Demands.png"),
            div(class = "section_box_divider"),
            
            tags$h3("Spares Consumption"),
            tags$img(style = "display: inline-block;vertical-align:top; width: 500px;",src = "img/Spares.png"),
            div(style = "display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
            tags$p(style = "display: inline-block;vertical-align:top; width: 800px;",
                   "The Spares Consumption tab allows the user to track the consumption of spares for a particular equipment.
                   The user can select different equipment from a drop down menu to view the ships that are demanding spares for that
                   particular equipment. This graph will give a count of the total number of demands raised by a ship for the selected
                   equipment. The user can then drill down further by clicking on the ship's name and getting the item description. The same 
                   will be displayed as a tree map.
                   ")

#             div(class = "section_box_divider"),
#             tags$h3("Trade Flow"),
#             tags$p(style = "display: inline-block;vertical-align:top; width: 800px;",
#                    "A trade area analysis tool which on a real time basis helps the user identify the most profitable region/area to position their vessel in order to maximize revenue.
# 
# The Trade flow comparison of Scorpio as well as competitor fleet would be provided to analyze historical data."),
#             div(style = "display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
#             tags$img(style = "display: inline-block;vertical-align:top; width: 500px;",src = "img/TradeFlow.png"),
#             # tags$video(style = "display: inline-block;vertical-align:top; width: 500px;",src = "img/Trade.mp4",
#             #            type = "video/mp4", autoplay = "",loop="", controls = NA,  width='600px'),
#             div(class = "section_box_divider"),
# 
#             tags$h3("Projects Desk"),
#             tags$img(style = "display: inline-block;vertical-align:top; width: 500px;",src = "img/Projects.png"),
#             # tags$video(style="display: inline-block;vertical-align:top; width: 500px;",src = "img/Projects.mp4",
#             #            type="video/mp4", autoplay = "",loop="", controls = NA,  width='600px'),
#             div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
#             tags$p(style="display: inline-block;vertical-align:top; width: 800px;",
#                    "A Time charter market analysis tool containing an extensive data table and calendar for all vessels which 
#                    are Time chartered in and out. A detailed calendar provides the user with an interactive interface which accentuates 
#                    cardinal dates and option periods for each charter to better keep a track of the same. This tab highlights 
#                    market analysis on time charter revenue by projection on earning trends to analyze time charter performance.")

            # tags$br(),
            # tags$p(style="display: inline-block;vertical-align:top; width: 800px;", "In case you wish to create your own list,
            #         please slect vessels of interest from the 'Select vessels to create custom list' drop dwon option. Remeber
            #        to give a suitable name to your list and then click on the 'Save Watch List' option."),
            # tags$br(),
            # tags$p(style="display: inline-block;vertical-align:top; width: 800px;", "In case you wish to create your own list,
            #         please slect vessels of interest from the 'Select vessels to create custom list' drop dwon option. Remeber
            #        to give a suitable name to your list and then click on the 'Save Watch List' option.")
            # "You can email us at",
            # tags$a("Pythian Technologies", href = "mailto:dhiraj@pythiantech.com"),

          )
        )
      )

    ),
    fluidRow(column(
      12,
      tags$blockquote(
        class = "pull-right",
        tags$p(tags$em("In God we trust. All others must bring data.")),
        tags$small("W. Edwards Deming")
      )
    ))
  )
)
