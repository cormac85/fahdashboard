library(shiny)
library(shinydashboard)

# Styling
OVERVIEW_PLOT_WIDTH = 9
OVERVIEW_PLOT_HEIGHT = 9

shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Folding@Home Live Stats"),
  shinydashboard::dashboardSidebar(
    sidebarMenu(
      menuItem("Credits", tabName = "credits", icon = icon(CREDITS_ICON)),
      menuItem("Network", tabName = "network", icon = icon(NETWORK_ICON)),
      menuItem("Live", tabName = "live",  icon = icon(LIVE_ICON),
               badgeLabel = "\U1F528", badgeColor = "yellow")
    )
  ),
  shinydashboard::dashboardBody(
    tags$head(tags$style(HTML(".small-box {height: 130px}"))),
    tabItems(
      tabItem(tabName = "credits",
              fluidRow(
                uiOutput("date_range_box_credits")
              ),
              fluidRow(
                shinydashboard::box(plotOutput("credits_plot"), 
                                    width = OVERVIEW_PLOT_WIDTH,
                                    title = "Credits", status = "primary",
                                    solidHeader = TRUE),
                uiOutput("total_credits_box"),
                uiOutput("credits_per_day_box")
              )
      ),
      tabItem(tabName = "network",
              fluidRow(
                uiOutput("date_range_box_network")
              ),
              fluidRow(
                shinydashboard::box(plotOutput("network_plot"), 
                                    width = OVERVIEW_PLOT_WIDTH,
                                    title = "Network", status = "warning",
                                    solidHeader = TRUE,
                                    height = 700),
                uiOutput("total_network_usage_box"),
                uiOutput("network_usage_per_day_box")
              )
      ),
      tabItem(tabName = "live",
              fluidRow(
                uiOutput("time_range_box_live")
              ),
              fluidRow(
                shiny::column(
                  uiOutput("folding_slot_boxes_rendered"),
                  width = 3
                ),
                shiny::column(
                  uiOutput("slot_name_boxes_rendered"),
                  width = 3
                ),
                shiny::column(
                  uiOutput("slot_progress_boxes_rendered"),
                  width = 3
                )
              )
      )
    )
  )
)
