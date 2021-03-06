library(shiny)
library(shinydashboard)

source(file.path(".", "global.R"), local = TRUE)


shinydashboard::dashboardPage(skin = "red",
  shinydashboard::dashboardHeader(
    title = "Folding@Home Live Stats",
    dropdownMenuOutput("folding_slot_menu")
    ),
  shinydashboard::dashboardSidebar(
    sidebarMenu(
      
      menuItem("Live", tabName = "live",  icon = icon(LIVE_ICON)),
      menuItem("Credits", tabName = "credits", icon = icon(CREDITS_ICON)),
      menuItem("Network", tabName = "network", icon = icon(NETWORK_ICON)),
      menuItem("Idle Time", tabName = "idle", icon = icon(IDLE_TIME_ICON))
    ),
    tags$head(tags$style(HTML(".fa-heartbeat {color: rgb(211, 55, 36)}")))
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
      tabItem(tabName = "idle",
              fluidRow(
                uiOutput("date_range_box_idle")
              ),
              fluidRow(
                shinydashboard::box(plotOutput("idle_plot"), 
                                    width = OVERVIEW_PLOT_WIDTH,
                                    title = "Idleness", status = "danger",
                                    solidHeader = TRUE),
                shiny::actionButton("idle_plot_weekly", 
                                    "Weekly",
                                    icon = icon("calendar")),
                shiny::actionButton("idle_plot_daily", 
                                    "Daily",
                                    icon = icon("sun")),
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
                ),
                shiny::column(
                  uiOutput("latest_credits_boxes_rendered"),
                  width = 3
                )
              )
      )
    )
  )
)

