library(shiny)
library(shinydashboard)

OVERVIEW_PLOT_WIDTH = 9

shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Folding@Home Live Stats"),
  shinydashboard::dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("book-reader")),
      menuItem("Live", tabName = "live",  icon = icon("heartbeat"),
               badgeLabel = "\U1F528", badgeColor = "yellow")
    )
  ),
  shinydashboard::dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
        fluidRow(
          shinydashboard::box(plotOutput("credits_plot"), 
                              width = OVERVIEW_PLOT_WIDTH,
                              title = "Credits", status = "primary",
                              solidHeader = TRUE)
        ),
        fluidRow(
          shinydashboard::box(plotOutput("network_plot"), 
                              width = OVERVIEW_PLOT_WIDTH,
                              title = "Network", status = "primary",
                              solidHeader = TRUE)
          
        )
      ),
      tabItem(tabName = "live",
        shiny::h3("Under Construction!")
      )
    )
  )
)
  