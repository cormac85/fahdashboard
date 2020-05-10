library(shiny)
library(shinydashboard)

OVERVIEW_PLOT_WIDTH = 9
OVERVIEW_PLOT_HEIGHT = 9
CREDITS_ICON = "award"
NETWORK_ICON = "wifi"

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
          shinydashboard::infoBox(title = "Date Range", 
                                  value = "2010/01/01 \U2192 2010/02/01",
                                  width = 12,
                                  icon = icon("calendar-alt"))
        ),
        fluidRow(
          shinydashboard::box(plotOutput("credits_plot"), 
                              width = OVERVIEW_PLOT_WIDTH,
                              title = "Credits", status = "primary",
                              solidHeader = TRUE),
          shinydashboard::valueBox(1000000, "Total Credits",
                                   width = 3, 
                                   color = "light-blue",
                                   icon = icon(CREDITS_ICON)),
          shinydashboard::valueBox(10000, "Credits per Day",
                                   width = 3,
                                   color = "aqua",
                                   icon = icon(CREDITS_ICON))
        ),
        fluidRow(
          shinydashboard::box(plotOutput("network_plot"), 
                              width = OVERVIEW_PLOT_WIDTH,
                              title = "Network", status = "warning",
                              solidHeader = TRUE,
                              height = 800),
          shinydashboard::valueBox(3.1, "Total Usage (GiB)",
                                   width = 3, 
                                   color = "yellow",
                                   icon = icon(NETWORK_ICON)),
          shinydashboard::valueBox(0.5, "Usage per Day (GiB)",
                                   width = 3, 
                                   color = "orange",
                                   icon = icon(NETWORK_ICON))
          
        )
      ),
      tabItem(tabName = "live",
        shiny::h3("Under Construction!")
      )
    )
  )
)
  