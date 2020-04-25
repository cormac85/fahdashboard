library(shiny)
fluidPage(
  
  titlePanel("Folding@Home Log Statistics Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("numInput", "A numeric input:", value = 7, min = 1, max = 30)
    ),
    mainPanel(
      p(strong("bold font "), em("italic font")),
      p(code("code block")),
      a(href="http://www.google.com", "link to Google")
    )
  )
)
