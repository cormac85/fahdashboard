library(shiny)
fluidPage(
  fileInput("log_files", NULL, buttonLabel = "Upload:", multiple = TRUE,
            accept = c(".txt")), 
  tableOutput("logs_df"),
  plotOutput("credits_plot")
)
