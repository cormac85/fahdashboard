library(shiny)
library(fahlogstats)
library(dplyr)

# shiny_read_log <- function(file_path) { 
#   
#   suppressMessages(tibble::tibble(message = scan(file_path,
#                                                  what = "character",
#                                                  sep = "\r",
#                                                  quiet = TRUE)))
#   
# }

function(input, output, session) {
  data <- reactive({
    req(input$log_files)
    
    file_extension <- tools::file_ext(input$log_files$name)
    switch(file_extension,
           csv = read.csv(input$log_files$datapath, stringsAsFactors = FALSE),
           validate(
             "Invalid file. Please supply a .txt log file from FAH Client"
           )
    )
  })
  output$logs_df <- renderTable({
    head(data())
  })
}

