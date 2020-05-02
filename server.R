library(shiny)
library(fahlogstats)
library(dplyr)
library(lubridate)
library(stringr)

shiny_read_log <- function(file_path) {

  log_df <- suppressMessages(tibble::tibble(message = scan(file_path,
                                                           what = "character",
                                                           sep = "\r",
                                                           quiet = TRUE)))
  log_df <- dplyr::filter(log_df, !stringr::str_starts(message, "\\*"))
  
  log_start_date <-
    log_df %>%
    dplyr::filter(stringr::str_detect(message, " Log Started")) %>%
    (function(x) x$message[1]) %>%
    stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}") %>%
    lubridate::ymd()
  
  log_df$log_date <- log_start_date
  log_df$log_file_name <- "log.txt"
  
  log_df
  
}

function(input, output, session) {
  log_df <- reactive({
    req(input$log_files)
    
    file_extension <- tools::file_ext(input$log_files$name)
    switch(file_extension,
           txt = shiny_read_log(input$log_files$datapath),
           validate(
             "Invalid file. Please supply a .txt log file from FAH Client"
           )
    )
  })
  
  work_unit_df <- reactive({
    log_df() %>% 
      fahlogstats::clean_logs() %>% 
      fahlogstats::get_work_unit_data()
  })
  
  
  
  output$logs_df <- renderTable({
    head(work_unit_df())
  })
  
  output$credits_plot <- renderPlot({
    work_unit_df() %>% 
      get_credits() %>%
      plot_credits(all_slots = TRUE)
  })
  
}

