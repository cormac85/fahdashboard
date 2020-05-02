library(shiny)
library(fahlogstats)
library(dplyr)
library(lubridate)
library(stringr)
library(shinydashboard)

FAH_CLIENT_FOLDER_PATH <- file.path("~", "..", "AppData", "Roaming", "FAHClient")

shiny_read_log <- function(log_file_path) {

  log_df <- suppressMessages(
    tibble::tibble(
      message = scan(log_file_path,
                     what = "character",
                     sep = "\r",
                     quiet = TRUE)
      )
    )
  
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
    shiny_read_log(file.path(FAH_CLIENT_FOLDER_PATH, "log.txt"))
  })
  
  work_unit_df <- reactive({
    log_df() %>% 
      fahlogstats::clean_logs() %>% 
      fahlogstats::get_work_unit_data()
  })
  
  output$credits_plot <- renderPlot({
    work_unit_df() %>% 
      get_credits() %>%
      plot_credits(all_slots = TRUE)
  })
  
  output$network_plot <- renderPlot({
    work_unit_df() %>% 
      fahlogstats::get_network_usage() %>%
      fahlogstats::calculate_daily_network_usage() %>% 
      plot_cumulative_network_usage()
  })
  
}

