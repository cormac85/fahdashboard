library(shiny)
library(fahlogstats)
library(dplyr)
library(lubridate)
library(stringr)
library(shinydashboard)

FAH_CLIENT_FOLDER_PATH <- file.path("~", "..", "AppData", "Roaming", "FAHClient")
FAH_CLIENT_RECENT_LOGS_PATH <- file.path(FAH_CLIENT_FOLDER_PATH, "logs")
FAH_CLIENT_LIVE_LOG_FILE_NAME <- "log.txt"
NUMBER_LOGS_READ <- 8

ggplot2::theme_set(
  ggplot2::theme_minimal(base_size = 20)
)

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

  log_df
}

function(input, output, session) {
  live_log_df <- reactive({
    live_log <- 
      shiny_read_log(file.path(FAH_CLIENT_FOLDER_PATH, 
                               FAH_CLIENT_LIVE_LOG_FILE_NAME)) %>% 
      mutate(log_file_name = FAH_CLIENT_LIVE_LOG_FILE_NAME)
    
    live_log
  })
  
  recent_logs_df <- reactive({
    recent_logs <- 
      tibble::tibble(
        log_file_name = list.files(pattern = "*.txt",
                                   path = FAH_CLIENT_RECENT_LOGS_PATH)
      ) %>% 
      arrange(log_file_name) %>% 
      tail(NUMBER_LOGS_READ) %>% 
      mutate(
        log_file_path = file.path(FAH_CLIENT_RECENT_LOGS_PATH, log_file_name),
        log_df = purrr::map(log_file_path, shiny_read_log)
      ) %>% 
      tidyr::unnest(log_df)
    
    print(recent_logs %>% pull(log_file_name) %>% unique())
    recent_logs
  })
  
  cleaned_logs <- reactive({
    print(recent_logs_df())
    print(live_log_df())
    recent_logs_df() %>% 
      fahlogstats::clean_logs() %>% 
      dplyr::union_all(
        fahlogstats::clean_logs(live_log_df())
      )
  })
  
  work_unit_df <- reactive({
      cleaned_logs() %>% 
      fahlogstats::get_work_unit_data()
  })
  
  output$credits_plot <- renderPlot({
    work_unit_df() %>% 
      get_credits() %>%
      fahlogstats::plot_credits(all_slots = TRUE) 
  })
  
  output$network_plot <- renderPlot({
    work_unit_df() %>% 
      fahlogstats::get_network_usage() %>%
      fahlogstats::calculate_daily_network_usage() %>% 
      fahlogstats::plot_cumulative_network_usage()
  }, height = 700)
  
}

