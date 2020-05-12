library(shiny)
library(fahlogstats)
library(dplyr)
library(lubridate)
library(stringr)
library(shinydashboard)
library(scales)

# Logs
FAH_CLIENT_FOLDER_PATH <- file.path("~", "..", "AppData", "Roaming", "FAHClient")
FAH_CLIENT_RECENT_LOGS_PATH <- file.path(FAH_CLIENT_FOLDER_PATH, "logs")
FAH_CLIENT_LIVE_LOG_FILE_NAME <- "log.txt"
NUMBER_LOGS_READ <- 8
RENDER_PERIOD_MINUTES <- 5
# Styling
CREDITS_ICON = "award"
NETWORK_ICON = "wifi"

# Functions
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
  
  global_timer <- reactiveTimer(1000 * 60 * RENDER_PERIOD_MINUTES)
  
  #############
  # Read Logs #
  #############
  live_log_df <- reactive({
    global_timer()
    live_log <- 
      shiny_read_log(file.path(FAH_CLIENT_FOLDER_PATH, 
                               FAH_CLIENT_LIVE_LOG_FILE_NAME)) %>% 
      mutate(log_file_name = FAH_CLIENT_LIVE_LOG_FILE_NAME)
    
    live_log
  })
  
  recent_logs_df <- reactive({
    global_timer()
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
    global_timer()
    
    print(recent_logs_df())
    print(live_log_df())
    recent_logs_df() %>% 
      fahlogstats::clean_logs() %>% 
      dplyr::union_all(
        fahlogstats::clean_logs(live_log_df())
      )
  })
  
  work_unit_df <- reactive({
    global_timer()
    
    cleaned_logs() %>% 
      fahlogstats::get_work_unit_data()
  })
  
  ###########
  # Credits #
  ###########
  credits_df <- reactive({
    global_timer()
    
    credits <-
      work_unit_df() %>% 
      get_credits()
    print(credits)
    
    credits
  })
  
  credits_per_day <- reactive({
    global_timer()
    
    credits_df() %>% 
      dplyr::group_by(log_date) %>% 
      dplyr::summarise(credits_per_day = sum(credits_attributed))
  })
  
  ###########
  # Network #
  ###########
  daily_network_usage_df <- reactive({
    global_timer()
    
    usage <- 
      work_unit_df() %>% 
      fahlogstats::get_network_usage() %>% 
      fahlogstats::calculate_daily_network_usage()
    print(usage)
    usage
  })
  
  total_daily_network_usage_df <- reactive({
    global_timer()
    
    daily_network_usage_df() %>% 
      dplyr::group_by(log_date) %>% 
      dplyr::summarise(total_usage_mib = sum(total_usage_mib))
  })
  
  #########
  # Plots #
  #########
  output$credits_plot <- renderPlot({
    credits_df() %>% 
      fahlogstats::plot_credits(all_slots = TRUE) 
  })
  
  output$network_plot <- renderPlot({
    daily_network_usage_df() %>% 
      fahlogstats::plot_cumulative_network_usage()
  }, height = 600)
  
  ###############
  # Info Boxes #
  ###############
  date_range_box <- reactive({
    log_start_date <- min(cleaned_logs()$log_date)
    log_end_date <- max(cleaned_logs()$log_date)
    
    infoBox(title = "Date Range", 
            value = paste0(
              strftime(log_start_date, format = "%A %Y-%m-%d"),
              " \U2192 ",
              strftime(log_end_date, format = "%A %Y-%m-%d")
            ),
            width = 12,
            icon = icon("calendar-alt"),
            subtitle = paste0(log_end_date - log_start_date,
                              " days (read from ", 
                              NUMBER_LOGS_READ + 1, 
                              " most recent log files)"))
  })
  
  output$date_range_box_credits <- renderUI({
    date_range_box()
  })
  
  output$date_range_box_network <- renderUI({
    date_range_box()
  })
  
  ###############
  # Value Boxes #
  ###############
  credits_formatter <- scales::label_number(accuracy = 0.1, scale = 1e-6, suffix = "M")
  network_usage_formatter <- scales::label_number(accuracy = 0.1, scale = 1e-3)
  
  output$total_credits_box <- renderUI({
    valueBox(
      subtitle = shiny::p("Total Credits"),
      value = credits_formatter(sum(credits_df()$credits_attributed)),
      icon = icon(CREDITS_ICON),
      color = "light-blue",
      width = 3
    )
  })
  output$credits_per_day_box <- renderUI({
    valueBox(
      subtitle = shiny::p("Credits per Day"),
      value = credits_formatter(mean(credits_per_day()$credits_per_day)),
      icon = icon(CREDITS_ICON),
      color = "aqua",
      width = 3
    )
  })
  
  output$total_network_usage_box <- renderUI({
    valueBox(
      subtitle = shiny::p("Total Usage (GiB) "),
      value = network_usage_formatter(
        sum(total_daily_network_usage_df()$total_usage_mib)
        ),
      icon = icon(NETWORK_ICON),
      color = "yellow",
      width = 3
    )
  })
  output$network_usage_per_day_box <- renderUI({
    valueBox(
      subtitle = shiny::p("Usage per Day (GB)"),
      value = network_usage_formatter(
        mean(total_daily_network_usage_df()$total_usage_mib)
        ),
      icon = icon(NETWORK_ICON),
      color = "orange",
      width = 3
    )
  })
}

