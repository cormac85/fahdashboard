live_time_range_box <- reactive({
  
  live_logs_clean <- 
    cleaned_logs() %>% 
    filter(log_file_name == "log.txt")
  
  log_start_time <- min(live_logs_clean$log_timestamp)
  log_end_time <- max(live_logs_clean$log_timestamp)
  print(dput(log_start_time))
  print(dput(log_end_time))
  log_period <- lubridate::as.period(log_end_time - log_start_time)
    
  
  infoBox(title = "Live Log Time Range", 
          value = paste0(
            strftime(log_start_time, format = "%A %Y-%m-%d %H:%M:%S"),
            " \U2192 ",
            strftime(log_end_time, format = "%A %Y-%m-%d %H:%M:%S")
          ),
          width = 12,
          icon = icon("clock"),
          subtitle = log_period)
})

output$time_range_box_live <- renderUI({
  live_time_range_box()
})