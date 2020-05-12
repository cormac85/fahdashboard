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
