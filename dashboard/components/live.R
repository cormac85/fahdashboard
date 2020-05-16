PROCESSOR_ICON = "microchip"
FOLDING_ICON_FINISHED = "stop"
FOLDING_ICON_RUNNING = "play"
FOLDING_ICON_NOT_STARTED = "clock"
FOLDING_ICON_ERROR = "exclamation"
FOLDING_VALUE_BOX_HEIGHT = 200


# Time Box
live_time_range_box <- reactive({
  
  live_logs_clean <- 
    cleaned_logs() %>% 
    filter(log_file_name == "log.txt")
  
  log_start_time <- min(live_logs_clean$log_timestamp)
  log_end_time <- max(live_logs_clean$log_timestamp)
  log_period <- 
    lubridate::as.period(log_end_time - log_start_time) %>% 
    as.numeric() %>% 
    round() %>% 
    lubridate::seconds_to_period()
  
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


# Slot Table

folding_slot_names_df <- reactive({
  live_logs_clean <- 
    cleaned_logs() %>% 
    filter(log_file_name == "log.txt")
  
  core_progress <- fahlogstats::get_slot_progress(live_logs_clean)
  dplyr::select(core_progress, folding_slot, 
                processor_name, processor_type, 
                slot_progress)
  
})

output$folding_slot_table <- renderTable({
  global_timer()
  folding_slot_names_df()
})


# Slot Name Boxes
slot_name_boxes <- reactive({
  req(folding_slot_names_df())
  
  folding_slot_names_df() %>% 
    dplyr::mutate(core_name_value_box = purrr::map2(
      processor_name, processor_type,
      function(processor_n, processor_t) {
      valueBox(
        value = processor_t,
        icon = icon(PROCESSOR_ICON),
        color = ifelse(processor_t == "CPU", "blue", "red"),
        width = 12,
        subtitle = processor_n
      )
    })) %>% 
    pull(core_name_value_box)
})


output$slot_name_boxes_rendered <- renderUI({
  global_timer()
  req(slot_name_boxes())
  
  purrr::iwalk(slot_name_boxes(), ~{
    output_name <- paste0("box_", .y)
    output[[output_name]] <- renderUI(.x)
  })
})

# Slot Progress Boxes
slot_progress_boxes <- reactive({
  req(folding_slot_names_df())
  
  folding_slot_names_df() %>% 
    dplyr::mutate(core_name_value_box = purrr::map(
      slot_progress,
      function(slot_p) {
        valueBox(
          value = scales::percent(slot_p / 100),
          icon =   dplyr::case_when(
            near(slot_p, 100) ~ list(icon(FOLDING_ICON_FINISHED)), 
            slot_p < 100      ~ list(icon(FOLDING_ICON_RUNNING)),
            is.na(slot_p)     ~ list(icon(FOLDING_ICON_NOT_STARTED)),
            TRUE              ~ list(icon(FOLDING_ICON_ERROR))
          )[[1]],
          color = dplyr::case_when(
            slot_p == 100 ~ "yellow", 
            slot_p < 100 ~ "green",
            is.na(slot_p) ~ "black",
            TRUE ~ "red"
          ),
          width = 12,
          subtitle = ""
        )
      })) %>% 
    pull(core_name_value_box)
})


output$slot_progress_boxes_rendered <- renderUI({
  global_timer()
  req(slot_progress_boxes())
  
  purrr::iwalk(slot_progress_boxes(), ~{
    output_name <- paste0("box_", .y)
    output[[output_name]] <- renderUI(.x)
  })
})

# Slot Progress Boxes
folding_slot_boxes <- reactive({
  req(folding_slot_names_df())
  
  folding_slot_names_df() %>% 
    dplyr::mutate(folding_slot_value_box = purrr::map(
      folding_slot,
      function(folding_s) {
        valueBox(
          value = folding_s,
          color = "black",
          width = 12,
          subtitle = ""
        )
      })) %>% 
    pull(folding_slot_value_box)
})


output$folding_slot_boxes_rendered <- renderUI({
  global_timer()
  req(folding_slot_boxes())
  
  purrr::iwalk(folding_slot_boxes(), ~{
    output_name <- paste0("box_", .y)
    output[[output_name]] <- renderUI(.x)
  })
})