# Folding Slot Dropdown
output$folding_slot_menu <- renderMenu({
  global_timer()
  
  msgs <- apply(folding_slot_names_df(), 1, function(row) {
    
    progress_colour = ifelse(
      (as.numeric(row[["slot_progress"]]) >= 0) & (as.numeric(row[["slot_progress"]]) < 100),
      "green",
      "yellow"
    )
    
    progress_colour = ifelse(is.na(progress_colour), "red", progress_colour)
    
    taskItem(
      text = tags$span(
        tags$strong(row[["folding_slot"]]),
        tags$p(row[["processor_name"]])
      ),
      value = row[["slot_progress"]],
      color = progress_colour
    )
  })
  
  dropdownMenu(type = "tasks", 
               .list = msgs,
               headerText = tags$div( 
                 tags$strong(nrow(folding_slot_names_df()), .noWS = "outside"),
                 tags$span(" folding slots available", .noWS = "outside"),
                 .noWS = "outside"
               ))
})


# Time Box
live_time_range_box <- reactive({
  
  live_logs_clean <- 
    cleaned_logs() %>% 
    filter(log_file_name == "log.txt")
  
  log_start_time <- min(live_logs_clean$log_timestamp)
  log_end_time <- max(live_logs_clean$log_timestamp)
  log_period <- 
    difftime(log_end_time, 
             log_start_time, 
             units = "hours") %>% 
    difftime_to_period(seconds_in_unit = 1)
  
  infoBox(
    title = "Live Log Time Range", 
    value = paste0(
      strftime(log_start_time, format = "%A %Y-%m-%d %H:%M:%S"),
      " \U2192 ",
      strftime(log_end_time, format = "%A %Y-%m-%d %H:%M:%S")
    ),
    width = 12,
    icon = icon("clock"),
    subtitle = log_period,
    fill = TRUE,
    color = "black")
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
                slot_progress, progress_timestamp, work_start)
  
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
  
  print(folding_slot_names_df())
  
  folding_slot_names_df() %>% 
    dplyr::mutate(
      current_work_duration = purrr::map(
        difftime(progress_timestamp, work_start, unit = "hours"),
        difftime_to_period,
        seconds_in_unit = 3600),
      core_name_value_box = purrr::map2(
        slot_progress, current_work_duration,
        function(slot_p, work_d) {
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
            subtitle = work_d
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

# Folding Slot Boxes
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

# Latest Work Unit Boxes
live_credits_formatter <- scales::label_number(accuracy = 0.1, scale = 1e-3, suffix = "k")


live_credits_df <- reactive({
  global_timer()
  
  credits <-
    work_unit_df() %>% 
    dplyr::filter(log_file_name == "log.txt") %>% 
    get_credits()
  
  credits
})

latest_work_df <- reactive({
  global_timer()
  
  work_unit_df() %>% 
    dplyr::filter(log_file_name == "log.txt") %>% 
    get_latest_work_unit_details()
  
})

latest_credits_boxes <- reactive({
  req(latest_work_df())
  
  print(latest_work_df())
  
  latest_work_df() %>% 
    dplyr::mutate(
      latest_work_duration = purrr::map(
        latest_work_duration,
        difftime_to_period,
        seconds_in_unit = 3600
      ),
      latest_credits_value_box = purrr::map2(
        latest_credits_attributed, latest_work_duration,
        function(latest_c, latest_d) {
          valueBox(
            value = live_credits_formatter(latest_c),
            color = "aqua",
            width = 12,
            subtitle = latest_d,
            icon = icon(CREDITS_ICON)
          )
        })) %>% 
    pull(latest_credits_value_box)
})


output$latest_credits_boxes_rendered <- renderUI({
  global_timer()
  req(latest_credits_boxes())
  
  purrr::iwalk(latest_credits_boxes(), ~{
    output_name <- paste0("box_", .y)
    output[[output_name]] <- renderUI(.x)
  })
})