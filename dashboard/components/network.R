source(file.path("components", "./date_range.R"), local = TRUE)

output$date_range_box_network <- renderUI({
  date_range_box()
})


network_usage_formatter <- scales::label_number(accuracy = 0.1, scale = 1e-3, suffix = "GiB")

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

output$total_network_usage_box <- renderUI({
  valueBox(
    subtitle = shiny::p("Total Usage"),
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
    subtitle = shiny::p("Usage per Day"),
    value = network_usage_formatter(
      mean(total_daily_network_usage_df()$total_usage_mib)
    ),
    icon = icon(NETWORK_ICON),
    color = "orange",
    width = 3
  )
})

output$network_plot <- renderPlot({
  daily_network_usage_df() %>% 
    fahlogstats::plot_cumulative_network_usage()
}, height = 600)
