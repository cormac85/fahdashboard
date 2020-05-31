source(file.path("components", "./date_range.R"), local = TRUE)

# Plot Idle Folding Slots
plot_weekly_idle_slots <- function(logs_df) {
  
  processing_time_summary <-
    logs_df %>%
    get_processing_time_summary() %>%
    dplyr::mutate(week_number = strftime(log_date, format = "%Y-W%W")) %>%
    dplyr::group_by(folding_slot, week_number) %>%
    dplyr::summarise(total_processing_time = sum(total_processing_time))
  
  
  log_duration_summary <-
    logs_df %>%
    get_daily_duration() %>%
    dplyr::mutate(week_number = strftime(log_date, format = "%Y-W%W")) %>%
    dplyr::group_by(week_number) %>%
    dplyr::summarise(total_log_duration = sum(total_log_duration))
  
  utilisation_summary <-
    processing_time_summary %>%
    dplyr::left_join(
      log_duration_summary,
      by = "week_number") %>%
    dplyr::mutate(utilisation_percent = total_processing_time / (as.numeric(total_log_duration) / 3600),
                  idle_percent = 1 - utilisation_percent) %>%
    tidyr::pivot_longer(cols = c(utilisation_percent, idle_percent),
                        names_to = "metric",
                        values_to = "value") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(folding_slot = paste0("Folding Slot: ", as.numeric(folding_slot)))
  
  
  
  utilisation_summary %>%
    ggplot2::ggplot(ggplot2::aes(week_number, value,
                                 fill = metric,
                                 group = metric)) +
    ggplot2::geom_col() +
    ggplot2::facet_wrap(~folding_slot, ncol = 1) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_minimal(base_size = BASE_PLOT_TEXT_SIZE) +
    ggplot2::theme(legend.position = "right",
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
                   panel.grid.major.x = ggplot2::element_blank(),
                   panel.grid.minor.x = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold",
                                                      size = ggplot2::rel(1.2)),
                   axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2))
                   ) +
    ggplot2::labs(title = paste0("Utilisation Percentage per Week"),
                  subtitle = paste0(min(logs_df$log_date), " - ",
                                    max(logs_df$log_date)),
                  x = "Week Number", y = "Used vs Idle Time (%)",
                  fill = "Idle vs Used") +
    ggplot2::scale_fill_manual(
      values = fah_web_palette,
      labels = c("Idle", "Used")
    )
}

plot_daily_idle_slots <- function(logs_df) {
  
  processing_time_summary <-
    logs_df %>%
    get_processing_time_summary() %>%
    dplyr::mutate(week_number = strftime(log_date, format = "%Y-W%W")) %>%
    dplyr::group_by(folding_slot, log_date) %>%
    dplyr::summarise(total_processing_time = sum(total_processing_time))
  
  
  log_duration_summary <-
    logs_df %>%
    get_daily_duration() %>%
    dplyr::mutate(week_number = strftime(log_date, format = "%Y-W%W")) %>%
    dplyr::group_by(log_date) %>%
    dplyr::summarise(total_log_duration = sum(total_log_duration))
  
  utilisation_summary <-
    processing_time_summary %>%
    dplyr::left_join(
      log_duration_summary,
      by = "log_date") %>%
    dplyr::mutate(utilisation_percent = total_processing_time / (as.numeric(total_log_duration) / 3600),
                  idle_percent = 1 - utilisation_percent) %>%
    tidyr::pivot_longer(cols = c(utilisation_percent, idle_percent),
                        names_to = "metric",
                        values_to = "value") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(folding_slot = paste0("Folding Slot: ", as.numeric(folding_slot)))
  
  
  
  utilisation_summary %>%
    dplyr::filter(metric == "utilisation_percent") %>% 
    ggplot2::ggplot(ggplot2::aes(log_date, value,
                                 group = metric,
                                 colour = metric)) +
    ggplot2::geom_point(alpha = 0.5) +
    ggplot2::geom_smooth(se=FALSE) +
    ggplot2::facet_wrap(~folding_slot, ncol = 1) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::theme_minimal(base_size = BASE_PLOT_TEXT_SIZE) +
    ggplot2::theme(
      legend.position = "none",
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold",
                                         size = ggplot2::rel(1.2)),
      axis.title.y = ggplot2::element_text(size = ggplot2::rel(1.2))
    ) +
    ggplot2::labs(title = paste0("Utilisation Percentage per Day"),
                  subtitle = paste0(min(logs_df$log_date), " - ",
                                    max(logs_df$log_date)),
                  x = "Date", y = "Used Time (%)") +
    ggplot2::scale_color_manual(values = fah_web_palette)
}

output$date_range_box_idle <- renderUI({
  date_range_box()
})


credits_df <- reactive({
  global_timer()

  credits <-
    work_unit_df() %>%
    get_credits()
  print(credits)

  credits
})
# 
# credits_per_day <- reactive({
#   global_timer()
#   
#   credits_df() %>% 
#     dplyr::group_by(log_date) %>% 
#     dplyr::summarise(credits_per_day = sum(credits_attributed))
# })
# 
# credits_formatter <- scales::label_number(accuracy = 0.1, scale = 1e-6, suffix = "M")
# 
# output$total_credits_box <- renderUI({
#   valueBox(
#     subtitle = shiny::p("Total Credits"),
#     value = credits_formatter(sum(credits_df()$credits_attributed)),
#     icon = icon(CREDITS_ICON),
#     color = "light-blue",
#     width = 3
#   )
# })
# 
# output$credits_per_day_box <- renderUI({
#   valueBox(
#     subtitle = shiny::p("Credits per Day"),
#     value = credits_formatter(mean(credits_per_day()$credits_per_day)),
#     icon = icon(CREDITS_ICON),
#     color = "aqua",
#     width = 3
#   )
# })

reactive_plot <- reactiveValues(
  idle_plot = NULL 
)

observeEvent(input$idle_plot_weekly, {
  reactive_plot$idle_plot <-
    plot_weekly_idle_slots(cleaned_logs()) 
})

observeEvent(input$idle_plot_daily, {
  reactive_plot$idle_plot <-
    plot_daily_idle_slots(cleaned_logs()) 
})

output$idle_plot <- renderPlot({
  global_timer()
  if (is.null(reactive_plot$idle_plot)) 
    return(plot_weekly_idle_slots(cleaned_logs()))
  reactive_plot$idle_plot
})


