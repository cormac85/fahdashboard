source(file.path("components", "./date_range.R"), local = TRUE)

output$date_range_box_idle <- renderUI({
  date_range_box()
})


# credits_df <- reactive({
#   global_timer()
#   
#   credits <-
#     work_unit_df() %>% 
#     get_credits()
#   print(credits)
#   
#   credits
# })
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

output$idle_plot <- renderPlot({
  cleaned_logs() %>% 
    fahlogstats::plot_weekly_idle_slots() 
})