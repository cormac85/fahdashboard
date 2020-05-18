source(file.path("components", "./date_range.R"), local = TRUE)

output$date_range_box_idle <- renderUI({
  date_range_box()
})
