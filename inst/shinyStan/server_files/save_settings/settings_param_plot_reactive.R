save_settings_param_plot <- reactive({
  list(
    show_density    = input$param_plot_show_density,
    show_ci_line    = input$param_plot_show_ci_line,
    color_by_rhat   = input$param_plot_color_by_rhat,
    fill_color      = input$param_plot_fill_color,
    outline_color   = input$param_plot_outline_color,
    point_est       = input$param_plot_point_est,
    est_color = input$param_plot_est_color,
    rhat_palette = input$param_plot_rhat_palette
  )
})
