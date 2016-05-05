calc_height_param_plot <- reactive({
  if (!isTRUE(input$param_plot_show_density)) {
    "auto"
  } else {
    params <- input$params_to_plot
    params <- .update_params_with_groups(params, PARAM_NAMES)
    LL <- length(params)
    LL <- ifelse(LL < 8, 8, LL)
    if (!is.null(input$param_plot_color_by_rhat)) {
      # delay until input is ready
      if (input$param_plot_color_by_rhat == TRUE) {
        LL <- LL + 1
      }
    }
    round(50 * LL)
  }
})

multiparam_plot <- reactive({
  validate(need(input$param_plot_fill_color, message = "Loading..."))
  if (is.null(input$param_plot_ci_level)) {
    # delay until input is ready
    return()
  }
  
  customize <- !is.null(input$param_plot_show_density)
  do.call(
    ".multiparam_plot",
    args = list(
      samps           = SAMPS_post_warmup,
      params          = input$params_to_plot,
      all_param_names = PARAM_NAMES,
      CI.level        = input$param_plot_ci_level / 100,
      rhat_values     = SUMMARY[, "Rhat"],
      show_density    = ifelse(customize, input$param_plot_show_density, FALSE), # == "yes", FALSE),
      show_ci_line    = ifelse(customize, input$param_plot_show_ci_line, TRUE), # == "yes", TRUE),
      color_by_rhat   = ifelse(customize, input$param_plot_color_by_rhat, FALSE), # == "yes", FALSE),
      rhat_palette    = ifelse(customize, input$param_plot_rhat_palette, "Oranges"),
      point_est       = ifelse(customize, input$param_plot_point_est, "Median"),
      fill_color      = ifelse(customize, input$param_plot_fill_color, "gray35"),
      outline_color   = ifelse(customize, input$param_plot_outline_color, "black"),
      est_color       = ifelse(customize, input$param_plot_est_color, "black")
    )
  )
})


output$multiparam_plot_out <- renderPlot({
  multiparam_plot()
}, height = calc_height_param_plot, bg = "transparent")

# download the plot
output$download_multiparam_plot <- downloadHandler(
  filename = 'shinystan-multiparam-gg.RData',
  content = function(file) {
    shinystan_multiparam_gg <- multiparam_plot()
    save(shinystan_multiparam_gg, file = file)
  }
)
output$save_pdf_multiparam = downloadHandler(
  filename = "shinstan-multiparam.pdf",
  content = function(file) {
    ggsave(file, plot = multiparam_plot(), device = pdf)
  }
)
