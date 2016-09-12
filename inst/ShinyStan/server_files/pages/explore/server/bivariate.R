# bivariate scatterplot
bivariate_transform_x <-
  eventReactive(input$bivariate_transform_go > 0, input$bivariate_transform_x)
bivariate_transform_y <-
  eventReactive(input$bivariate_transform_go > 0, input$bivariate_transform_y)
bivariate_plot <- reactive({
  validate(
    need(input$param, message = FALSE),
    need(input$bivariate_ellipse_lev, message = FALSE),
    need(input$bivariate_param_y, message = FALSE)
  )
  
  if (!is.null(input$bivariate_ellipse_lev)) {
    validate(
      need(is.numeric(input$bivariate_pt_size), message = "Point size must be numeric"),
      need(is.numeric(input$bivariate_pt_shape), message = "Point shape must be numeric")
    )
    
    if (input$bivariate_ellipse_lev != "None") {
      validate(
        need(
          input$param != input$bivariate_param_y,
          "For this option the x and y can't be the same parameter."
        ),
        need(
          is.numeric(input$bivariate_ellipse_lwd),
          message = "Ellipse size must be numeric"
        ),
        need(
          is.numeric(input$bivariate_ellipse_lty),
          message = "Ellipse shape must be numeric"
        )
      )
    }
  }
  
  do.call(
    ".bivariate_plot",
    args = list(
      samps = SAMPS_post_warmup,
      sp = if (!identical(SAMPLER_PARAMS_post_warmup, FALSE) && STAN_ALGORITHM == "NUTS")
        SAMPLER_PARAMS_post_warmup else NULL,
      max_td = if ("max_td" %in% names(MISC)) MISC$max_td else NULL,
      param = input$param,
      param2 = input$bivariate_param_y,
      pt_alpha = input$bivariate_pt_alpha,
      pt_size = input$bivariate_pt_size,
      pt_shape = input$bivariate_pt_shape,
      pt_color = input$bivariate_pt_color,
      ellipse_lev = input$bivariate_ellipse_lev,
      ellipse_color = input$bivariate_ellipse_color,
      ellipse_lty = input$bivariate_ellipse_lty,
      ellipse_lwd = input$bivariate_ellipse_lwd,
      ellipse_alpha = input$bivariate_ellipse_alpha,
      lines  = input$bivariate_lines,
      lines_color = input$bivariate_lines_color,
      lines_alpha = input$bivariate_lines_alpha,
      transform_x = bivariate_transform_x(),
      transform_y = bivariate_transform_y()
    )
  )
})

output$bivariate_plot_out <- renderPlot({
  x <- bivariate_plot()
  suppressWarnings(print(x))
}, bg = "transparent")

# download
output$download_bivariate <- downloadHandler(
  filename = 'shinystan-bivariate-gg.RData',
  content = function(file) {
    shinystan_bivariate_gg <- bivariate_plot()
    save(shinystan_bivariate_gg, file = file)
  }
)
output$save_pdf_bivariate = downloadHandler(
  filename = "shinstan-bivariate.pdf",
  content = function(file) {
    ggsave(file, plot = bivariate_plot(), device = pdf)
  }
)
