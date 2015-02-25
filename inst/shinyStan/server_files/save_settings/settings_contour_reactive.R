save_settings_contour <- reactive({
  type <- input$contour_type
  type_contour <- type == "Contour"
  type_point <- type == "Point"
  type_scatter <- type == "Scatter"

  if (type_contour) {
    ops <- list(
      nBins = input$contour_bins,
      high_color = input$contour_high_color,
      low_color = input$contour_low_color
    )
  }
  if (type_point) {
    ops <- list(
      high_color = input$point_high_color,
      low_color = input$point_low_color
    )
  }
  if (type_scatter) {
    ops <- list(
    pt_alpha    = input$scatter_pt_alpha,
    pt_size     = input$scatter_pt_size,
    pt_shape    = input$scatter_pt_shape,
    pt_color    = input$scatter_pt_color,
    ci_lev      = input$scatter_ellipse_lev,
    ci_color    = input$scatter_ellipse_color,
    ci_lty      = input$scatter_ellipse_lty,
    ci_lwd      = input$scatter_ellipse_lwd,
    ci_alpha    = input$scatter_ellipse_alpha
    )
  }

  out <- list(
    type  = type,
    ops   = ops
  )
  out
})
