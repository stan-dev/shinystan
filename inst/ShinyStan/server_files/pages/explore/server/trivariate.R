# trivariate scatterplot
trivariate_transform_x <-
  eventReactive(input$trivariate_transform_go > 0,
                input$trivariate_transform_x)
trivariate_transform_y <-
  eventReactive(input$trivariate_transform_go > 0,
                input$trivariate_transform_y)
trivariate_transform_z <-
  eventReactive(input$trivariate_transform_go > 0,
                input$trivariate_transform_z)

trivariate_plot <- reactive({
  validate(
    need(input$trivariate_flip, message = "Loading..."),
    need(input$trivariate_param_x, message = "Waiting for x ..."),
    need(input$trivariate_param_y, message = "Waiting for y ..."),
    need(input$trivariate_param_z, message = "Waiting for z ...")
  )
  x <- input$trivariate_param_x
  y <- input$trivariate_param_y
  z <- input$trivariate_param_z
  samps <- SAMPS_post_warmup
  do.call(
    ".param_trivariate",
    args = list(
      params = c(x, y, z),
      samps = samps,
      pt_color = input$trivariate_pt_color,
      pt_size = input$trivariate_pt_size,
      show_grid = input$trivariate_grid == "show",
      flip_y = input$trivariate_flip == "flip",
      transform_x = trivariate_transform_x(),
      transform_y = trivariate_transform_y(),
      transform_z = trivariate_transform_z()
    )
  )
})

output$trivariate_plot_out <- threejs::renderScatterplotThree({
  trivariate_plot()
})
