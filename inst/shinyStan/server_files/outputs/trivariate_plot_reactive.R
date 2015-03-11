trivariate_plot <- reactive({
  validate(need(input$trivariate_flip, message = "Loading..."),
           need(input$trivariate_param_x, message = "Waiting for x ..."),
           need(input$trivariate_param_y, message = "Waiting for y ..."),
           need(input$trivariate_param_z, message = "Waiting for z ..."))

  x <- input$trivariate_param_x
  y <- input$trivariate_param_y
  z <- input$trivariate_param_z
  pt_size <- input$trivariate_pt_size
  grid <- input$trivariate_grid == "show"
  flip_y <- input$trivariate_flip == "flip"
  warmup <- input$trivariate_warmup == "include"
  
  samps <- if (warmup) samps_all else samps_post_warmup
  
  do.call(".param_trivariate", args = list(
    params = c(x, y, z),
    samps = samps,
    pt_size = pt_size,
    show_grid = grid,
    flip_y = flip_y
  ))

})
