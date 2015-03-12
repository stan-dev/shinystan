trivariate_plot <- reactive({
  test_transform_x <- function(x) is.function(eval(parse(text = paste0("function(x) {",x,"}"))))
  test_transform_y <- function(y) is.function(eval(parse(text = paste0("function(y) {",y,"}"))))
  test_transform_z <- function(z) is.function(eval(parse(text = paste0("function(z) {",z,"}"))))
  validate(need(input$trivariate_flip, message = "Loading..."),
           need(input$trivariate_param_x, message = "Waiting for x ..."),
           need(input$trivariate_param_y, message = "Waiting for y ..."),
           need(input$trivariate_param_z, message = "Waiting for z ..."),
           need(try(test_transform_x(input$trivariate_transform_x)), message = FALSE),
           need(try(test_transform_y(input$trivariate_transform_y)), message = FALSE),
           need(try(test_transform_z(input$trivariate_transform_z)), message = FALSE))

  x <- input$trivariate_param_x
  y <- input$trivariate_param_y
  z <- input$trivariate_param_z

  warmup <- input$trivariate_warmup == "include"
  samps <- if (warmup) samps_all else samps_post_warmup
  
  do.call(".param_trivariate", args = list(
    params = c(x, y, z),
    samps = samps,
    pt_color = input$trivariate_pt_color,
    pt_size = input$trivariate_pt_size,
    show_grid = input$trivariate_grid == "show",
    flip_y = input$trivariate_flip == "flip",
    transform_x      = input$trivariate_transform_x,
    transform_y      = input$trivariate_transform_y,
    transform_z      = input$trivariate_transform_z
  ))

})
