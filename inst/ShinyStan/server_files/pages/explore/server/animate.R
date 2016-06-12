# # animate scatterplot
#Transforms would be nice, but let's start small

# bivariate_transform_x <-
#   eventReactive(input$bivariate_transform_go > 0, input$bivariate_transform_x)
# bivariate_transform_y <-
#   eventReactive(input$bivariate_transform_go > 0, input$bivariate_transform_y)
animate_plot <- reactive({
  validate(
    need(input$param, message = FALSE),
#    need(input$bivariate_ellipse_lev, message = FALSE),
    need(input$animate_param_x, message = FALSE)
  )
  if(input$animate_now==0) return(list(src='blank.png'))
  
#   if (!is.null(input$bivariate_ellipse_lev)) {
#     validate(
#       need(is.numeric(input$bivariate_pt_size), message = "Point size must be numeric"),
#       need(is.numeric(input$bivariate_pt_shape), message = "Point shape must be numeric")
#     )
#     
#     if (input$bivariate_ellipse_lev != "None") {
#       validate(
#         need(
#           input$param != input$bivariate_param_y,
#           "For this option the x and y can't be the same parameter."
#         ),
#         need(
#           is.numeric(input$bivariate_ellipse_lwd),
#           message = "Ellipse size must be numeric"
#         ),
#         need(
#           is.numeric(input$bivariate_ellipse_lty),
#           message = "Ellipse shape must be numeric"
#         )
#       )
#     }
#   }
  
  isolate(.animate_plot(
      samps = SAMPS_post_warmup,
      sp = if (!identical(SAMPLER_PARAMS_post_warmup, FALSE)) 
        SAMPLER_PARAMS_post_warmup else NULL,
      max_td = if ("max_td" %in% names(MISC)) MISC$max_td else NULL,
      param = input$param,
      param2 = input$animate_param_x
#       pt_alpha = input$bivariate_pt_alpha,
#       pt_size = input$bivariate_pt_size,
#       pt_shape = input$bivariate_pt_shape,
#       pt_color = input$bivariate_pt_color,
#       ellipse_lev = input$bivariate_ellipse_lev,
#       ellipse_color = input$bivariate_ellipse_color,
#       ellipse_lty = input$bivariate_ellipse_lty,
#       ellipse_lwd = input$bivariate_ellipse_lwd,
#       ellipse_alpha = input$bivariate_ellipse_alpha,
#       lines  = input$bivariate_lines,
#       lines_color = input$bivariate_lines_color,
#       lines_alpha = input$bivariate_lines_alpha,
#       transform_x = bivariate_transform_x(),
#       transfor)
  ))
})  

output$animate_plot_out <- renderImage({
  # Return a list with a src attribute that equals the location of the GIF file
  animate_plot()
})

# download
output$download_animate <- downloadHandler(
  filename = 'gg_animate_shinystan.gif',
  content = function(file) {
    # File already exists, do nothing
    
  }
)
