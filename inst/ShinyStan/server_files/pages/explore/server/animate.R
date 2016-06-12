# animate scatterplot
#Transforms would be nice, but let's start small



animate_plot <- reactive({
  validate(
    need(input$param, message = FALSE),
    need(input$animate_ellipse_lev, message = FALSE),
    need(input$animate_param_x, message = FALSE)
  )

  
  if (!is.null(input$animate_ellipse_lev)) {
    validate(
      need(is.numeric(input$animate_pt_size), message = "Point size must be numeric"),
      need(is.numeric(input$animate_pt_shape), message = "Point shape must be numeric")
    )
    
    if (input$animate_ellipse_lev != "None") {
      validate(
        need(
          input$param != input$animate_param_x,
          "For this option the x and y can't be the same parameter."
        ),
        need(
          is.numeric(input$animate_ellipse_lwd),
          message = "Ellipse size must be numeric"
        ),
        need(
          is.numeric(input$animate_ellipse_lty),
          message = "Ellipse shape must be numeric"
        )
      )
    }
  }
  
  .animate_plot(
      samps = SAMPS_post_warmup,
      sp = if (!identical(SAMPLER_PARAMS_post_warmup, FALSE)) 
        SAMPLER_PARAMS_post_warmup else NULL,
      max_td = if ("max_td" %in% names(MISC)) MISC$max_td else NULL,
      param = input$animate_param_y,
      param2 = input$animate_param_x,
      pt_alpha = input$animate_pt_alpha,
      pt_size = input$animate_pt_size,
      pt_shape = input$animate_pt_shape,
      pt_color = input$animate_pt_color,
      ellipse_lev = input$animate_ellipse_lev,
      ellipse_color = input$animate_ellipse_color,
      ellipse_lty = input$animate_ellipse_lty,
      ellipse_lwd = input$animate_ellipse_lwd,
      ellipse_alpha = input$animate_ellipse_alpha,
      lines  = input$animate_lines,
      lines_color = input$animate_lines_color,
      lines_alpha = input$animate_lines_alpha,
      transform_x = input$animate_transform_x,
      transform_y = input$animate_transform_y,
      frame_speed = input$animate_framespeed,
      this_chain = input$animate_chain
  )
})  

output$animate_plot_out <- renderImage({
  if(input$animate_now==0) return(list(src='blank.png')) 
  # Return a list with a src attribute that equals the location of the GIF file
  isolate(animate_plot())
})

# download
output$download_animate <- downloadHandler(
  filename = 'gg_animate_shinystan_download.gif',
  content = function(file) {
    # File already exists
    gif_file <- animate_plot()
    file.copy(gif_file$src,file)
  },
  contentType = 'image/gif'
)
