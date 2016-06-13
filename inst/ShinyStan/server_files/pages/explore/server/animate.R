# animate scatterplot
#Transforms would be nice, but let's start small

# Set options

# animate_package_options <- reactive({
# 
# animation::ani.options(interval = 1/input$frame_speed,ani.height=350)
# 
# })

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
      this_chain = input$animate_chain,
      frame_speed = input$frame_speed
  )
})  

output$animate_plot_out <- renderUI({
  if(input$animate_now==0) return(p("To animate a video of the MCMC estimates of the above parameters, 
                                    please click on the Animate button above. Please be aware that this 
                                    process can take a long time as a new graph must be prepared for each 
                                    frame of the following animation. The animation file is produced in .WEBM format, 
                                    which can be uploaded to Youtube.")) 
  # Return a list with a src attribute that equals the location of the GIF file
  output_info <- isolate(animate_plot())
  tags$video(src=output_info$src,height='450',type='video/webm; codecs="vp8.0,vorbis"',controls="controls")
})

# download
output$download_animate <- downloadHandler(
  filename = 'gg_animate_shinystan_download.webm',
  content = function(file) {
    # File already exists
   return(paste0('www/',animate_plot()$src))
  },
  contentType = 'video/webm'
)
