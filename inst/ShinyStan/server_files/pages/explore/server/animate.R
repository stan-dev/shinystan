# animate scatterplot options

animate_plot <- reactive({
  validate(
    need(input$param, message = FALSE),
    need(input$animate_ellipse_lev, message = FALSE),
    need(input$animate_param_x, message = FALSE)
  )
  
  if(length(input$animate_param_x)>1) 
    validate(need(input$animate_chain!='All',
              message='Please select a specific chain if examining more than one x variable.'))

  
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
      samps = SAMPS_all,
      sp = SAMPLER_PARAMS,
      max_td = if ("max_td" %in% names(MISC)) MISC$max_td else NULL,
      param = input$animate_param_y,
      param2 = input$animate_param_x,
      pt_alpha = input$animate_pt_alpha,
      pt_size = input$animate_pt_size,
      pt_shape = input$animate_pt_shape,
      ellipse_lev = input$animate_ellipse_lev,
      ellipse_lty = input$animate_ellipse_lty,
      ellipse_lwd = input$animate_ellipse_lwd,
      ellipse_alpha = input$animate_ellipse_alpha,
      lines  = input$animate_lines,
      lines_alpha = input$animate_lines_alpha,
      transform_x = input$animate_transform_x,
      transform_y = input$animate_transform_y,
      this_chain = input$animate_chain,
      frame_speed = input$frame_speed,
      row_min = input$animate_iters[1],
      row_max = input$animate_iters[2],
      standardize = input$animate_standardize,
      colour_palette = input$animate_color,
      tween_ratio = input$frame_tween,
      top_title = input$animate_title,
      height = input$animate_height,
      width = input$animate_width,
      resolution = input$animate_resolution
  )
})  

output$animate_plot_out <- renderUI({
  if(input$animate_now==0) return(includeMarkdown('markdown/about_video.md'))
  # Return a list with a src attribute that equals the location of the WEBM file
  output_info <- isolate(animate_plot())
  tags$video(src=output_info$src,height='500',width='100%',type='video/webm; codecs="vp8.0,vorbis"',controls="controls")
})

# download
output$download_animate <- downloadHandler(
  filename = 'gg_animate_shinystan_download.webm',
  content = function(file) {

    file.copy('www/gg_animate_shinystan.webm',file)
  },
  contentType = 'video/webm'
)
