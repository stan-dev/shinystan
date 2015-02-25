# two functions: density_plot

density_plot <- reactive({

  validate(need(input$param, message = FALSE),
           need(!is.null(input$dens_chain), message = FALSE))

  chain <- input$dens_chain
  if (is.na(chain)) chain <- 0

#   customize <- input$dens_collapse == "open"
#   customize <- input$dens_customize
#   if (customize & is.null(input$dens_x_breaks)) {
#     # delay until the customization inputs are ready
#     return()
#   }

  do.call(".param_dens", args = list(
    param       = input$param,
    dat         = par_samps_post_warmup(),
    chain       = chain,
    chain_split = input$dens_chain_split == "Separate",
    fill_color  = input$dens_fill_color,
    line_color  = input$dens_line_color,
    point_est   = input$dens_point_est,
    CI          = input$dens_ci,
#     y_breaks    = input$dens_y_breaks,
    x_breaks    = input$dens_x_breaks
  ))
})
