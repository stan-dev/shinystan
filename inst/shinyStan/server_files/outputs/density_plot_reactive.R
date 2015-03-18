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

  prior_fam <- input$dens_prior
  prior_params <- if (prior_fam == "None") NULL 
                    else if (prior_fam == "Normal") list(mean = input$dens_prior_normal_mu, sd = input$dens_prior_normal_sigma)
                    else if (prior_fam == "t") list(df = input$dens_prior_t_df, location = input$dens_prior_t_mu, scale = input$dens_prior_t_sigma)
                    else if (prior_fam == "Cauchy") list(location = input$dens_prior_cauchy_mu, scale = input$dens_prior_cauchy_sigma)
                    else if (prior_fam == "Beta") list(shape1 = input$dens_prior_beta_shape1, shape2 = input$dens_prior_beta_shape2)
                    else if (prior_fam == "Exponential") list(rate = input$dens_prior_expo_rate)
                    else if (prior_fam == "Gamma") list(shape = input$dens_prior_gamma_shape, rate = input$dens_prior_gamma_rate)
                    else if (prior_fam == "Inverse Gamma") list(shape = input$dens_prior_inversegamma_shape, scale = input$dens_prior_inversegamma_scale)
                    else NULL
  
  xzoom <- input$dens_xzoom != "Auto"
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
    x_breaks    = input$dens_x_breaks,
    xzoom = xzoom,
    x_lim = if (xzoom) eval(parse(text = input$dens_xzoom)) else NULL,
    prior_fam   = prior_fam,
    prior_params = prior_params,
    transform_x = input$dens_transform_x
  ))
})
