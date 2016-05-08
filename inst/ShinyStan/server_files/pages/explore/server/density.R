# kernel density plot
dens_transform_x <- eventReactive(
  input$dens_transform_x_go > 0,
  input$dens_transform_x
)

user_xlim <- function(lim) {
  xz <- strsplit(lim, split = "c(", fixed = TRUE)[[1L]][2]
  xz <- strsplit(xz, split = ",", fixed = TRUE)[[1L]]
  if (identical(xz, NA_character_))
    return(FALSE)
  x_lim <- unlist(strsplit(xz, split = ")", fixed = TRUE))
  x_lim <- gsub(" ", "", x_lim)
  if (x_lim[1L] == "min")
    x_lim[1L] <- NA
  if (x_lim[2L] == "max")
    x_lim[2L] <- NA
  as.numeric(x_lim)
}

density_plot <- reactive({
  xzoom <- input$dens_xzoom
  if (xzoom == "")
    return(last_plot())
  
  validate(
    need(input$param, message = FALSE),
    need(!is.null(input$dens_chain), message = FALSE),
    need(xzoom, message = FALSE)
  )
  
  x_lim <- if (xzoom == "c(min, max)") {
    NULL
  } else {
    check <- try(user_xlim(xzoom))
    validate(need(check, message = "Invalid input"))
    check
  }
  
  chain <- input$dens_chain
  if (is.na(chain))
    chain <- 0
  prior_fam <- input$dens_prior
  prior_params <- if (prior_fam == "None") {
    NULL
  } else if (prior_fam == "Normal") {
    list(
      mean = input$dens_prior_normal_mu,
      sd = input$dens_prior_normal_sigma
    )
  } else if (prior_fam == "t") {
    list(
      df = input$dens_prior_t_df,
      location = input$dens_prior_t_mu,
      scale = input$dens_prior_t_sigma
    )
  } else if (prior_fam == "Cauchy") {
    list(
      location = input$dens_prior_cauchy_mu,
      scale = input$dens_prior_cauchy_sigma
    )
  } else if (prior_fam == "Beta") {
    list(
      shape1 = input$dens_prior_beta_shape1,
      shape2 = input$dens_prior_beta_shape2
    )
  } else if (prior_fam == "Exponential") {
    list(rate = input$dens_prior_expo_rate)
  } else if (prior_fam == "Gamma") {
    list(
      shape = input$dens_prior_gamma_shape,
      rate = input$dens_prior_gamma_rate
    )
  } else if (prior_fam == "Inverse Gamma") {
    list(
      shape = input$dens_prior_inversegamma_shape,
      scale = input$dens_prior_inversegamma_scale
    )
  } else {
    NULL
  }
  
  do.call(
    ".param_dens",
    args = list(
      param = input$param,
      dat = par_samps_post_warmup(),
      chain = chain,
      chain_split = input$dens_chain_split == "Separate",
      fill_color = input$dens_fill_color,
      line_color = input$dens_line_color,
      point_est = input$dens_point_est,
      CI = input$dens_ci,
      #     y_breaks    = input$dens_y_breaks,
      x_breaks = input$dens_x_breaks,
      x_lim = x_lim,
      prior_fam = prior_fam,
      prior_params = prior_params,
      transform_x = dens_transform_x()
    )
  )
})


output$density_plot_out <- renderPlot({
  suppress_and_print(density_plot())
}, bg = "transparent")

# download plot
output$download_density <- downloadHandler(
  filename = 'shinystan-density-gg.RData',
  content = function(file) {
    shinystan_density_gg <- density_plot()
    save(shinystan_density_gg, file = file)
  }
)
output$save_pdf_density = downloadHandler(
  filename = "shinstan-density.pdf",
  content = function(file) {
    ggsave(file, plot = density_plot(), device = pdf)
  }
)
