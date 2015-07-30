# This file is part of shinyStan
# Copyright (C) 2015 Jonah Sol Gabry & Stan Development Team
#
# shinyStan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinyStan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.

dens_transform_x <- eventReactive(
  input$dens_transform_x_go > 0,
  input$dens_transform_x
)

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
                    else if (prior_fam == "Normal") 
                      list(mean = input$dens_prior_normal_mu, sd = input$dens_prior_normal_sigma)
                    else if (prior_fam == "t") 
                      list(df = input$dens_prior_t_df, location = input$dens_prior_t_mu, scale = input$dens_prior_t_sigma)
                    else if (prior_fam == "Cauchy") 
                      list(location = input$dens_prior_cauchy_mu, scale = input$dens_prior_cauchy_sigma)
                    else if (prior_fam == "Beta") 
                      list(shape1 = input$dens_prior_beta_shape1, shape2 = input$dens_prior_beta_shape2)
                    else if (prior_fam == "Exponential") 
                      list(rate = input$dens_prior_expo_rate)
                    else if (prior_fam == "Gamma") 
                      list(shape = input$dens_prior_gamma_shape, rate = input$dens_prior_gamma_rate)
                    else if (prior_fam == "Inverse Gamma") 
                      list(shape = input$dens_prior_inversegamma_shape, scale = input$dens_prior_inversegamma_scale)
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
    transform_x = dens_transform_x()
  ))
})

output$density_plot_out <- renderPlot({
  density_plot()
}, bg = "transparent")

# download plot
output$download_density <- downloadHandler(
  filename = 'shinystan_density.RData',
  content = function(file) {
    shinystan_density <- density_plot()
    save(shinystan_density, file = file)
  }
)
