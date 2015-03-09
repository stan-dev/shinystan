output$ui_density_customize <- renderUI({
  
  my_point_est <- "None"
  my_fill_color <- "gray35"
  my_line_color <- "lightgray"
  #   my_y_breaks <- "None"
  my_x_breaks <- "Some"
  my_CI <- "None"
  
  #   if (input$user_dens_customize == TRUE) {
  #     ok <- exists("shinystan_settings_density")
  #     validate(need(ok == TRUE, message = "Sorry, can't find any user density settings."))
  #     user_dens <- shinystan_settings_density
  #     my_point_est <- user_dens$point_est
  #     my_fill_color <- user_dens$fill_color
  #     my_line_color <- user_dens$line_color
  #     my_CI <- user_dens$CI
  #     my_y_breaks <- user_dens$y_breaks
  #     my_x_breaks <- user_dens$x_breaks
  #   }
  
  bsCollapse(
    bsCollapsePanel(title = "View Options", id = "density_collapse",
                    bsCollapse(
                      bsCollapsePanel(title = span(style = "color:#428bca;", "Options"), id = "density_options_collapse",
                                      fluidRow(
                                        column(3,numericInput("dens_chain", label = strong(style = "color: white;", "Chain (0 = all)"), min = 0, max = object@nChains, step = 1, value = 0)),
                                        column(3, conditionalPanel(condition = "input.dens_chain == 0",
                                                                   radioButtons("dens_chain_split", label = strong("All chains"), choices = c("Together", "Separate"), selected = "Together", inline = FALSE))),
                                        column(3, conditionalPanel(condition = "input.dens_chain_split == 'Together'", selectInput("dens_point_est", strong("Point est"), choices = c("None","Mean","Median","MAP"), selected = my_point_est))),
                                        column(3, conditionalPanel(condition = "input.dens_chain_split == 'Together'", selectInput("dens_ci", strong("CI %"), choices = c("None" = "None", "50%" = 0.5, "80%" = 0.8, "95%" = 0.95), selected = my_CI)))
                                      )
                      ),
                      bsCollapsePanel(title = span(style = "color:#428bca;", "Aesthetics"), id = "density_collors_collapse",
                                      fluidRow(
                                        column(3, conditionalPanel(condition = "input.dens_chain_split == 'Together'", selectInput("dens_x_breaks", strong("x breaks"), choices = c("None", "Some", "Many"), selected = my_x_breaks))),
                                        column(3, conditionalPanel(condition = "input.dens_chain_split == 'Together'", selectInput("dens_fill_color", strong(style = "color: white;", "Fill color"), choices = colors(), selected = my_fill_color, selectize = TRUE))),
                                        column(3, conditionalPanel(condition = "input.dens_chain_split == 'Together' && (input.dens_point_est != 'None' || input.dens_ci != 'None')", selectInput("dens_line_color", strong(style = "color: white;", "Line color"), choices = colors(), selected = my_line_color, selectize = TRUE)))
                                      )
                      ),
                      bsCollapsePanel(title = span(style = "color:#428bca;", "Add prior"), id = "density_prior_collapse",
                                      fluidRow(
                                        column(4, selectInput("dens_prior", "Family", choices = list("None", "Normal", "t", "Cauchy", "Exponential", "Gamma", "Inverse Gamma", "Beta"))),
                                        column(2, conditionalPanel(condition = "input.dens_prior == 'Normal'",
                                                                   numericInput("dens_prior_normal_mu", "Location", value = 0)
                                        ),
                                        conditionalPanel(condition = "input.dens_prior == 't'",
                                                         numericInput("dens_prior_t_df", "df", value = 1, min = 0)
                                        ),
                                        conditionalPanel(condition = "input.dens_prior == 'Cauchy'",
                                                         numericInput("dens_prior_cauchy_mu", "Location", value = 0)
                                        ),
                                        conditionalPanel(condition = "input.dens_prior == 'Beta'",
                                                         numericInput("dens_prior_beta_shape1", "Shape1", value = 1, min = 0)
                                        ),
                                        conditionalPanel(condition = "input.dens_prior == 'Exponential'",
                                                         numericInput("dens_prior_expo_rate", "Rate", value = 1, min = 0)
                                        ),
                                        conditionalPanel(condition = "input.dens_prior == 'Gamma'",
                                                         numericInput("dens_prior_gamma_shape", "Shape", value = 1, min = 0)
                                        ),
                                        conditionalPanel(condition = "input.dens_prior == 'Inverse Gamma'",
                                                         numericInput("dens_prior_inversegamma_shape", "Shape", value = 1, min = 0)
                                        )
                                        ),
                                        column(2, conditionalPanel(condition = "input.dens_prior == 'Normal'",
                                                                   numericInput("dens_prior_normal_sigma", "Scale", value = 1, min = 0)
                                        ),
                                        conditionalPanel(condition = "input.dens_prior == 't'",
                                                         numericInput("dens_prior_t_mu", "Location", value = 0)
                                        ),
                                        conditionalPanel(condition = "input.dens_prior == 'Cauchy'",
                                                         numericInput("dens_prior_cauchy_sigma", "Scale", value = 1, min = 0)
                                        ),
                                        conditionalPanel(condition = "input.dens_prior == 'Beta'",
                                                         numericInput("dens_prior_beta_shape2", "Shape2", value = 1, min = 0)
                                        ),
                                        conditionalPanel(condition = "input.dens_prior == 'Gamma'",
                                                         numericInput("dens_prior_gamma_rate", "Rate", value = 1, min = 0)
                                        ),
                                        conditionalPanel(condition = "input.dens_prior == 'Inverse Gamma'",
                                                         numericInput("dens_prior_inversegamma_scale", "Scale", value = 1, min = 0)
                                        )
                                        ),
                                        column(2, 
                                               conditionalPanel(condition = "input.dens_prior == 't'",
                                                                numericInput("dens_prior_t_sigma", "Scale", value = 1, min = 0)
                                               )
                                        )
                                      )
                                      
                      )
                    ),
                    hr(),
                    downloadButton("download_density", "Save as ggplot2 object")
    )
  )
})
