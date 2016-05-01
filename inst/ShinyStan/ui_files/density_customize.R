shinyjs::hidden(
  div(id = "density_options",
      wellPanel(
        class = "optionswell",
        hr(class = "hroptions"),
        strongBig("Transformation"),
        transform_helpText("x"),
        fluidRow(
          column(4, transformation_selectInput("dens_transform_x")),
          column(2, actionButton("dens_transform_x_go", label = "Transform", 
                                 class = "transform-go"))
        ),
        hr(class = "hroptions"),
        selectInput("dens_options_display", label = strongBig("Control"),
                    choices = c("Options", "Aesthetics", "Compare to function" = "Compare"),
                    selected = "Options", width = "50%"),
        conditionalPanel(condition = "input.dens_options_display == 'Options'",
                         fluidRow(
                           column(3, numericInput("dens_chain", label = strongMed("Chain"), min = 0, max = .nChains, step = 1, value = 0)),
                           column(3, conditionalPanel(condition = "input.dens_chain == 0",
                                                      radioButtons("dens_chain_split", label = strongMed("All chains"), choices = c("Together", "Separate"), selected = "Together", inline = FALSE))),
                           column(3, selectInput("dens_point_est", strongMed("Point est"), choices = c("None","Mean","Median","MAP"), selected = "None")),
                           column(3, selectInput("dens_ci", strongMed("CI %"), choices = c("None" = "None", "50%" = 0.5, "80%" = 0.8, "95%" = 0.95), selected = "None"))
                         )
        ),
        conditionalPanel(condition = "input.dens_options_display == 'Aesthetics'",
                         fluidRow(
                           column(3, selectInput("dens_x_breaks", strongMed("x breaks"), choices = c("None", "Some", "Many"), selected = "Some")),
                           column(3, shinyjs::colourInput("dens_fill_color", strongMed("Fill"), base_fill)), 
                           column(3, shinyjs::colourInput("dens_line_color", strongMed("Line"), vline_base_clr)) 
                         )
        ),
        conditionalPanel(condition = "input.dens_options_display == 'Compare'",
                         fluidRow(
                           column(4, selectInput("dens_prior", strongMed("Family"), choices = list("None", "Normal", "t", "Cauchy", "Exponential", "Gamma", "Inverse Gamma", "Beta"))),
                           column(2, 
                                  condPanel_dens_prior("Normal", numericInput("dens_prior_normal_mu", "Location", value = 0, step = 0.1)),
                                  condPanel_dens_prior("t", numericInput("dens_prior_t_df", "df", value = 1, min = 0, step = 0.1)
                                  ),
                                  condPanel_dens_prior("Cauchy", 
                                                       numericInput("dens_prior_cauchy_mu", "Location", value = 0, step = 0.1)
                                  ),
                                  condPanel_dens_prior("Beta", 
                                                       numericInput("dens_prior_beta_shape1", "Shape1", value = 1, min = 0, step = 0.1)
                                  ),
                                  condPanel_dens_prior("Exponential", 
                                                       numericInput("dens_prior_expo_rate", "Rate", value = 1, min = 0, step = 0.1)
                                  ),
                                  condPanel_dens_prior("Gamma", 
                                                       numericInput("dens_prior_gamma_shape", "Shape", value = 1, min = 0, step = 0.1)
                                  ),
                                  condPanel_dens_prior("Inverse Gamma", 
                                                       numericInput("dens_prior_inversegamma_shape", "Shape", value = 1, min = 0, step = 0.1)
                                  )
                           ),
                           column(2, condPanel_dens_prior("Normal",
                                                          numericInput("dens_prior_normal_sigma", "Scale", value = 1, min = 0, step = 0.1)
                           ),
                           condPanel_dens_prior("t",
                                                numericInput("dens_prior_t_mu", "Location", value = 0, step = 0.1)
                           ),
                           condPanel_dens_prior("Cauchy",
                                                numericInput("dens_prior_cauchy_sigma", "Scale", value = 1, min = 0, step = 0.1)
                           ),
                           condPanel_dens_prior("Beta",
                                                numericInput("dens_prior_beta_shape2", "Shape2", value = 1, min = 0, step = 0.1)
                           ),
                           condPanel_dens_prior("Gamma",
                                                numericInput("dens_prior_gamma_rate", "Rate", value = 1, min = 0, step = 0.1)
                           ),
                           condPanel_dens_prior("Inverse Gamma",
                                                numericInput("dens_prior_inversegamma_scale", "Scale", value = 1, min = 0, step = 0.1)
                           )
                           ),
                           column(2, 
                                  condPanel_dens_prior("t",
                                                       numericInput("dens_prior_t_sigma", "Scale", value = 1, min = 0, step = 0.1)
                                  )
                           )
                         ),
                         condPanel_dens_together(
                           textInput("dens_xzoom", label = strongMed("x-axis limits"), value = "c(min, max)")
                         ),
                         br()
        )
      )
  )
)
