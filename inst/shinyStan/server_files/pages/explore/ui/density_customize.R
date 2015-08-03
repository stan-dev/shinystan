# This file is part of shinystan
# Copyright (C) Jonah Gabry
#
# shinystan is free software; you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software
# Foundation; either version 3 of the License, or (at your option) any later
# version.
# 
# shinystan is distributed in the hope that it will be useful, but WITHOUT ANY
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
# A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with
# this program; if not, see <http://www.gnu.org/licenses/>.


condPanel_together <- function(...) {
  conditionalPanel(condition = "input.dens_chain_split == 'Together'", ...)
}
condPanel_prior <- function(dist, ...) {
  cond <- paste0("input.dens_prior ==","'", dist,"'")
  conditionalPanel(cond, ...)
}

output$ui_density_customize <- renderUI({
  
  my_point_est <- "None"
  my_fill_color <- base_fill
  my_line_color <- vline_base_clr
  my_x_breaks <- "Some"
  my_CI <- "None"
  
  shinyjs::hidden(
    div(id = "density_options",
        wellPanel(
          class = "optionswell",
          hr(class = "hroptions"),
          strongBig("Transformation"),
          transform_helpText(),
          fluidRow(
            column(4, textInput("dens_transform_x", label = NULL, value = "x")),
            column(2, actionButton("dens_transform_x_go", label = "Transform"))
          ),
          hr(class = "hroptions"),
          selectInput("dens_options_display", label = strongBig("Control"),
                      choices = c("Options", "Aesthetics", "Compare to function" = "Compare"),
                      selected = "Options", width = "50%"),
          conditionalPanel(condition = "input.dens_options_display == 'Options'",
                           fluidRow(
                             column(3, numericInput("dens_chain", label = strongMed("Chain"), min = 0, max = object@nChains, step = 1, value = 0)),
                             column(3, conditionalPanel(condition = "input.dens_chain == 0",
                                                        radioButtons("dens_chain_split", label = strongMed("All chains"), choices = c("Together", "Separate"), selected = "Together", inline = FALSE))),
                             column(3, selectInput("dens_point_est", strongMed("Point est"), choices = c("None","Mean","Median","MAP"), selected = my_point_est)),
                             column(3, selectInput("dens_ci", strongMed("CI %"), choices = c("None" = "None", "50%" = 0.5, "80%" = 0.8, "95%" = 0.95), selected = my_CI))
                           )
          ),
          conditionalPanel(condition = "input.dens_options_display == 'Aesthetics'",
                           fluidRow(
                             column(3, selectInput("dens_x_breaks", strongMed("x breaks"), choices = c("None", "Some", "Many"), selected = my_x_breaks)),
                             column(3, shinyjs::colourInput("dens_fill_color", strongMed("Fill"), my_fill_color)), 
                             column(3, shinyjs::colourInput("dens_line_color", strongMed("Line"), my_line_color)) 
                           )
          ),
          conditionalPanel(condition = "input.dens_options_display == 'Compare'",
                           fluidRow(
                             column(4, selectInput("dens_prior", strongMed("Family"), choices = list("None", "Normal", "t", "Cauchy", "Exponential", "Gamma", "Inverse Gamma", "Beta"))),
                             column(2, 
                                    condPanel_prior("Normal", numericInput("dens_prior_normal_mu", "Location", value = 0, step = 0.1)),
                                    condPanel_prior("t", numericInput("dens_prior_t_df", "df", value = 1, min = 0, step = 0.1)
                                    ),
                                    condPanel_prior("Cauchy", 
                                                    numericInput("dens_prior_cauchy_mu", "Location", value = 0, step = 0.1)
                                    ),
                                    condPanel_prior("Beta", 
                                                    numericInput("dens_prior_beta_shape1", "Shape1", value = 1, min = 0, step = 0.1)
                                    ),
                                    condPanel_prior("Exponential", 
                                                    numericInput("dens_prior_expo_rate", "Rate", value = 1, min = 0, step = 0.1)
                                    ),
                                    condPanel_prior("Gamma", 
                                                    numericInput("dens_prior_gamma_shape", "Shape", value = 1, min = 0, step = 0.1)
                                    ),
                                    condPanel_prior("Inverse Gamma", 
                                                    numericInput("dens_prior_inversegamma_shape", "Shape", value = 1, min = 0, step = 0.1)
                                    )
                             ),
                             column(2, condPanel_prior("Normal",
                                                       numericInput("dens_prior_normal_sigma", "Scale", value = 1, min = 0, step = 0.1)
                             ),
                             condPanel_prior("t",
                                             numericInput("dens_prior_t_mu", "Location", value = 0, step = 0.1)
                             ),
                             condPanel_prior("Cauchy",
                                             numericInput("dens_prior_cauchy_sigma", "Scale", value = 1, min = 0, step = 0.1)
                             ),
                             condPanel_prior("Beta",
                                             numericInput("dens_prior_beta_shape2", "Shape2", value = 1, min = 0, step = 0.1)
                             ),
                             condPanel_prior("Gamma",
                                             numericInput("dens_prior_gamma_rate", "Rate", value = 1, min = 0, step = 0.1)
                             ),
                             condPanel_prior("Inverse Gamma",
                                             numericInput("dens_prior_inversegamma_scale", "Scale", value = 1, min = 0, step = 0.1)
                             )
                             ),
                             column(2, 
                                    condPanel_prior("t",
                                                    numericInput("dens_prior_t_sigma", "Scale", value = 1, min = 0, step = 0.1)
                                    )
                             )
                           ),
                           condPanel_together(
                             textInput("dens_xzoom", label = strongMed("x-axis limits"), value = "c(min, max)")
                           ),
                           br()
          )
        )
    )
  )
})
