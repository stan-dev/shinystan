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

shinyjs::hidden(
  div(id = "bivariate_options",
      wellPanel(
        class = "optionswell",
        hr(class = "hroptions"),
        strongBig("Transformation"),
        transform_helpText("x,y"),
        fluidRow(
          column(3, transformation_selectInput("bivariate_transform_x")),
          column(3, transformation_selectInput("bivariate_transform_y")),
          column(2, actionButton("bivariate_transform_go", label = "Transform",
                                 class = "transform-go"))
        ),
        hr(class = "hroptions"),
        selectInput("bivariate_options_display", label = strongBig("Control"),
                    choices = c("Points", "Ellipse", "Lines"),
                    selected = "Points", width = "50%"),
        conditionalPanel(condition = "input.bivariate_options_display == 'Points'",
                         fluidRow(
                           column(3, shinyjs::colourInput("bivariate_pt_color", strongMed("Color"), base_fill)),
                           column(2, numericInput("bivariate_pt_size", strongMed("Size"), value = 3.5, min = 0, max = 10, step = 0.5)),
                           column(2, numericInput("bivariate_pt_shape", strongMed("Shape"), value = 10, min = 1, max = 10, step = 1)),
                           column(2, sliderInput("bivariate_pt_alpha", strongMed("Opacity"), value = alpha_calc_pt(.nIter), min = 0, max = 1, step = 0.01, ticks = FALSE))
                         )
        ),
        conditionalPanel(condition = "input.bivariate_options_display == 'Ellipse'",
                         fluidRow(
                           column(2, selectizeInput(inputId = "bivariate_ellipse_lev", 
                                                    label = strongMed("Type"), 
                                                    selected = "None", 
                                                    choices = list("None" = "None", "50%" = 0.5, "80%" = 0.8, "95%" = 0.95, "99%" = 0.99))),
                           column(3, shinyjs::colourInput("bivariate_ellipse_color", strongMed("Color"), vline_base_clr)),
                           column(2, numericInput("bivariate_ellipse_lwd", strongMed("Size"), value = 1, min = 0, max = 5, step = 0.5)),
                           column(2, numericInput("bivariate_ellipse_lty", strongMed("Shape"), value = 1, min = 1, max = 6, step = 1)),
                           column(2, sliderInput("bivariate_ellipse_alpha", strongMed("Opacity"), value = 1, min = 0, max = 1, step = 0.01, ticks = FALSE))
                         )
        ),
        conditionalPanel(condition = "input.bivariate_options_display == 'Lines'",
                         fluidRow(
                           column(2, selectizeInput(inputId = "bivariate_lines", 
                                                    label = strongMed("Position"), 
                                                    choices = c(Hide = "hide", Back = "back", Front = "front"), selected = "back")),
                           column(3, shinyjs::colourInput("bivariate_lines_color", strongMed("Color"), "gray")),
                           column(2, sliderInput("bivariate_lines_alpha", label = strongMed("Opacity"), value = alpha_calc_lines(.nIter), 
                                                 min = 0, max = 1, step = 0.01, ticks = FALSE))
                         )
        )
      )
  )
)

