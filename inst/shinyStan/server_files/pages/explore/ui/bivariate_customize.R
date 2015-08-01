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

# span_gr <- function(...) {
#   span(style = "color: #222222; opacity: 1;", ...)
# }

output$ui_bivariate_customize <- renderUI({
  my_ellipse_lev <- "None"
  my_pt_size     <- 3.5
  my_pt_shape    <- 10
  my_pt_color    <- base_fill
  my_ellipse_color    <- vline_base_clr
  my_ellipse_lty      <- 1
  my_ellipse_lwd      <- 1
  my_ellipse_alpha    <- 1
  my_lines_color <- "gray"
  
  alpha_calc_pt <- function(N) {
    if (N <= 100) return(1)
    else if (N <= 200) return(0.75)
    else if (N >= 1500) return(0.15) 
    else 1 - pnorm(N/1500)
  }
  
  alpha_calc_lines <- function(N) {
    if (N < 50) return(0.5)
    if (N < 500) return(0.4)
    if (N < 1000) return(0.3)
    if (N < 5000) return(0.2)
    else return(0.1)
  }
  
  my_pt_alpha <- alpha_calc_pt(nIter)
  
  shinyjs::hidden(
    div(id = "bivariate_options",
        wellPanel(
          class = "optionswell",
          hr(class = "hroptions"),
          div(class = "divoptions_light",
              options_header("Transformation"),
              transform_helpText("y"),
              fluidRow(
                column(3, textInput("bivariate_transform_x", label = NULL, value = "x")),
                column(3, textInput("bivariate_transform_y", label = NULL, value = "y")),
                column(2, actionButton("bivariate_transform_go", label = "Transform"))
              )
          ),
          hr(class = "hroptions"),
          div(class = "divoptions_light",
              fluidRow(
                column(2, strong20("Points")),
                column(3, offset = 1, shinyjs::colourInput("bivariate_pt_color", strong20("Color"), my_pt_color)),
                column(2, numericInput("bivariate_pt_size", strong20("Size"), value = my_pt_size, min = 0, max = 10, step = 0.5)),
                column(2, numericInput("bivariate_pt_shape", strong20("Shape"), value = my_pt_shape, min = 1, max = 10, step = 1)),
                column(2, sliderInput("bivariate_pt_alpha", strong20("Opacity"), value = my_pt_alpha, min = 0, max = 1, step = 0.01, ticks = FALSE))
              )
          ),
          hr(class = "hroptions"),
          div(class = "divoptions_light",
              fluidRow(
                column(2, selectizeInput(inputId = "bivariate_ellipse_lev", 
                                         label = strong20("Ellipse"), 
                                         selected = my_ellipse_lev, 
                                         choices = list("None" = "None", "50%" = 0.5, "80%" = 0.8, "95%" = 0.95, "99%" = 0.99))),
                column(3, offset = 1, shinyjs::colourInput("bivariate_ellipse_color", strong20("Color"), my_ellipse_color)),
                column(2, numericInput("bivariate_ellipse_lwd", strong20("Size"), value = my_ellipse_lwd, min = 0, max = 5, step = 0.5)),
                column(2, numericInput("bivariate_ellipse_lty", strong20("Shape"), value = my_ellipse_lty, min = 1, max = 6, step = 1)),
                column(2, sliderInput("bivariate_ellipse_alpha", strong20("Opacity"), value = my_ellipse_alpha, min = 0, max = 1, step = 0.01, ticks = FALSE))
              )
          ),
          hr(class = "hroptions"),
          div(class = "divoptions_light",
              fluidRow(
                column(2, selectizeInput(inputId = "bivariate_lines", 
                                         label = strong20("Lines"), 
                                         choices = c(Hide = "hide", Back = "back", Front = "front"), selected = "back")),
                column(3, offset = 1, shinyjs::colourInput("bivariate_lines_color", strong20("Color"), my_lines_color)),
                column(2, sliderInput("bivariate_lines_alpha", label = strong20("Opacity"), value = alpha_calc_lines(nIter), min = 0, max = 1, step = 0.01, ticks = FALSE))
              )
          )
        )
    )
  )
})

#   if (input$user_contour_customize == TRUE) {
#     ok <- exists("shinystan_settings_contour")
#     validate(need(ok == TRUE, message = "Sorry, can't find any user bivariate plot settings."))
#     user_contour <- shinystan_settings_contour
#     ops <- user_contour[["ops"]]
#
#     my_ellipse_lev <- ops$ellipselev
#     my_pt_alpha    <- ops$pt_alpha
#     my_pt_size     <- ops$pt_size
#     my_pt_shape    <- ops$pt_shape
#     my_pt_color    <- ops$pt_color
#     my_ellipsecolor    <- ops$ellipsecolor
#     my_ellipselty      <- ops$ellipselty
#     my_ellipselwd      <- ops$ellipselwd
#     my_ellipsealpha    <- ops$ellipsealpha
#   }
