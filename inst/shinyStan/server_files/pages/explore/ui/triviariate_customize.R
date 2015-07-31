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

strong_bl <- function(...) {
  strong(style = "color: #006DCC;", ...)
}

output$ui_trivariate_select <- renderUI({
  fluidRow(
    column(3, selectizeInput("trivariate_param_x", label = strong_bl("x-axis"), 
                             choices = .make_param_list(object), selected = input$param, 
                             multiple = FALSE)),
    column(3, selectizeInput("trivariate_param_y", label = strong_bl("y-axis"), 
                             choices = .make_param_list(object), selected = .make_param_list(object)[1], 
                             multiple = FALSE)),
    column(3, selectizeInput("trivariate_param_z", label = strong_bl("z-axis"), 
                             choices = rev(.make_param_list(object)), 
                             multiple = FALSE))
  )
})

output$ui_triviariate_customize <- renderUI({
  shinyjs::hidden(
    div(id = "trivariate_options",
        wellPanel(
          class = "optionswell",
          fluidRow(
            column(3, shinyjs::colourInput("trivariate_pt_color", strong("Color"), value = base_fill)),
            column(3, sliderInput("trivariate_pt_size", strong("Size"), value = 0.5, min = 0, max = 2, step = 0.1, ticks = FALSE)),
            column(2, radioButtons("trivariate_warmup", strong("Warmup"), choices = list(Include = "include", Omit = "omit"), selected = "omit", inline = FALSE)),
            column(2, radioButtons("trivariate_grid", strong("Grid"), choices = list(Show = "show", Hide = "hide"), selected = "show", inline = FALSE)),
            column(2, radioButtons("trivariate_flip", strong("y-axis"), choices = list(Normal = "normal", Flipped = "flip"), selected = "normal", inline = FALSE))
          ),
          hr(class = "hroptions"),
          options_header("Transformation"),
          transform_helpText("y,z"),
          fluidRow(
            column(3, textInput("trivariate_transform_x", label = NULL, value = "x")),
            column(3, textInput("trivariate_transform_y", label = NULL, value = "y")),
            column(3, textInput("trivariate_transform_z", label = NULL, value = "z")),
            column(2, actionButton("trivariate_transform_go", label = "Transform"))
          )
        )
    )
  )
  
#   bsCollapse(
#     bsCollapsePanel(title = "View Options", id = "trivariate_collapse",
#                     fluidRow(
#                       column(3, shinyjs::colourInput("trivariate_pt_color", strong("Color"), value = base_fill)),
#                       column(3, sliderInput("trivariate_pt_size", strong("Size"), value = 0.5, min = 0, max = 2, step = 0.1, ticks = FALSE)),
#                       column(2, radioButtons("trivariate_warmup", strong("Warmup"), choices = list(Include = "include", Omit = "omit"), selected = "omit", inline = FALSE)),
#                       column(2, radioButtons("trivariate_grid", strong("Grid"), choices = list(Show = "show", Hide = "hide"), selected = "show", inline = FALSE)),
#                       column(2, radioButtons("trivariate_flip", strong("y-axis"), choices = list(Normal = "normal", Flipped = "flip"), selected = "normal", inline = FALSE))
#                     ),
#                     hr(),
#                     h5(style = "color: #006DCC;", "Controlling the dynamic 3D scatterplot"),
#                     helpText(style = "color: white; font-size: 12px;", "Use your mouse or trackpad to rotate the plot and zoom in or out.")
#     )
#   )
})
