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
  div(id = "trivariate_options",
      wellPanel(
        class = "optionswell",
        hr(class = "hroptions"),
        strongBig("Transformation"),
        transform_helpText("x,y,z"),
        fluidRow(
          column(3, transformation_selectInput("trivariate_transform_x")),
          column(3, transformation_selectInput("trivariate_transform_y")),
          column(3, transformation_selectInput("trivariate_transform_z")),
          column(2, actionButton("trivariate_transform_go", label = "Transform",
                                 class = "transform-go"))
        ),
        hr(class = "hroptions"),
        fluidRow(
          column(3, shinyjs::colourInput("trivariate_pt_color", strongMed("Color"), 
                                         value = base_fill)),
          column(3, sliderInput("trivariate_pt_size", strongMed("Size"), 
                                value = 0.5, min = 0, max = 2, step = 0.1, ticks = FALSE)),
          column(2, radioButtons("trivariate_grid", strongMed("Grid"), 
                                 choices = list(Show = "show", Hide = "hide"), 
                                 selected = "show", inline = FALSE)),
          column(2, radioButtons("trivariate_flip", strongMed("y-axis"), 
                                 choices = list(Normal = "normal", Flipped = "flip"), 
                                 selected = "normal", inline = FALSE))
        )
      )
  )
)

