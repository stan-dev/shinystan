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
  div(id = "hist_options",
      wellPanel(
        class = "optionswell",
        hr(class = "hroptions"),
        strongBig("Transformation"),
        transform_helpText("x"),
        fluidRow(
          column(4, transformation_selectInput("hist_transform_x")),
          column(2, actionButton("hist_transform_x_go", label = "Transform",
                                 class = "transform-go"))
        ),
        hr(class = "hroptions"),
        fluidRow(
          column(2, numericInput("hist_chain", label = strongMed("Chain"), min = 0, max = .nChains, step = 1, value = 0)),
          column(4, sliderInput("hist_binwd", label =  strongMed("Binwidth (0 = default)"), min = 0, value = 0, max = 50, step = 0.05, ticks = FALSE)),
          column(3, shinyjs::colourInput("hist_fill_color", strongMed("Fill"), base_fill)),
          column(3, shinyjs::colourInput("hist_line_color", strongMed("Line"), vline_base_clr))
        )
      )
  )
)
