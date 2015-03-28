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



output$ui_triviariate_customize <- renderUI({
  bsCollapse(
    bsCollapsePanel(title = "View Options", id = "trivariate_collapse",
                    fluidRow(
                      column(3, selectInput("trivariate_pt_color", strong("Color"), choices = colors(), selected = "maroon")),
                      column(3, sliderInput("trivariate_pt_size", strong("Size"), value = 0.5, min = 0, max = 2, step = 0.1, ticks = FALSE)),
                      column(2, radioButtons("trivariate_warmup", strong("Warmup"), choices = list(Include = "include", Omit = "omit"), selected = "omit", inline = FALSE)),
                      column(2, radioButtons("trivariate_grid", strong("Grid"), choices = list(Show = "show", Hide = "hide"), selected = "show", inline = FALSE)),
                      column(2, radioButtons("trivariate_flip", strong("y-axis"), choices = list(Normal = "normal", Flipped = "flip"), selected = "normal", inline = FALSE))
                    ),
                    hr(),
                    h5(style = "color: #337ab7;", "Controlling the dynamic 3D scatterplot"),
                    helpText(style = "color: white; font-size: 12px;", "Use your mouse or trackpad to rotate the plot and zoom in or out.")
    )
  )
})
