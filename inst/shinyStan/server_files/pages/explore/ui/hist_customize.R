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



output$ui_hist_customize <- renderUI({
  bsCollapse(
    bsCollapsePanel(title = "View Options", id = "hist_collapse",
                    fluidRow(
                      column(2, numericInput("hist_chain", label = h5("Chain"), min = 0, max = object@nChains, step = 1, value = 0)),
                      column(4, sliderInput("hist_binwd", label = h5("Binwidth (0 = default)"), min = 0, value = 0, max = 50, step = 0.05, ticks = FALSE)),
                      column(3, selectInput("hist_fill_color", label = h5("Fill color"), choices = colors(), selected = "gray20")),
                      column(3, selectInput("hist_line_color", label = h5("Outline color"), choices = colors(), selected = "gray35"))
                    ),
                    hr(),
                    downloadButton("download_histogram", "Save as ggplot2 object")
    )
  )
})
