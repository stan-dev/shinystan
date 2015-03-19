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


output$ui_autocorr_customize <- renderUI({
  params <- input$ac_params

  if (length(params > 1)) {
    select_collapse <- bsCollapsePanel(title = "Options", id = "ac_options_collapse",
                                       sliderInput("ac_lags", label = "Lags", post = " lags", min = 0, max = nIter-warmup_val-5, step = 5, value = min(25, round((nIter-warmup_val)/2))),
                                       checkboxInput("ac_partial", "Partial autocorrelation", value = FALSE),
                                       checkboxInput("ac_warmup", label = "Include warmup", TRUE),
                                       checkboxInput("ac_combine", label = "Combine chains", FALSE),
                                       conditionalPanel(condition = "input.ac_combine == false", checkboxInput("ac_flip", label = "Flip facets", FALSE))
    )
  } else {
    select_collapse <- bsCollapsePanel(title = "Options", id = "ac_options_collapse",
                                       sliderInput("ac_lags", label = "Lags", post = " lags", min = 0, max = nIter-warmup_val-5, step = 5, value = min(25, round((nIter-warmup_val)/2))),
                                       checkboxInput("ac_partial", "Partial autocorrelation", value = FALSE),
                                       checkboxInput("ac_warmup", label = "Include warmup", FALSE),
                                       checkboxInput("ac_combine", label = "Combine chains", FALSE)

    )
  }

  absolutePanel(id = "controls_autocorr", class = "panel panel-default hvr-glow", fixed = TRUE,
    top = 400, right = 20, width = 270,
    draggable = TRUE,
    div(class = "shinystan_customize", "shinyStan customize"),
    wellPanel(style = "background-color: #222222; padding-top: 10px ; padding-bottom: 10px;",
              bsCollapse(
                select_collapse
              ),
              hr(class = "hr hr_controls"),
              downloadButton("download_autocorr", "Save as ggplot2 object")
    )
  )
})
