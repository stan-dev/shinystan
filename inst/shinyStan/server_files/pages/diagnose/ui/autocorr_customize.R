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
  absolutePanel(id = "controls_autocorr", 
                class = "draggable_controls",
                fixed = TRUE,
                top = 175, right = 20, width = 270,
                draggable = TRUE,
                shinyjs::hidden(
                  div(id = "autocorr_options",
                      wellPanel(
                        class = "optionswell",
                        strongBig(style = "align: right;", "Autocorrelation"),
                        hr(class = "hroptions"),
                        sliderInput("ac_lags", label = strongMed("Lags"), 
                                    post = " lags", min = 0, max = nIter-warmup_val-5, 
                                    step = 5, value = min(25, round((nIter-warmup_val)/2))),
                        hr(class = "hroptions"),
                        strongMed("Options"),
                        checkboxInput("ac_partial", label = "Partial autocorrelation", 
                                      value = FALSE),
                        checkboxInput("ac_warmup", label = "Include warmup", FALSE),
                        checkboxInput("ac_combine", label = "Combine chains", FALSE),
                        checkboxInput("ac_flip", label = "Flip facets", FALSE),
                        hr(class = "hroptions"),
                        br(),
                        downloadButton("download_autocorr", "Save as ggplot2 object")
                      )
                  )
                )
  )
})
