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

absolutePanel(id = "controls_autocorr", 
              class = "draggable_controls",
              fixed = TRUE,
              top = 185, right = 20, width = 200,
              draggable = TRUE,
              shinyjs::hidden(
                div(id = "autocorr_options",
                    wellPanel(
                      class = "optionswell",
                      strongBig("Autocorrelation"),
                      hr(class = "hroptions"),
                      br(),
                      sliderInput("ac_lags", label = NULL, 
                                  post = " lags", min = 0, max = .nIter-.nWarmup-5, 
                                  step = 5, value = min(25, round((.nIter-.nWarmup)/2))),
                      checkboxInput("ac_partial", label = "Partial autocorrelation", 
                                    value = FALSE),
                      checkboxInput("ac_warmup", label = "Include warmup", FALSE),
                      checkboxInput("ac_combine", label = "Combine chains", FALSE),
                      checkboxInput("ac_flip", label = "Flip facets", FALSE),
                      hr(class = "hroptions"),
                      downloadButton("download_autocorr", "ggplot2", class = "plot-download"),
                      downloadButton('save_pdf_autocorr', "pdf", class = "plot-download pdf-download")
                    )
                )
              )
)

