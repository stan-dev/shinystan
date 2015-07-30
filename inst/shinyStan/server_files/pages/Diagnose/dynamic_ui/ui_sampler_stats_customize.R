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



output$ui_sampler_stats_customize <- renderUI({
  fluidRow(
    column(3, radioButtons("sampler_warmup", label = h5("Warmup period"),
                           choices = list(Include = "include", Omit = "omit"),
                           inline = TRUE
    )),
    column(5, radioButtons("sampler_report", label = h5("Report average, sd, max, or min"),
                           choices = list(Mean = "average", SD = "sd", Maximum = "maximum", Minimum = "minimum"),
                           inline = TRUE
    )),
    column(2, numericInput("sampler_digits", label = h5("Decimals"), value = 4, min = 0, max = 10, step = 1))
  )
})
