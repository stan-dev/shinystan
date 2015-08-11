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


fluidRow(
  column(3, radioButtons("sampler_warmup", label = h5("Warmup"),
                         choices = list(Omit = "omit", Include = "include"),
                         inline = TRUE)
  ),
  column(4, radioButtons("sampler_report", label = h5("Statistic"),
                         choices = list(Mean = "average", SD = "sd", 
                                        Max = "maximum", Min = "minimum"),
                         inline = TRUE)
  ),
  column(2, numericInput("sampler_digits", label = h5("Decimals"), value = 4, 
                         min = 0, max = 10, step = 1))
)
