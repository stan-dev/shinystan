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



sampler_plot_tests <- reactive({
  validate(
    need(sampler_params[[1]] != "Not Stan", message = "Only available for Stan models"),
    need(stan_algorithm == "NUTS", message = "Only available for algorithm = NUTS")
  )
})
sampler_plot_treedepth <- reactive({
  sampler_plot_tests()
  do.call(".sampler_plot_treedepth", args = list(
    sampler_params  = sampler_params,
    warmup_val      = warmup_val,
    divergent     = "All"
  ))
})
sampler_plot_treedepth0 <- reactive({
  sampler_plot_tests()
  do.call(".sampler_plot_treedepth", args = list(
    sampler_params  = sampler_params,
    warmup_val      = warmup_val,
    divergent     = 0
  ))
})
sampler_plot_treedepth1 <- reactive({
  sampler_plot_tests()
  do.call(".sampler_plot_treedepth", args = list(
    sampler_params  = sampler_params,
    warmup_val      = warmup_val,
    divergent     = 1
  ))
})
sampler_plot_divergent <- reactive({
  sampler_plot_tests()
  do.call(".sampler_plot_divergent", args = list(
    sampler_params  = sampler_params,
    warmup_val      = warmup_val
  ))
})
