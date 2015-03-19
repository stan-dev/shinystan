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



# two functions: density_plot, density_plot_multiview

dependence_plot <- reactive({
  if (is.null(input$dependence_lag) | is.null(input$dependence_span)) {
    return()
  }
  lag_text <- input$dependence_lag
  lag <- eval(parse(text = lag_text))

  do.call(".dependence_plot", args = list(
    session         = session, # needed for updating progress bar
    dependence      = dependence,
    lag             = lag,
    samps           = samps_all,
    warmup          = warmup_val,
    model_name      = model_name,
    sampler_params  = sampler_params,
    pars            = dependence$pars_oi,
    f               = input$dependence_span,
    rug             = input$dependence_rug == "show"
  ))
})
