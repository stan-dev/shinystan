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



# reactive function to make latex table of summary stats
summary_stats_latex <- reactive({
  
  params <- .update_params_with_groups(input$tex_params, param_names)
  if (length(params) == 0) params <- param_names
  if (length(params) == 1) {
    x <- do.call(".param_summary", args = list(
      param       = params,
      summary     = fit_summary
    ))
  } else {
    x <- do.call(".all_summary", args = list(
      summary     = fit_summary[params, ],
      digits      = input$stats_digits,
      cols        = input$stats_columns
    ))
  }

  
  tab_env <- if (input$tex_long) "longtable" else getOption("xtable.tabular.environment", "tabular")
  
  xtable::print.xtable(xtable::xtable(x), 
                       booktabs = input$tex_booktabs,
                       tabular.environment = tab_env,
                       include.rownames = FALSE
                       )
})

