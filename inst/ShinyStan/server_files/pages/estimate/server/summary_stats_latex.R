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


# reactive function to make latex table of summary stats
summary_stats_latex <- reactive({
  
  params <- unique(.update_params_with_groups(input$tex_params, param_names))
  nParams <- length(params)
  if (nParams == 0) params <- param_names
  if (nParams == 1) {
    x <- do.call(".param_summary", args = list(
      param       = params,
      summary     = fit_summary
    ))
  } else {
    x <- do.call(".tex_summary", args = list(
      summary     = fit_summary[params, ],
      cols        = input$tex_columns
    ))
  }

  pkgs <- input$tex_pkgs
  tab_env <- if ("Longtable" %in% pkgs) 
    "longtable" else getOption("xtable.tabular.environment", "tabular")
  caption <- if (nzchar(input$tex_caption)) input$tex_caption else NULL
  xt <- xtable::xtable(x, caption = caption)
  xtable::digits(xt) <- input$tex_digits
  if ("n_eff" %in% colnames(xt)) 
    xtable::display(xt)[1 + which(colnames(xt) == "n_eff")] <- "d"
  xtable::print.xtable(xt, 
                       booktabs = "Booktabs" %in% pkgs,
                       tabular.environment = tab_env,
                       include.rownames = FALSE)
})

output$summary_stats_latex_out <- renderPrint({
  input$tex_go
  isolate(summary_stats_latex())
})
