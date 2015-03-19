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



save_settings_param_plot <- reactive({
  list(
    show_density    = input$param_plot_show_density,
    show_ci_line    = input$param_plot_show_ci_line,
    color_by_rhat   = input$param_plot_color_by_rhat,
    fill_color      = input$param_plot_fill_color,
    outline_color   = input$param_plot_outline_color,
    point_est       = input$param_plot_point_est,
    est_color = input$param_plot_est_color,
    rhat_palette = input$param_plot_rhat_palette
  )
})
