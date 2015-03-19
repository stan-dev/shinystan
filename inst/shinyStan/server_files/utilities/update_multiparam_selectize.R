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



# functions for updating the choices and selected for
# the selectizeInput input$params_to_plot when the sorting
# option is changed in input$param_plot_sort_j

copy_params_to_plot <- reactive({
  copy <- input$params_to_plot
  if (is.null(copy) | length(copy) == 0) {
    return(NULL)
  }
  copy
})

observe({
  x <- input$param_plot_sort_j
  choices <- make_param_list_with_groups_sort()
  selected <- copy_params_to_plot()
  updateSelectizeInput(session, inputId = "params_to_plot", choices = choices,
                       selected = selected)
})
