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

output$ui_multiparam_selectize <- renderUI({
  choices <- make_param_list_with_groups_sort()
  selected <- c(input$params_to_plot)
  selectizeInput("params_to_plot",
                 label = h5("Select or enter parameter names"),
                 width = '100%',
                 choices = choices,
                 multiple = TRUE)
})
