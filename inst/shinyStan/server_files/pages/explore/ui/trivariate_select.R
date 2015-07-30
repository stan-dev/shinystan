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



output$ui_trivariate_select <- renderUI({
    fluidRow(
      column(3, selectizeInput("trivariate_param_x", label = strong(style = "color: #337ab7;", "x-axis"), choices = .make_param_list(object), selected = input$param, multiple = FALSE)),
      column(3, selectizeInput("trivariate_param_y", label = strong(style = "color: #337ab7;", "y-axis"), choices = .make_param_list(object), selected = .make_param_list(object)[1], multiple = FALSE)),
      column(3, selectizeInput("trivariate_param_z", label = strong(style = "color: #337ab7;", "z-axis"), choices = rev(.make_param_list(object)), multiple = FALSE))
    )
})