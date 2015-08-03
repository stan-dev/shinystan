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


# posterior summary statistics for a single parameter ---------------------
parameter_summary <- reactive({
  validate(need(input$param != "", message = FALSE))

  do.call(".param_summary", args = list(
    param       = input$param,
    summary     = fit_summary
  ))
})

output$param_name <- renderText({
  input$param
})
output$parameter_summary_out <- DT::renderDataTable({
  DT::datatable({
    as.data.frame(round(parameter_summary(), 2))
  }, 
  rownames = FALSE,
  options = list(
    paging = FALSE, searching = FALSE, info = FALSE, ordering = FALSE,
    autoWidth = TRUE,
    columnDefs = list(list(sClass="alignRight", targets ="_all")),
    initComplete = htmlwidgets::JS( # change background color of table header
      'function(settings, json) {
      $(this.api().table().header()).css({"background-color": "transparent", "color": "black"});
      }')
  ))
})
