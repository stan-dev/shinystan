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


# Table of posterior summary statistics -----------------------------------
summary_stats <- reactive({
  do.call(".all_summary", args = list(
    summary     = fit_summary,
    digits      = input$stats_digits,
    cols        = input$stats_columns
  ))
})

output$all_summary_out <- DT::renderDataTable({
  DT::datatable({
    summary_stats()
  },
  filter = 'bottom',
  options = list(
    searchHighlight = TRUE,
    search = list(regex = input$user_regex), # allow regular expression when searching for parameter names
    processing = TRUE,
    pagingType = "full", # show only first, previous, next, last buttons (no page numbers)
    pageLength = 10,
    lengthMenu = list(c(5, 10, 20, 50, -1), c('5', '10', '20', '50', 'All')),
    scrollY = 400,
    scrollX = TRUE,
    autoWidth = TRUE,
    scrollCollapse = FALSE,
    columnDefs = list(list(width="85px", targets=list(0)), list(sClass="alignRight", targets ="_all")),
    initComplete = htmlwidgets::JS( # change text color of column titles
      'function(settings, json) {
      $(this.api().table().header()).css({"color": "#337ab7"});
      }'),
    rowCallback = htmlwidgets::JS(
      'function(row, data) {
      // Bold cells in the first column
      $("td:eq(0)", row).css("font-weight", "bold");
      }')
  )
  )
})

# download the table
output$download_all_summary <- downloadHandler(
  filename = paste0('shinystan_summary_stats.RData'),
  content = function(file) {
    shinystan_summary_stats <- summary_stats()
    save(shinystan_summary_stats, file = file)
  }
)
# latex the table
observeEvent(input$tex_go, handlerExpr = {
  summary_stats_latex()
})
