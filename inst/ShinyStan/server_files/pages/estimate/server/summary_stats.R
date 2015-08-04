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

# Table of posterior summary statistics -----------------------------------

summary_stats <- reactive({
  do.call(".all_summary", args = list(
    summary     = fit_summary
    # digits      = input$stats_digits,
    # cols        = input$stats_columns
  ))
})

table_digits <- reactive({
  input$stats_digits
})

output$all_summary_out <- DT::renderDataTable({
  `%>%` <- DT::`%>%`
  validate(need(input$table_options_display, "loading"))
  DT::datatable(data = table_stats, colnames = c('mcse' = 'se_mean'),
    options = list(
      #dom = 'Rlfrtip', 
      colReorder = list(realtime = TRUE),
      dom = 'C<"clear">Rlfritp<"bottom">T',
      colVis = list(exclude = list(0), activate = 'mouseover'),
      pageLength = 10, 
      # autoWidth = TRUE,
      pagingType = "full",
      processing = TRUE,
      deferRender = TRUE,
      scrollY = 400,
      scrollX = TRUE,
      scrollCollapse = FALSE,
      preDrawCallback = DT::JS('function() { 
Shiny.unbindAll(this.api().table().node()); }'), 
      drawCallback = DT::JS('function() { 
Shiny.bindAll(this.api().table().node()); } '),
      search = list(regex = input$user_regex),
      tableTools = list(sSwfPath = DT::copySWF("www", pdf = TRUE), 
                        aButtons = list('copy', 'print', list(
                          sExtends = 'collection',
                          sButtonText = 'Save',
                          aButtons = c('csv', 'pdf')
                        )))
    ), filter = 'bottom', extensions = c("TableTools", "ColReorder", "ColVis", "FixedColumns", "Scroller")) 
  # %>% DT::formatRound(digits = table_digits())
})

#   options = list(
#     searchHighlight = TRUE,
#     search = list(regex = input$user_regex), # allow regular expression when searching for parameter names
#     processing = TRUE,
#     pagingType = "full", # show only first, previous, next, last buttons (no page numbers)
#     pageLength = 10,
#     lengthMenu = list(c(5, 10, 20, 50, -1), c('5', '10', '20', '50', 'All')),
#     scrollY = 400,
#     scrollX = TRUE,
#     autoWidth = TRUE,
#     scrollCollapse = FALSE,
#     columnDefs = list(list(width="85px", targets=list(0)), 
#                       list(sClass="alignRight", targets ="_all")),
#     initComplete = htmlwidgets::JS( # change text color of column titles
#       'function(settings, json) {
#       $(this.api().table().header()).css({"color": "#006DCC"});
#       }'),
#     rowCallback = htmlwidgets::JS(
#       'function(row, data) {
#       // Bold cells in the first column
#       $("td:eq(0)", row).css("font-weight", "bold");
#       }')
#   )

