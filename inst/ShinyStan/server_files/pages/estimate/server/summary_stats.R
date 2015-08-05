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
  `%>%` <- DT::`%>%`
  validate(need(input$table_digits, "loading"))
  DT::datatable(data = round(table_stats, digits = input$table_digits), 
                colnames = c('mcse' = 'se_mean'),
                options = list(
                  colReorder = list(realtime = TRUE),
                  dom = 'RfClrTtip',
                  colVis = list(exclude = list(0), activate = 'mouseover'),
                  pageLength = 10, 
                  # autoWidth = TRUE,
                  pagingType = "full",
                  processing = TRUE,
                  deferRender = TRUE,
                  scrollY = 400,
                  scrollX = TRUE,
                  scrollCollapse = FALSE,
                  language = list(search = "_INPUT_", searchPlaceholder = "Regex searching"),
                  #       preDrawCallback = DT::JS('function() { 
                  # Shiny.unbindAll(this.api().table().node()); }'), 
                  #       drawCallback = DT::JS('function() { 
                  # Shiny.bindAll(this.api().table().node()); } '),
                  search = list(regex = TRUE),
                  # searchHighlight = TRUE,
                  tableTools = list(sSwfPath = DT::copySWF("www", pdf = TRUE), 
                                    aButtons = list('copy', 'print', list(
                                      sExtends = 'collection',
                                      sButtonText = 'Save',
                                      aButtons = c('csv', 'pdf')
                                    )))
                ), 
                # filter = list(position = 'top', clear = TRUE, plain = FALSE),
                extensions = 
                  c("TableTools", "ColReorder", "ColVis", "FixedColumns", "Scroller")) %>%
    DT::formatStyle(columns = "Rhat", 
                    color = DT::styleInterval(1.1, c("blue", "red"))) %>% 
    DT::formatRound(columns = "n_eff", digits = 0) # %>%
  #     DT::formatRound(columns = c("Rhat", "mean", "mcse", "sd", "2.5%", "25%", 
  #                                 "50%", "75%", "97.5%"), digits = input$table_digits)
})
output$all_summary_out <- DT::renderDataTable({
  summary_stats()
})
