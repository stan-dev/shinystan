summary_stats <- reactive({
  `%>%` <- DT::`%>%`
  validate(need(input$table_digits, "loading"))
  DT::datatable(data = round(TABLE_STATS, digits = input$table_digits),
                colnames = c('mcse' = 'se_mean'),
                options = list(
                  colReorder = list(realtime = TRUE),
                  # dom = 'RBfClrTtip',
                  dom = "Bflrtip",
                  buttons = list('copy', 'print', list(
                    extend = 'collection',
                    buttons = c('csv', 'pdf'),
                    text = 'Download'
                  ), 
                  list(extend = 'colvis', columns = 1:ncol(TABLE_STATS))),
                  pageLength = 10,
                  pagingType = "full",
                  processing = TRUE,
                  deferRender = TRUE,
                  scrollY = 400,
                  scrollX = TRUE,
                  scrollCollapse = FALSE,
                  language = list(search = "_INPUT_", searchPlaceholder = "Regex searching"),
                  search = list(regex = TRUE)
                ),
                extensions =
                  c("Buttons", "ColReorder", "FixedColumns", "Scroller")) %>%
    DT::formatStyle(columns = "Rhat",
                    color = DT::styleInterval(1.1, c("blue", "red"))) %>%
    DT::formatRound(columns = "n_eff", digits = 0) # %>%
  #     DT::formatRound(columns = c("Rhat", "mean", "mcse", "sd", "2.5%", "25%",
  #                                 "50%", "75%", "97.5%"), digits = input$table_digits)
})
output$all_summary_out <- DT::renderDataTable({
  summary_stats()
})
