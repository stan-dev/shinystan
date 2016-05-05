# posterior summary statistics for a single parameter
parameter_summary <- reactive({
  validate(need(input$param != "", message = FALSE))
  do.call(
    ".param_summary", 
    args = list(
      param = input$param,
      summary = SUMMARY
    )
  )
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
    paging = FALSE, 
    searching = FALSE, 
    info = FALSE, 
    ordering = FALSE,
    autoWidth = TRUE,
    columnDefs = list(list(sClass="alignRight", targets ="_all")),
    initComplete = htmlwidgets::JS( # change background color of table header
      'function(settings, json) {
      $(this.api().table().header()).css({"background-color": "transparent", "color": "black"});
      }')
  ))
})
