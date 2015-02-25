# functions for updating the choices and selected for
# the selectizeInput input$params_to_plot when the sorting
# option is changed in input$multiparam_sort

copy_params_to_plot <- reactive({
  copy <- input$params_to_plot
  if (is.null(copy) | length(copy) == 0) {
    return(NULL)
  }
  copy
})

observe({
  x <- input$multiparam_sort
  choices <- make_param_list_with_groups_sort()
  selected <- copy_params_to_plot()
  updateSelectizeInput(session, inputId = "params_to_plot", choices = choices,
                       selected = selected)
})
