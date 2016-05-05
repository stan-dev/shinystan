output$ui_multiparam_selectize <- renderUI({
  choices <- make_param_list_with_groups_sort()
  selected <- c(input$params_to_plot)
  selectizeInput(
    "params_to_plot",
    label = h5("Select or enter parameter names"),
    width = '100%',
    choices = choices,
    multiple = TRUE
  )
})


# updating the choices and selected for the selectizeInput input$params_to_plot
# when the sorting option is changed in input$param_plot_sort_j or when
# parameters added by regex search

copy_params_to_plot <- reactive({
  copy <- input$params_to_plot
  if (is.null(copy) || !length(copy))
    NULL
  else
    copy
})

observe({
  x <- input$param_plot_sort_j
  choices <- make_param_list_with_groups_sort()
  selected <- copy_params_to_plot()
  selected <- .update_params_with_groups(selected, PARAM_NAMES)
  updateSelectizeInput(
    session,
    inputId = "params_to_plot",
    choices = choices,
    selected = selected
  )
})

observeEvent(input$param_plot_regex, {
  pattern <- input$params_to_plot_regex
  if (pattern != "") {
    choices <- make_param_list_with_groups_sort()
    selected <- copy_params_to_plot()
    selected <- .update_params_with_groups(selected, PARAM_NAMES)
    if (.test_valid_regex(pattern)) {
      selected <- .update_params_with_regex(selected, PARAM_NAMES, pattern)
      updateSelectizeInput(
        session,
        inputId = "params_to_plot",
        choices = choices,
        selected = selected
      )
    }
  }
})

output$invalid_regex <- renderText({
  pattern <- input$params_to_plot_regex
  if (length(pattern)) {
    msg <- "Invalid regular expression.\nYou might need to add the escape character '\\' ."
    validate(need(.test_valid_regex(pattern), message = msg))
  }
})

