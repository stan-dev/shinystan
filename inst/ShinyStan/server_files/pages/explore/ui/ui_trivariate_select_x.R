output$ui_trivariate_select_x <- renderUI({
  selectizeInput(
    "trivariate_param_x",
    label = strong_bl("x-axis"),
    choices = .make_param_list(object),
    selected = input$param,
    multiple = FALSE
  )
})