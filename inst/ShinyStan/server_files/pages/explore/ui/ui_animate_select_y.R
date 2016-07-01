output$ui_animate_select_y <- renderUI({
  selectizeInput(
    "animate_param_y",
    label = strong_bl("y-axis"),
    choices = .make_param_list(object),
    selected = input$param,
    multiple = FALSE
  )
})