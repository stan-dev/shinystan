output$ui_bivariate_select <- renderUI({
  fluidRow(
    column(4, selectizeInput("bivariate_param_y", label = strong(style = "color: #337ab7;", "y-axis"), choices = rev(.make_param_list(object)), multiple = FALSE)),
    column(3, offset = 2, textInput("bivariate_transform_y", label = "Transform y", value = "y")),
    column(3, textInput("bivariate_transform_x", label = "Transform x", value = "x"))
  )
})