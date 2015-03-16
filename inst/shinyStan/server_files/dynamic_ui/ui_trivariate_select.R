output$ui_trivariate_select <- renderUI({
  tags$div(
    fluidRow(
      column(3, selectizeInput("trivariate_param_x", label = strong(style = "color: #337ab7;", "x-axis"), choices = .make_param_list(object), multiple = FALSE)),
      column(3, selectizeInput("trivariate_param_y", label = strong(style = "color: #337ab7;", "y-axis"), choices = .make_param_list(object), multiple = FALSE)),
      column(3, selectizeInput("trivariate_param_z", label = strong(style = "color: #337ab7;", "z-axis"), choices = rev(.make_param_list(object)), multiple = FALSE))
    ),
    fluidRow(
      column(3, textInput("trivariate_transform_x", label = "Transform x", value = "x")),
      column(3, textInput("trivariate_transform_y", label = "Transform y", value = "y")),
      column(3, textInput("trivariate_transform_z", label = "Transform z", value = "z"))
    )
  )
})