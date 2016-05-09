fluidRow(
  column(
    width = 3, 
    uiOutput("ui_trivariate_select_x")
  ),
  column(
    width = 3,
    selectizeInput(
      "trivariate_param_y",
      label = strong_bl("y-axis"),
      choices = .param_list,
      selected = if (length(unlist(.param_list)) > 1) 
        unlist(.param_list)[2] else unlist(.param_list)[1],
      multiple = FALSE
    )
  ),
  column(
    width = 3,
    selectizeInput(
      "trivariate_param_z",
      label = strong_bl("z-axis"),
      choices = rev(.param_list),
      multiple = FALSE
    )
  )
)