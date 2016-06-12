fluidRow(
  column(
    width = 3, 
    uiOutput("ui_animate_select_y")
  ),
  column(
    width = 3,
    selectizeInput(
      "animate_param_x",
      label = strong_bl("x-axis"),
      choices = .param_list,
      selected = if (length(unlist(.param_list)) > 1) 
        unlist(.param_list)[2] else unlist(.param_list)[1],
      multiple = TRUE
    )
  ),
  column(
    width=3,
    actionButton("animate_now","Create Animation")
  )
)