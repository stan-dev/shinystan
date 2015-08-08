fluidRow(
  column(3, uiOutput("ui_trivariate_select_x")),
  column(3, selectizeInput("trivariate_param_y", label = strong_bl("y-axis"), 
                           choices = .param_list, selected = .param_list[1L], 
                           multiple = FALSE)),
  column(3, selectizeInput("trivariate_param_z", label = strong_bl("z-axis"), 
                           choices = rev(.param_list), 
                           multiple = FALSE))
)