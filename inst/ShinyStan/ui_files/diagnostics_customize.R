div(id = "diagnostics_customize",
    wellPanel(
      fluidRow(
        column(3, h4(textOutput("diagnostic_chain_text"))),
        column(4, h5("Parameter")),
        column(4, h5("Transformation"))
      ),
      fluidRow(
        column(3, div(style = "width: 100px;", 
                      numericInput("diagnostic_chain", label = NULL, value = 0, 
                                   min = 0, max = .nChains))),
        column(4, selectizeInput(
          inputId = "diagnostic_param", label = NULL, multiple = FALSE, 
          choices = .param_list, 
          selected = .param_list[1])),
        column(3, transformation_selectInput("diagnostic_param_transform")),
        column(2, actionButton("diagnostic_param_transform_go", "Transform"))
      ),
      helpText(strong(style = "color: red; font-size: 13px;", 
                      textOutput("diagnostics_warnings_text")))
    )
)
