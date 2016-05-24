div(id = "diagnostics_customize",
    wellPanel(
      fluidRow(
        column(width = 3, h4(textOutput("diagnostic_chain_text"))),
        column(width = 4, h5("Parameter")),
        column(width = 4, h5("Transformation"))
      ),
      fluidRow(
        column(
          width = 3, div(style = "width: 100px;",
          numericInput(
            "diagnostic_chain",
            label = NULL,
            value = 0,
            min = 0,
            # don't allow changing chains if only 1 chain
            max = ifelse(.nChains == 1, 0, .nChains)
          )
        )),
        column(
          width = 4,
          selectizeInput(
            inputId = "diagnostic_param",
            label = NULL,
            multiple = FALSE,
            choices = .param_list,
            selected = .param_list[1]
          )
        ),
        column(
          width = 3,
          transformation_selectInput("diagnostic_param_transform")
        ),
        column(
          width = 2,
          actionButton("diagnostic_param_transform_go", "Transform", class = "transform-go")
        )
      ),
      helpText(strong(
        style = "color: red; font-size: 13px;",
        textOutput("diagnostics_warnings_text")
      ))
    ))
