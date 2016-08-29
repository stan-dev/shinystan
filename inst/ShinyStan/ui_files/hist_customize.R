shinyjs::hidden(div(
  id = "hist_options",
  wellPanel(
    class = "optionswell",
    hr(class = "hroptions"),
    strongBig("Transformation"),
    transform_helpText("x"),
    fluidRow(
      column(width = 4, transformation_selectInput("hist_transform_x")),
      column(
        width = 2,
        actionButton("hist_transform_x_go", label = "Transform", class = "transform-go")
      )
    ),
    hr(class = "hroptions"),
    fluidRow(
      column(
        width = 2,
        numericInput(
          "hist_chain",
          label = strongMed("Chain"),
          min = 0,
          max = .nChains,
          step = 1,
          value = 0
        )
      ),
      column(
        width = 4,
        sliderInput(
          "hist_binwd",
          label =  strongMed("Binwidth (0 = default)"),
          min = 0,
          value = 0,
          max = 50,
          step = 0.05,
          ticks = FALSE
        )
      ),
      column(
        width = 3,
        colourpicker::colourInput("hist_fill_color", strongMed("Fill"), base_fill)
      ),
      column(
        width = 3,
        colourpicker::colourInput("hist_line_color", strongMed("Line"), vline_base_clr)
      )
    )
  )
))
