shinyjs::hidden(div(
  id = "trivariate_options",
  wellPanel(
    class = "optionswell",
    hr(class = "hroptions"),
    strongBig("Transformation"),
    transform_helpText("x,y,z"),
    fluidRow(
      column(
        width = 3,
        transformation_selectInput("trivariate_transform_x")
      ),
      column(
        width = 3,
        transformation_selectInput("trivariate_transform_y")
      ),
      column(
        width = 3,
        transformation_selectInput("trivariate_transform_z")
      ),
      column(
        width = 2,
        actionButton(
          "trivariate_transform_go",
          label = "Transform",
          class = "transform-go"
        )
      )
    ),
    hr(class = "hroptions"),
    fluidRow(
      column(
        width = 3,
        colourpicker::colourInput("trivariate_pt_color", strongMed("Color"), value = base_fill)
      ),
      column(
        width = 3,
        sliderInput(
          "trivariate_pt_size",
          strongMed("Size"),
          value = 0.5,
          min = 0,
          max = 2,
          step = 0.1,
          ticks = FALSE
        )
      ),
      column(
        width = 2,
        radioButtons(
          "trivariate_grid",
          strongMed("Grid"),
          choices = list(Show = "show", Hide = "hide"),
          selected = "show",
          inline = FALSE
        )
      ),
      column(
        width = 2,
        radioButtons(
          "trivariate_flip",
          strongMed("y-axis"),
          choices = list(Normal = "normal", Flipped = "flip"),
          selected = "normal",
          inline = FALSE
        )
      )
    )
  )
))
