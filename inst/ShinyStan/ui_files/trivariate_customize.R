shinyjs::hidden(
  div(id = "trivariate_options",
      wellPanel(
        class = "optionswell",
        hr(class = "hroptions"),
        strongBig("Transformation"),
        transform_helpText("x,y,z"),
        fluidRow(
          column(3, transformation_selectInput("trivariate_transform_x")),
          column(3, transformation_selectInput("trivariate_transform_y")),
          column(3, transformation_selectInput("trivariate_transform_z")),
          column(2, actionButton("trivariate_transform_go", label = "Transform",
                                 class = "transform-go"))
        ),
        hr(class = "hroptions"),
        fluidRow(
          column(3, shinyjs::colourInput("trivariate_pt_color", strongMed("Color"), 
                                         value = base_fill)),
          column(3, sliderInput("trivariate_pt_size", strongMed("Size"), 
                                value = 0.5, min = 0, max = 2, step = 0.1, ticks = FALSE)),
          column(2, radioButtons("trivariate_grid", strongMed("Grid"), 
                                 choices = list(Show = "show", Hide = "hide"), 
                                 selected = "show", inline = FALSE)),
          column(2, radioButtons("trivariate_flip", strongMed("y-axis"), 
                                 choices = list(Normal = "normal", Flipped = "flip"), 
                                 selected = "normal", inline = FALSE))
        )
      )
  )
)

