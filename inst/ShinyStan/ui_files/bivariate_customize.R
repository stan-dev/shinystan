shinyjs::hidden(
  div(id = "bivariate_options",
      wellPanel(
        class = "optionswell",
        hr(class = "hroptions"),
        strongBig("Transformation"),
        transform_helpText("x,y"),
        fluidRow(
          column(width = 3, transformation_selectInput("bivariate_transform_x")),
          column(width = 3, transformation_selectInput("bivariate_transform_y")),
          column(
            width = 2,
            actionButton("bivariate_transform_go", label = "Transform", class = "transform-go")
          )
        ),
        hr(class = "hroptions"),
        selectInput(
          "bivariate_options_display",
          label = strongBig("Control"),
          choices = c("Points", "Ellipse", "Lines"),
          selected = "Points",
          width = "50%"
        ),
        conditionalPanel(
          condition = "input.bivariate_options_display == 'Points'",
          fluidRow(
            column(
              width = 3,
              colourpicker::colourInput("bivariate_pt_color", strongMed("Color"), base_fill)
            ),
            column(
              width = 2,
              numericInput(
                "bivariate_pt_size",
                strongMed("Size"),
                value = 3.5,
                min = 0,
                max = 10,
                step = 0.5
              )
            ),
            column(
              width = 2,
              numericInput(
                "bivariate_pt_shape",
                strongMed("Shape"),
                value = 10,
                min = 1,
                max = 10,
                step = 1
              )
            ),
            column(
              width = 2,
              sliderInput(
                "bivariate_pt_alpha",
                strongMed("Opacity"),
                value = alpha_calc_pt(.nIter),
                min = 0,
                max = 1,
                step = 0.01,
                ticks = FALSE
              )
            )
          )),
        conditionalPanel(
          condition = "input.bivariate_options_display == 'Ellipse'",
          fluidRow(
            column(
              width = 2,
              selectizeInput(
                inputId = "bivariate_ellipse_lev",
                label = strongMed("Type"),
                selected = "None",
                choices = list(
                  "None" = "None",
                  "50%" = 0.5,
                  "80%" = 0.8,
                  "95%" = 0.95,
                  "99%" = 0.99
                )
              )
            ),
            column(
              width = 3,
              colourpicker::colourInput(
                "bivariate_ellipse_color",
                strongMed("Color"),
                vline_base_clr
              )
            ),
            column(
              width = 2,
              numericInput(
                "bivariate_ellipse_lwd",
                strongMed("Size"),
                value = 1,
                min = 0,
                max = 5,
                step = 0.5
              )
            ),
            column(
              width = 2,
              numericInput(
                "bivariate_ellipse_lty",
                strongMed("Shape"),
                value = 1,
                min = 1,
                max = 6,
                step = 1
              )
            ),
            column(
              width = 2,
              sliderInput(
                "bivariate_ellipse_alpha",
                strongMed("Opacity"),
                value = 1,
                min = 0,
                max = 1,
                step = 0.01,
                ticks = FALSE
              )
            )
          )
        ),
        conditionalPanel(
          condition = "input.bivariate_options_display == 'Lines'",
          fluidRow(
            column(
              width = 2,
              selectizeInput(
                inputId = "bivariate_lines",
                label = strongMed("Position"),
                choices = c(Hide = "hide", Back = "back", Front = "front"),
                selected = "back"
              )
            ),
            column(
              width = 3,
              colourpicker::colourInput("bivariate_lines_color", strongMed("Color"), "gray")
            ),
            column(
              width = 2,
              sliderInput(
                "bivariate_lines_alpha",
                label = strongMed("Opacity"),
                value = alpha_calc_lines(.nIter),
                min = 0,
                max = 1,
                step = 0.01,
                ticks = FALSE
              )
            )
          )
        )
      )
  )
)

