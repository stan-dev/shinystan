shinyjs::hidden(
  div(id = "animate_options",
      wellPanel(
        class = "optionswell",
        hr(class='hroptions'),
        strongBig('Animation'),
        fluidRow(
          column(width=2,numericInput("frame_speed",strongMed("Frames per second"),value = 16,min = 1,step = 1),
                 numericInput("frame_tween",strongMed("Smoothing frames"),value=10,min=1,step=1)),
          column(width = 2,selectInput("animate_color",label = strongMed("Color Palette"),choices = row.names(RColorBrewer::brewer.pal.info),selected = "Set1",
              multiple = FALSE),
              selectInput("animation_quality",strongMed("Select video quality"),choices = names(youtube_aspect),selected = "1080p",multiple = FALSE)),
          column(width=2,
                 selectInput("animate_resolution",strongMed("Resolution (pixels)"),choices=c("Automatic",seq(from=10,to=500,by=10)),multiple = FALSE,selected = "Automatic"),
                 checkboxInput("animate_title",strongMed("Frame counter?"),value=FALSE)),
          column(width=2,
                 selectInput("animate_plot",strongMed("Plot Type"),choices=c("Scatterplot","Density"),multiple=FALSE,selected="Scatterplot"))
          ),
        hr(class = "hroptions"),
        strongBig("Transformation"),
        transform_helpText("x,y"),
        fluidRow(
          column(width = 3, transformation_selectInput("animate_transform_y")),
          column(width = 3, transformation_selectInput("animate_transform_x"))
        ),
        hr(class = "hroptions"),
        selectInput(
          "animate_options_display",
          label = strongBig("Control"),
          choices = c("Points", "Ellipse", "Lines"),
          selected = "Points",
          width = "50%"
        ), 
        conditionalPanel(
          condition = "input.animate_options_display == 'Points'",
          fluidRow(
            column(
              width = 2,
              numericInput(
                "animate_pt_size",
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
                "animate_pt_shape",
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
                "animate_pt_alpha",
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
          condition = "input.animate_options_display == 'Ellipse'",
          fluidRow(
            column(
              width = 2,
              selectizeInput(
                inputId = "animate_ellipse_lev",
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
              width = 2,
              numericInput(
                "animate_ellipse_lwd",
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
                "animate_ellipse_lty",
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
                "animate_ellipse_alpha",
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
          condition = "input.animate_options_display == 'Lines'",
          fluidRow(
            column(
              width = 2,
              selectizeInput(
                inputId = "animate_lines",
                label = strongMed("Position"),
                choices = c(Hide = "hide", Back = "back", Front = "front"),
                selected = "back"
              )
            ),
            column(
              width = 2,
              sliderInput(
                "animate_lines_alpha",
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

