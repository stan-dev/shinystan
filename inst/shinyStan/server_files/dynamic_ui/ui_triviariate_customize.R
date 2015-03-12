output$ui_triviariate_customize <- renderUI({
  bsCollapse(
    bsCollapsePanel(title = "View Options", id = "trivariate_collapse",
                    fluidRow(
                      column(3, selectInput("trivariate_pt_color", strong("Color"), choices = colors(), selected = "maroon")),
                      column(3, sliderInput("trivariate_pt_size", strong("Size"), value = 0.5, min = 0, max = 2, step = 0.1, ticks = FALSE)),
                      column(2, radioButtons("trivariate_warmup", strong("Warmup"), choices = list(Include = "include", Omit = "omit"), selected = "omit", inline = FALSE)),
                      column(2, radioButtons("trivariate_grid", strong("Grid"), choices = list(Show = "show", Hide = "hide"), selected = "show", inline = FALSE)),
                      column(2, radioButtons("trivariate_flip", strong("y-axis"), choices = list(Normal = "normal", Flipped = "flip"), selected = "normal", inline = FALSE))
                    ),
                    hr(),
                    h5(style = "color: white;", "Controlling the dynamic 3D scatterplot"),
                    helpText(style = "color: white; font-size: 12px;", "Use your mouse or trackpad to rotate the plot and zoom in or out.")
    )
  )
})
