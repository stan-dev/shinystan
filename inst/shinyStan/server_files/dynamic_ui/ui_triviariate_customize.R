output$ui_triviariate_customize <- renderUI({
  bsCollapse(
    bsCollapsePanel(title = "View Options", id = "trivariate_collapse",
                    fluidRow(
                      column(3, sliderInput("trivariate_pt_size", h5("Point size"), value = 0.5, min = 0, max = 2, step = 0.1, ticks = FALSE)),
                      column(3, radioButtons("trivariate_warmup", h5("Warmup"), choices = list(Include = "include", Omit = "omit"), selected = "omit", inline = FALSE)),
                      column(3, radioButtons("trivariate_grid", h5("Grid"), choices = list(Show = "show", Hide = "hide"), selected = "show", inline = FALSE)),
                      column(3, radioButtons("trivariate_flip", h5("y-axis"), choices = list(Normal = "normal", Flipped = "flip"), selected = "normal", inline = FALSE))
                    ),
                    hr(),
                    h5(style = "color: white;", "Controlling the dynamic 3D scatterplot"),
                    helpText(style = "color: white; font-size: 12px;", "Use your mouse or trackpad to rotate the plot and zoom in or out.")
    )
  )
})
