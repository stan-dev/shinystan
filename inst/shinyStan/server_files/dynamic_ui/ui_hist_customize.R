output$ui_hist_customize <- renderUI({
  bsCollapse(
    bsCollapsePanel(title = "View Options", id = "hist_collapse",
                    fluidRow(
                      column(2, numericInput("hist_chain", label = h5("Chain"), min = 0, max = object@nChains, step = 1, value = 0)),
                      column(4, sliderInput("hist_binwd", label = h5("Binwidth (0 = default)"), min = 0, value = 0, max = 50, step = 0.05, ticks = FALSE)),
                      column(3, selectInput("hist_fill_color", label = h5("Fill color"), choices = colors(), selected = "gray35")),
                      column(3, selectInput("hist_line_color", label = h5("Outline color"), choices = colors(), selected = "gray35"))
                    ),
                    hr(),
                    downloadButton("download_histogram", "Save as ggplot2 object")
    )
  )
})
