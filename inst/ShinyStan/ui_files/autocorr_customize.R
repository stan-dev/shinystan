absolutePanel(
  id = "controls_autocorr",
  class = "draggable_controls",
  fixed = TRUE,
  top = 185,
  right = 20,
  width = 200,
  draggable = TRUE,
  shinyjs::hidden(div(
    id = "autocorr_options",
    wellPanel(
      class = "optionswell",
      strongBig("Autocorrelation"),
      hr(class = "hroptions"),
      br(),
      sliderInput(
        "ac_lags",
        label = NULL,
        post = " lags",
        min = 0,
        max = .nIter - .nWarmup - 5,
        step = 5,
        value = min(25, round((.nIter - .nWarmup) / 2))
      ),
      checkboxInput("ac_partial", label = "Partial autocorrelation", value = FALSE),
      checkboxInput("ac_warmup", label = "Include warmup", FALSE),
      checkboxInput("ac_combine", label = "Combine chains", FALSE),
      checkboxInput("ac_flip", label = "Flip facets", FALSE),
      hr(class = "hroptions"),
      downloadButton("download_autocorr", "ggplot2", class = "plot-download"),
      downloadButton('save_pdf_autocorr', "pdf", class = "plot-download pdf-download")
    )
  ))
)
