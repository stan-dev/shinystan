output$ui_sampler_stats_customize <- renderUI({
  fluidRow(
    column(3, radioButtons("sampler_warmup", label = h5("Warmup period"),
                           choices = list(Include = "include", Omit = "omit"),
                           inline = TRUE
    )),
    column(5, radioButtons("sampler_report", label = h5("Report average, maximum, or minimum values"),
                           choices = list(Mean = "average", SD = "sd", Maximum = "maximum", Minimum = "minimum"),
                           inline = TRUE
    )),
    column(2, numericInput("sampler_digits", label = h5("Decimals"), value = 4, min = 0, max = 10, step = 1))
  )
})
