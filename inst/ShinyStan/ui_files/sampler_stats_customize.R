fluidRow(
  column(
    width = 3,
    radioButtons(
      "sampler_warmup",
      label = h5("Warmup"),
      choices = list(Omit = "omit", Include = "include"),
      inline = TRUE
    )
  ),
  column(
    width = 4,
    radioButtons(
      "sampler_report",
      label = h5("Statistic"),
      choices = list(
        Mean = "average",
        SD = "sd",
        Max = "maximum",
        Min = "minimum"
      ),
      inline = TRUE
    )
  ),
  column(
    width = 2,
    numericInput(
      "sampler_digits",
      label = h5("Decimals"),
      value = 4,
      min = 0,
      max = 10,
      step = 1
    )
  )
)
