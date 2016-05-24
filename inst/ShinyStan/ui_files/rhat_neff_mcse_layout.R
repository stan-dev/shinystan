sidebarLayout(
  position = "right",
  sidebarPanel(
    width = 3,
    class = "optionswell",
    strongBig("Definitions"),
    hr(class = "hroptions"),
    div(actionLink("open_quick_mcse", "mcse (se_mean)")),
    div(actionLink("open_quick_neff", "n_eff (ESS)")),
    div(actionLink("open_quick_rhat", "Rhat")),
    br(),
    strongBig("Warnings"),
    hr(class = "hroptions"),
    withMathJax(),
    sliderInput(
      "n_eff_threshold",
      "\\(n_{eff} / N\\) warning threshold",
      ticks = FALSE,
      value = 10,
      min = 0,
      max = 100,
      step = 5,
      post = "%"
    ),
    sliderInput(
      "mcse_threshold",
      "\\(\\text{se}_{mean} / sd\\) warning threshold",
      ticks = FALSE,
      value = 10,
      min = 0,
      max = 100,
      step = 5,
      post = "%"
    ),
    sliderInput(
      "rhat_threshold",
      "\\(\\hat{R}\\) warning threshold",
      ticks = FALSE,
      value = 1.1,
      min = 1,
      max = 1.2,
      step = 0.01
    )
  ),
  mainPanel(
    width = 9,
    withMathJax(),
    br(),
    splitLayout(
      h4("\\(n_{eff} / N\\)", align = "center"),
      h4("\\(mcse / sd\\)", align = "center"),
      h4("\\(\\hat{R}\\)", align = "center")
    ),
    splitLayout(
      plotOutput("n_eff_plot_out", height = "200px"),
      plotOutput("mcse_over_sd_plot_out", height = "200px"),
      plotOutput("rhat_plot_out", height = "200px")
    ),
    hr(),
    div(
      fluidRow(
        column(width = 4, strong(textOutput(
          "n_eff_warnings_title"
        ))),
        column(width = 4, strong(
          textOutput("mcse_over_sd_warnings_title")
        )),
        column(width = 4, strong(textOutput(
          "rhat_warnings_title"
        )))
      ),
      tags$style(
        type = "text/css",
        "#n_eff_warnings_title, #rhat_warnings_title, #mcse_over_sd_warnings_title {font-size: 13px;}"
      ),
      br(),
      fluidRow(
        column(width = 4, div(style = "color: #006dcc;", textOutput("n_eff_warnings"))),
        column(width = 4, div(
          style = "color: #006dcc;", textOutput("mcse_over_sd_warnings")
        )),
        column(width = 4, div(style = "color: #006dcc;", textOutput("rhat_warnings")))
      ),
      tags$style(
        type = "text/css",
        "#n_eff_warnings, #rhat_warnings, #mcse_over_sd_warnings {font-size: 12px;}"
      )
    )
  )
)