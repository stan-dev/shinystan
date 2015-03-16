output$ui_rhat_neff_mcse <- renderUI({
  tags$div(
    splitLayout(h4("\\(n_{eff} / N\\)", align = "center"),
                h4("\\(\\text{se}_{mean} / sd\\)", align = "center"),
                h4("\\(\\hat{R}\\)", align = "center")),
    splitLayout(
      plotOutput("n_eff_plot_out", height = "250px"),
      plotOutput("mcse_over_sd_plot_out", height = "250px"),
      plotOutput("rhat_plot_out", height = "250px"),
      cellArgs = list(class = "plot_hover_shadow")
    )
  )
})

output$ui_rhat_neff_mcse_warnings <- renderUI({
  tags$div(
    fluidRow(
      column(4, strong(textOutput("n_eff_warnings_title"))),
      column(4, strong(textOutput("mcse_over_sd_warnings_title"))),
      column(4, strong(textOutput("rhat_warnings_title")))
    ),
    tags$style(type="text/css", "#n_eff_warnings_title, #rhat_warnings_title, #mcse_over_sd_warnings_title {font-size: 13px;}"),
    br(),
    fluidRow(
      column(4, div(style = "color: #337ab7;", textOutput("n_eff_warnings"))),
      column(4, div(style = "color: #337ab7;", textOutput("mcse_over_sd_warnings"))),
      column(4, div(style = "color: #337ab7;", textOutput("rhat_warnings")))
    ),
    tags$style(type="text/css", "#n_eff_warnings, #rhat_warnings, #mcse_over_sd_warnings {font-size: 12px;}")
  )
})
