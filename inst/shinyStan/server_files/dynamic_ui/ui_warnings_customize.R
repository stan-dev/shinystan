output$ui_warnings_customize <- renderUI({
  
  absolutePanel(id = "controls_warnings", class = "panel panel-default hvr-glow", fixed = TRUE,
                top = 100, right = 20, width = 270,
                draggable = TRUE,
                wellPanel(style = "background-color: #222222 ;",
                          h4(style = "color: #428bca; ",strong("shinyStan customize")),
                          hr(),
                          bsCollapse(
                            bsCollapsePanel(title = "Options",
                                            withMathJax(),
#                                             span("Set thresholds for warnings"),
                                            sliderInput("n_eff_threshold", "\\(n_{eff} / N\\) warning threshold", ticks = FALSE, value = 10, min = 0, max = 100, step = 5, post = "%"),
                                            sliderInput("mcse_threshold", "\\(\\text{se}_{mean} / sd\\) warning threshold", ticks = FALSE, value = 10, min = 0, max = 100, step = 5, post = "%"),
                                            sliderInput("rhat_threshold", "\\(\\hat{R}\\) warning threshold", ticks = FALSE, value = 1.1, min = 1, max = 1.2, step = 0.01)
                                            )
                          )
                )
  )
})
