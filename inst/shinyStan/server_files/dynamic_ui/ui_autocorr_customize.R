output$ui_autocorr_customize <- renderUI({
  params <- input$ac_params

  if (length(params > 1)) {
    select_collapse <- bsCollapsePanel(title = "Options", id = "ac_options_collapse",
                                       sliderInput("ac_lags", label = "Lags", post = " lags", min = 0, max = nIter-warmup_val-5, step = 5, value = min(25, round((nIter-warmup_val)/2))),
                                       checkboxInput("ac_partial", "Partial autocorrelation", value = FALSE),
                                       checkboxInput("ac_warmup", label = "Include warmup", TRUE),
                                       checkboxInput("ac_combine", label = "Combine chains", FALSE),
                                       conditionalPanel(condition = "input.ac_combine == false", checkboxInput("ac_flip", label = "Flip facets", FALSE))
    )
  } else {
    select_collapse <- bsCollapsePanel(title = "Options", id = "ac_options_collapse",
                                       sliderInput("ac_lags", label = "Lags", post = " lags", min = 0, max = nIter-warmup_val-5, step = 5, value = min(25, round((nIter-warmup_val)/2))),
                                       checkboxInput("ac_partial", "Partial autocorrelation", value = FALSE),
                                       checkboxInput("ac_warmup", label = "Include warmup", FALSE),
                                       checkboxInput("ac_combine", label = "Combine chains", FALSE)

    )
  }

  absolutePanel(id = "controls_autocorr", class = "panel panel-default hvr-glow", fixed = TRUE,
    top = 400, right = 20, width = 270,
    draggable = TRUE,
    div(class = "shinystan_customize", "shinyStan customize"),
    wellPanel(style = "background-color: #222222; padding-top: 10px ; padding-bottom: 10px;",
              bsCollapse(
                select_collapse
              ),
              hr(class = "hr hr_controls"),
              downloadButton("download_autocorr", "Save as ggplot2 object")
    )
  )
})
